#!/usr/bin/env python3
"""OQ-215 arm 3 — five-run variance driver (pre-registered: PROPOSAL.md).

Runs the full narrative pipeline five times, SERIALLY (one pipeline at a
time — the shared-corpus rule), over originals/the-empty-pan.md with
--skip-engine (matching arm 1 and the baseline). Between runs it enforces
the pre-registered mechanical kill conditions:

  K1: pipeline error -> STOP.
  K2: M2 bare-5 — the final stage_10 D9 entry lacks either witness
      subsection label -> STOP (the D9 compose didn't take; that finding
      outranks any variance number).

Per run it collects the three pre-registered metrics (M1 numeric register,
M2 D9 witness subsections, M3 invariant survival) into RESULTS.tsv and a
per-run block in RESULTS.md. The driver collects; the operator read
verdicts. Foam-class substrate and refutation quality are NOT mechanized.
"""
import json
import re
import subprocess
import sys
import time
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "agent"))
import uke_narrative_orchestrator as uko  # noqa: E402

AUDIT = REPO / "audits" / "2026-07-12_oq215_arm3_variance"
UKE = REPO / "agent" / "narrative_transform" / "uke"
SOURCE = REPO / "agent" / "narrative_transform" / "originals" / "the-empty-pan.md"
N_RUNS = 5
RUN_TIMEOUT_S = 4200

D9_LABELS = ("STRONGEST CANDIDATE (own):", "STAGE-9 FINDING ADJUDICATION:")


def d9_entry(stage_10_text: str) -> str:
    """Slice the D9 block (from the D9 header to the next D-less header)."""
    m = re.search(r'^.{0,10}D9[:\s].*$', stage_10_text, flags=re.MULTILINE)
    if not m:
        return ""
    tail = stage_10_text[m.start():]
    nxt = re.search(r'^\s{0,3}(?:#{1,6}\s+|\*\*)?(?:TOTAL|FRACTURES|ROUTE|AUTOMATIC)\b',
                    tail[1:], flags=re.MULTILINE)
    return tail[:nxt.start() + 1] if nxt else tail


def d9_bare(stage_10_text: str) -> bool:
    """K2: a D9 score recorded without both witness subsections."""
    entry = d9_entry(stage_10_text)
    if not entry:
        return True  # no D9 entry at all is also invalid
    return not all(lbl in entry for lbl in D9_LABELS)


def stage9_verdict(stage_9_text: str) -> str:
    f = uko._extract_stage9_falsifier(stage_9_text)
    for v in ("HOLDS", "LOST", "UNVERIFIED"):
        if re.search(rf'\b{v}\b', f):
            return v
    return "UNPARSED"


def collect(run_dir: Path) -> dict:
    row: dict = {"run_dir": run_dir.name}

    s3 = (run_dir / "stage_3_output.md")
    s3t = s3.read_text(encoding="utf-8") if s3.exists() else ""
    row["m1_numeric_register"] = (
        "complete" if "<numeric_register>" in s3t and "</numeric_register>" in s3t
        else ("open-only" if "<numeric_register>" in s3t else "ABSENT"))

    for stage in ("stage_4", "stage_8"):
        p = run_dir / f"numeric_inventory_{stage}.json"
        row[f"m1_density_{stage}"] = (
            json.loads(p.read_text())["density_per_1000"] if p.exists() else None)
    row["m1_density_open_flag"] = (run_dir / "NUMERIC_DENSITY_OPEN.md").exists()

    s10 = (run_dir / "stage_10_output.md")
    s10t = s10.read_text(encoding="utf-8") if s10.exists() else ""
    row["m2_d9_present"] = bool(d9_entry(s10t))
    row["m2_bare"] = d9_bare(s10t) if s10t else None  # None = stage 10 never ran
    dm = re.search(r'D9[^\n]*?(\d(?:/5)?)', d9_entry(s10t) or "")
    row["m2_d9_score_line"] = dm.group(0).strip()[:60] if dm else ""

    s9 = (run_dir / "stage_9_output.md")
    row["m3_stage9_falsifier"] = (
        stage9_verdict(s9.read_text(encoding="utf-8")) if s9.exists() else "NO-STAGE9")
    c0 = (run_dir / "invariant_contract_stage0_output.md")
    row["m3_floor_authored"] = (
        "yes" if c0.exists() and re.search(
            r'missing_floor\s+present="yes"', c0.read_text(encoding="utf-8"))
        else ("no" if c0.exists() else "NO-CONTRACT"))
    c2 = (run_dir / "invariant_contract_output.md")
    row["m3_stage2_contract"] = "present" if c2.exists() else "ABSENT"
    return row


def main() -> int:
    AUDIT.mkdir(parents=True, exist_ok=True)
    results: list[dict] = []
    tsv = AUDIT / "RESULTS.tsv"
    md = AUDIT / "RESULTS.md"

    for i in range(1, N_RUNS + 1):
        before = {p.name for p in UKE.glob("the_empty_pan_*")}
        print(f"[arm3] run {i}/{N_RUNS} starting", flush=True)
        t0 = time.time()
        proc = subprocess.run(
            [sys.executable, str(REPO / "agent" / "uke_narrative_orchestrator.py"),
             "--skip-engine", str(SOURCE)],
            cwd=str(REPO), capture_output=True, text=True, timeout=RUN_TIMEOUT_S,
        )
        new = [p for p in UKE.glob("the_empty_pan_*") if p.name not in before]
        run_dir = max(new, key=lambda p: p.stat().st_mtime) if new else None
        (AUDIT / f"run{i}_driver.log").write_text(
            proc.stdout[-20000:] + "\n=== STDERR ===\n" + proc.stderr[-20000:],
            encoding="utf-8")

        if proc.returncode != 0 or run_dir is None:
            print(f"[arm3] K1 STOP: run {i} failed (rc={proc.returncode}, "
                  f"dir={run_dir}) — see run{i}_driver.log", flush=True)
            break

        row = collect(run_dir)
        row["run"] = i
        row["duration_s"] = round(time.time() - t0)
        results.append(row)
        print(f"[arm3] run {i} done in {row['duration_s']}s: "
              f"reg={row['m1_numeric_register']} "
              f"d4={row['m1_density_stage_4']} d8={row['m1_density_stage_8']} "
              f"bare={row['m2_bare']} s9={row['m3_stage9_falsifier']} "
              f"floor={row['m3_floor_authored']}", flush=True)

        if row["m2_bare"]:
            print(f"[arm3] K2 STOP: run {i} produced a bare D9 (witness "
                  f"subsections missing) — the compose didn't take. "
                  f"Arm halted per pre-registration.", flush=True)
            break

    if results:
        keys = ["run", "run_dir", "duration_s", "m1_numeric_register",
                "m1_density_stage_4", "m1_density_stage_8",
                "m1_density_open_flag", "m2_d9_present", "m2_bare",
                "m2_d9_score_line", "m3_stage9_falsifier",
                "m3_floor_authored", "m3_stage2_contract"]
        lines = ["\t".join(keys)]
        for r in results:
            lines.append("\t".join(str(r.get(k, "")) for k in keys))
        tsv.write_text("\n".join(lines) + "\n", encoding="utf-8")
        md.write_text(
            "# OQ-215 arm 3 — driver-collected results (operator read pending)\n\n"
            + "```\n" + "\n".join(lines) + "\n```\n", encoding="utf-8")
        print(f"[arm3] wrote {tsv}", flush=True)

    done = len(results) == N_RUNS and not any(r["m2_bare"] for r in results)
    print(f"[arm3] {'COMPLETE' if done else 'HALTED'} — "
          f"{len(results)}/{N_RUNS} runs recorded", flush=True)
    return 0 if done else 2


if __name__ == "__main__":
    sys.exit(main())
