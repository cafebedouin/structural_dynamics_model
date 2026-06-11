#!/usr/bin/env python3
"""Grid first-contact gate (OQ-93 flip ruling, operator 2026-06-11).

The N=10 variant-prompt batch certified the VARIANT prompt; the live prompt
had zero generated grids under it at flip time. Ruling: flip now, with the
one-time κ plausibility gate converted to FIRST-CONTACT — every
grid-authoring story gets the three-indicator audit ONCE, before any
consumer read, recorded in the tracked ledger
(`python/grid_audit_ledger.json`). Per-story exclusion is fail-closed
(exit 1 naming the story); **C-echo in any new story HALTS and the live
flip REVERTS** (a prompt witnessed teaching counterfeits has no honest
fraction — operator ruling, PREREGISTRATION.md κ section).

Indicators (pinned 2026-06-11, same operationalization as the batch audit):
  C-range  — slot count != 32 or value outside [0,1] (schema/compiler make
             this unreachable; firing = battery failure, halt).
  C-echo   — all 32 values one constant, OR value-tuple identical to ANY
             ledgered story's tuple hash (the live form of "the prompt
             taught a convention"; the ledger accumulates the comparison
             set as the corpus grows).
  C-flat   — every (metric, time) slot-group spans < 0.05 across levels.
  C-dir    — engine G_sys (|G| > 0.01) opposite in sign to EVERY authored
             scalar series with |delta| >= 0.1 (mixed signs exempt).

Wired into run_pipeline.py beside the ISSUES and load-warning gates — a
checker that isn't run isn't checking. Steady-state cost is zero: only
not-yet-ledgered grid stories are audited (opt-in prevalence is low).

Usage:
    python3 python/grid_first_contact_gate.py            # gate (exit 1 on fail)
    python3 python/grid_first_contact_gate.py --json-dir DIR --testsets-dir DIR
                                                          # (positive controls)
"""

import argparse
import hashlib
import json
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
JSON_DIR = ROOT / "json"
TESTSETS_DIR = ROOT / "prolog" / "testsets"
LEDGER_PATH = ROOT / "python" / "grid_audit_ledger.json"

GRID_METRICS = ["accessibility_collapse", "stakes_inflation", "suppression", "resistance"]
LEVELS = ["structural", "organizational", "class", "individual"]


def value_hash(grid):
    vals = {(p["metric"], p["level"], p["time_point"]): p["value"]
            for p in grid.get("points") or []}
    tup = tuple(v for _, v in sorted(vals.items()))
    return hashlib.sha256(repr(tup).encode()).hexdigest()[:16], vals


def engine_gsys(cid, testsets_dir):
    pl = Path(testsets_dir) / f"{cid}.pl"
    if not pl.exists():
        return None, f"no compiled testset at {pl} (json with grid but no .pl — unauditable)"
    goal = (
        "use_module(narrative_ontology), use_module(config), "
        "use_module(pattern_analysis), "
        f"consult('{pl.resolve()}'), "
        f"pattern_analysis:analyze_interval({cid}, G, _, P), "
        "format('RES G=~w P=~w~n', [G, P]), halt."
    )
    p = subprocess.run(["swipl", "-g", goal, "-t", "halt(1)"],
                       cwd=ROOT / "prolog", capture_output=True, text=True,
                       timeout=180)
    out = [l for l in p.stdout.splitlines() if l.startswith("RES ")]
    if not out:
        return None, f"engine error: {p.stderr.strip()[-150:]}"
    g = dict(kv.split("=", 1) for kv in out[-1][4:].split(" "))["G"]
    return (None if g == "open" else float(g)), None


def audit_story(doc, cid, ledger_hashes, testsets_dir):
    """Return (fired:list, vhash:str). Caller decides halt semantics."""
    grid = doc["coercion_grid"]
    fired = []
    vhash, vals = value_hash(grid)

    n_slots = len(vals)
    pts = grid.get("points") or []
    # C-range (corrected 2026-06-12): only the genuinely schema/compiler-
    # unreachable shapes — a value outside [0,1] or duplicate slots. The
    # original first-contact form also fired on slot count != 32, welding
    # the BATCH addendum's full-grid mandate into the standing gate: partial
    # grids are operator-CONFIRMED legal (no fraction threshold; consumer-
    # named-levels decides sufficiency, and the coverage read reports OPEN
    # where insufficient). First misfire: institutional_trust_erosion,
    # 12/32 all-valid points, excluded and the pipeline halted while the
    # story was an OQ-90 flip target (witness:
    # audits/2026-06-12_gate_partial_fix/).
    if len(pts) != n_slots or any(
            not (0.0 <= p["value"] <= 1.0) for p in pts):
        fired.append("C-range")

    tup_vals = list(vals.values())
    if len(set(tup_vals)) == 1 and tup_vals:
        fired.append("C-echo:constant")
    if vhash in ledger_hashes:
        fired.append(f"C-echo:tuple-collision-with:{ledger_hashes[vhash]}")

    # C-flat over the slot-groups actually present: a group is evaluable
    # when >= 2 levels carry values at that (metric, time); fire only if
    # evaluable groups exist and ALL of them span < 0.05. A grid with no
    # evaluable group (single-level authoring) is flat-UNEVALUABLE — the
    # coverage read already makes such grids OPEN for system claims.
    spans = []
    for m in GRID_METRICS:
        for t in (grid["t0"], grid["tn"]):
            group = [vals[(m, lv, t)] for lv in LEVELS if (m, lv, t) in vals]
            if len(group) >= 2:
                spans.append(max(group) - min(group))
    if spans and all(s < 0.05 for s in spans):
        fired.append("C-flat")

    g, err = engine_gsys(cid, testsets_dir)
    if err:
        fired.append(f"C-dir:ENGINE-UNREADABLE({err[:80]})")
    elif g is not None and abs(g) > 0.01:
        series = {}
        for m in doc.get("measurements") or []:
            series.setdefault(m["metric"], []).append((m["time_point"], m["value"]))
        deltas = []
        for _, pts2 in series.items():
            pts2.sort()
            d = pts2[-1][1] - pts2[0][1]
            if abs(d) >= 0.1:
                deltas.append(d)
        if deltas:
            signs = {d > 0 for d in deltas}
            if len(signs) == 1 and (next(iter(signs)) != (g > 0)):
                fired.append("C-dir")
    return fired, vhash


def run_gate(json_dir=JSON_DIR, testsets_dir=TESTSETS_DIR,
             ledger_path=LEDGER_PATH, write_ledger=True):
    ledger = json.loads(ledger_path.read_text()) if Path(ledger_path).exists() \
        else {"entries": []}
    audited = {e["cid"] for e in ledger["entries"]}
    ledger_hashes = {e["value_hash"]: e["cid"] for e in ledger["entries"]
                     if e.get("value_hash")}

    new, problems, echo = [], [], []
    for f in sorted(Path(json_dir).glob("*.json")):
        try:
            doc = json.loads(f.read_text())
        except json.JSONDecodeError:
            continue
        if not doc.get("coercion_grid"):
            continue
        cid = doc.get("header", {}).get("constraint_id", f.stem)
        if cid in audited:
            continue
        fired, vhash = audit_story(doc, cid, ledger_hashes, testsets_dir)
        if any(x.startswith("C-echo") for x in fired):
            echo.append((cid, fired))
        n_pts = len(doc["coercion_grid"].get("points") or [])
        if fired:
            problems.append((cid, fired))
        else:
            entry = {"cid": cid, "audited_at":
                     datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
                     "verdict": "pass", "value_hash": vhash,
                     "coverage": f"{n_pts}/32"}
            if n_pts < 32:
                # Legal (consumer-named-levels) but the live prompt mandates
                # the full grid when opting in — surfaced, never excluded.
                entry["note"] = "partial grid (prompt mandates full 32 on opt-in)"
                print(f"[GRID-GATE] note: {cid} authored a PARTIAL grid "
                      f"({n_pts}/32) — legal (coverage read fails closed "
                      f"downstream), but the live prompt mandates the full "
                      f"grid on opt-in; prompt-compliance signal, not an "
                      f"exclusion.")
            new.append(entry)
            ledger_hashes[vhash] = cid  # within-run cross-story echo check

    for cid, fired in problems:
        print(f"[GRID-GATE] EXCLUDED (fail-closed): {cid} fired={fired}",
              file=sys.stderr)
    if echo:
        print(f"[GRID-GATE] C-ECHO HALT: {[c for c, _ in echo]} — the live "
              f"prompt is witnessed teaching a value convention. REVERT the "
              f"opt-in flip (prompts/constraint_story_generation_prompt_json.md "
              f"grid section) before generating further grid stories; there is "
              f"no honest fraction of this (operator ruling 2026-06-11).",
              file=sys.stderr)
    if new and write_ledger and not problems:
        ledger["entries"].extend(new)
        Path(ledger_path).write_text(json.dumps(ledger, indent=2) + "\n")
    n_grid = len(audited) + len(new) + len(problems)
    print(f"[GRID-GATE] {n_grid} grid stories: {len(audited)} previously "
          f"ledgered, {len(new)} newly passed"
          + (f", {len(problems)} EXCLUDED" if problems else "") + ".")
    return 1 if problems else 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--json-dir", default=str(JSON_DIR))
    ap.add_argument("--testsets-dir", default=str(TESTSETS_DIR))
    ap.add_argument("--ledger", default=str(LEDGER_PATH))
    ap.add_argument("--no-write", action="store_true",
                    help="audit without appending to the ledger")
    args = ap.parse_args()
    sys.exit(run_gate(Path(args.json_dir), Path(args.testsets_dir),
                      Path(args.ledger), write_ledger=not args.no_write))


if __name__ == "__main__":
    main()
