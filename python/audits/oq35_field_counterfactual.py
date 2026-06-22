#!/usr/bin/env python3
"""OQ-35 rows 2-3 counterfactual driver.

Runs prolog/probe_oq35_field_counterfactual.pl once per corpus (one swipl
process per corpus — controls are per-process), then computes the observation-
tuple diffs and the committed verdict + kill condition.

Verdict logic (the canonical falsifier, single statement):
  - A 0-diff treatment votes COSMETIC in a corpus ONLY IF presence>0 for BOTH
    fields AND the positive control diff is non-empty there.
  - A 0-diff with presence==0 is recorded "field absent here", NOT cosmetic.
  - A non-empty treatment diff implies LOAD-BEARING ONLY with a passing (0-diff)
    null control (observable byte-identical under identical inputs).
  - If treatment is 0-diff but the positive control is also 0-diff (in a
    presence>0 corpus), the probe is BROKEN — abort, do not conclude cosmetic.

Output: per-corpus raw files under
  audits/2026-06-21_oq35_field_counterfactual/<corpus>/
and a printed SUMMARY table (also the writeup's evidence).
"""
import subprocess
import sys
from pathlib import Path

PROLOG_DIR = Path(__file__).resolve().parents[2] / "prolog"
AUDIT_DIR = (Path(__file__).resolve().parents[2]
             / "audits" / "2026-06-21_oq35_field_counterfactual")

# (label, corpus_path relative to prolog/)
CORPORA = [
    ("testsets", "testsets"),
    ("testsets_haiku", "testsets_haiku"),
    ("testsets_flash", "testsets_flash"),
    ("kernel_v1", "archives/datasets/kernel_v1"),
    ("original_v6", "archives/datasets/original_v6"),
]


def run_corpus(label, corpus_path):
    out = AUDIT_DIR / label
    out.mkdir(parents=True, exist_ok=True)
    goal = (
        f"asserta(config:param(corpus_path,'{corpus_path}')), "
        f"[stack], corpus_loader:load_all_testsets, "
        f"[probe_oq35_field_counterfactual], "
        f"run_oq35_probe('{out}'), halt"
    )
    cmd = ["swipl", "-g", goal, "-t", "halt(1)"]
    proc = subprocess.run(cmd, cwd=str(PROLOG_DIR),
                          capture_output=True, text=True, timeout=3600)
    (out / "swipl_stdout.txt").write_text(proc.stdout)
    (out / "swipl_stderr.txt").write_text(proc.stderr)
    # scrape the summary line
    summary = {}
    for line in (proc.stdout + proc.stderr).splitlines():
        if line.startswith("OQ35_SUMMARY"):
            for tok in line.split()[1:]:
                k, _, v = tok.partition("=")
                summary[k] = int(v)
    summary["returncode"] = proc.returncode
    return out, summary, proc


def diff_lines(a, b):
    """Return list of (id, base_obs, other_obs) where the obs differ.
    Keyed by constraint id (first TAB field)."""
    def load(p):
        d = {}
        if not p.exists():
            return d
        for line in p.read_text().splitlines():
            if "\t" in line:
                cid, obs = line.split("\t", 1)
                d[cid] = obs
        return d
    da, db = load(a), load(b)
    keys = sorted(set(da) | set(db))
    diffs = []
    for k in keys:
        if da.get(k) != db.get(k):
            diffs.append((k, da.get(k), db.get(k)))
    return diffs


def main():
    AUDIT_DIR.mkdir(parents=True, exist_ok=True)
    rows = []
    for label, cpath in CORPORA:
        print(f"=== {label} ({cpath}) ===", flush=True)
        out, summary, proc = run_corpus(label, cpath)
        if summary.get("returncode") != 0 and "denominator" not in summary:
            print(f"  ABORT: swipl rc={summary.get('returncode')}; see {out}/swipl_stderr.txt")
            print(proc.stderr[-2000:])
            rows.append((label, summary, None))
            continue
        base = out / "baseline.txt"
        treat_d = diff_lines(base, out / "treatment.txt")
        pcv_d = diff_lines(base, out / "pc_victim.txt")
        pcc_d = diff_lines(base, out / "pc_claim.txt")
        null_d = diff_lines(base, out / "null.txt")

        n = summary.get("denominator", 0)
        nacc = summary.get("presence_accessibility_collapse", 0)
        nres = summary.get("presence_resistance", 0)
        pc_pass = (len(pcv_d) > 0) or (len(pcc_d) > 0)
        null_clean = (len(null_d) == 0)
        presence_ok = (nacc > 0 and nres > 0)

        # verdict per the canonical falsifier
        if not null_clean:
            verdict = "PROBE-NONDETERMINISTIC (null control non-empty) — normalize observable"
        elif len(treat_d) > 0:
            verdict = "LOAD-BEARING (treatment diff non-empty, null control clean)"
        elif not presence_ok:
            verdict = "FIELD ABSENT HERE (presence==0; cannot witness cosmetic)"
        elif not pc_pass:
            verdict = "PROBE BROKEN (treatment 0-diff AND positive control 0-diff)"
        else:
            verdict = "COSMETIC (treatment 0-diff, presence>0, positive control non-empty)"

        # persist per-corpus diff detail
        detail = [f"corpus={label} path={cpath}",
                  f"denominator(N)={n} (corpus_constraint/1)",
                  f"presence accessibility_collapse={nacc} resistance={nres}",
                  f"treatment diff (full tuple)={len(treat_d)}",
                  f"positive control victim diff={len(pcv_d)}",
                  f"positive control claim  diff={len(pcc_d)}",
                  f"null control diff={len(null_d)} (must be 0)",
                  f"VERDICT: {verdict}", ""]
        if treat_d:
            detail.append("--- treatment diffs (id | baseline -> treatment) ---")
            for cid, bo, to in treat_d[:50]:
                detail.append(f"  {cid}\n    base : {bo}\n    treat: {to}")
            if len(treat_d) > 50:
                detail.append(f"  ... (+{len(treat_d)-50} more)")
        if null_d:
            detail.append("--- NULL CONTROL diffs (should be empty!) ---")
            for cid, bo, to in null_d[:20]:
                detail.append(f"  {cid}\n    base: {bo}\n    null: {to}")
        (out / "diff_report.txt").write_text("\n".join(detail) + "\n")

        rows.append((label, {"n": n, "nacc": nacc, "nres": nres,
                             "treat": len(treat_d), "pcv": len(pcv_d),
                             "pcc": len(pcc_d), "null": len(null_d),
                             "verdict": verdict}, out))
        print("\n".join("  " + d for d in detail[:8]), flush=True)

    # master summary
    lines = ["# OQ-35 rows 2-3 counterfactual — master summary", ""]
    hdr = f"{'corpus':16} {'N':>5} {'acc':>5} {'res':>5} {'treat':>6} {'pcV':>5} {'pcC':>5} {'null':>5}  verdict"
    lines.append(hdr)
    lines.append("-" * len(hdr))
    for label, s, _ in rows:
        if s is None or "n" not in s:
            lines.append(f"{label:16} ABORTED")
            continue
        lines.append(f"{label:16} {s['n']:>5} {s['nacc']:>5} {s['nres']:>5} "
                     f"{s['treat']:>6} {s['pcv']:>5} {s['pcc']:>5} {s['null']:>5}  {s['verdict']}")
    master = "\n".join(lines) + "\n"
    (AUDIT_DIR / "master_summary.txt").write_text(master)
    print("\n" + master)


if __name__ == "__main__":
    sys.exit(main())
