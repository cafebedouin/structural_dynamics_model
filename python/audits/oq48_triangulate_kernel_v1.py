#!/usr/bin/env python3
"""
OQ-48 triangulation arm — kernel_v1 (CONFOUNDED, corroboration-only).

kernel_v1 is the 1,106-story PRE-RESET, PRE-DE-LEAK archive (generated 2026-02-26). It is a
confounded third point, NOT a fourth twin:
  * different generation regime (the old template baked guidance into the prompt — the OQ-70
    bait era), so its metric distributions are shaped partly by generation guidance;
  * model provenance is not stamped in the .pl files (cannot confirm "haiku");
  * never pooled into the twin denominator (OQ-26 single-regime scoping).

So it can CORROBORATE but not ADJUDICATE: if kernel_v1 independently validates a break near a
cut, that is EXTERNAL-validity evidence stronger than haiku/flash agreement (the twins share
prompts+seeds; kernel_v1 shares neither, so agreement rules out prompt-structure artifacts too).
A DISAGREEMENT is uninformative — confounded by regime + de-leak + unknown model — and cannot
break the haiku/flash tie or license moving a value.

This reuses oq48_analyze.find_validated_breaks and the in-scope THRESHOLDS, computes kernel_v1's
validated breaks per metric, and reports for each cut whether kernel_v1 corroborates (a validated
break within +/-0.05) and how it lines up with the two haiku-validated cuts. Read-only; writes
triangulation_kernel_v1.json into the audit dir and prints a corroboration table.
"""

import csv
import json
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
import oq48_analyze as A  # noqa: E402

ROOT = A.ROOT
AUDIT_DIR = A.AUDIT_DIR
NEAR = A.NEAR_CUT


def load_metric_arrays(name):
    tsv = AUDIT_DIR / f"rows_{name}.tsv"
    data = {m: [] for m in A.METRICS}
    with open(tsv) as fh:
        for r in csv.DictReader(fh, delimiter="\t"):
            for m in A.METRICS:
                if r[m] != "unknown":
                    data[m].append(float(r[m]))
    return {m: np.asarray(v, float) for m, v in data.items()}


def nearest(breaks, cut):
    return min(breaks, key=lambda b: abs(b["loc"] - cut)) if breaks else None


def main():
    kv1_tsv = AUDIT_DIR / "rows_kernel_v1.tsv"
    if not kv1_tsv.exists():
        print("rows_kernel_v1.tsv missing — run: "
              "python3 python/audits/oq48_threshold_distributions.py kernel_v1")
        return 1

    kv1 = load_metric_arrays("kernel_v1")
    n = len(next(iter(kv1.values())))
    print(f"kernel_v1 loaded: {n} readings (CONFOUNDED corroboration arm).\n")

    # validated breaks per metric on kernel_v1
    breaks = {}
    diag = {}
    for m in A.METRICS:
        v, d = A.find_validated_breaks(kv1[m])
        breaks[m] = v
        diag[m] = d
        print(f"  {m:5s} n={d['n']} dip_p={d['dip_p']:.4f} multimodal={d['multimodal']} "
              f"validated={[round(x['loc'],4) for x in v]}")

    # corroboration table vs the in-scope cuts (and the haiku-validated locations from the
    # main run, for the two cuts that haiku corroborated)
    haiku_validated = {  # from threshold_evidence.json (main run); the two haiku-corroborated cuts
        "snare_chi_floor": 0.6662,
        "snare_epsilon_floor": 0.4839,
    }
    rows = []
    print(f"\n{'threshold':<32}{'metric':<6}{'cut':>6}  {'kv1 break':>10}{'dist':>7}  corroborates?")
    for label, mkey, cut in A.THRESHOLDS:
        nb = nearest(breaks[mkey], cut)
        loc = round(nb["loc"], 4) if nb else None
        dist = round(abs(nb["loc"] - cut), 4) if nb else None
        corrob = bool(nb and abs(nb["loc"] - cut) <= NEAR)
        rows.append({
            "label": label, "metric": mkey, "current_value": cut,
            "kernel_v1_nearest_break": loc, "dist_to_cut": dist,
            "kernel_v1_corroborates": corrob,
            "haiku_validated_loc": haiku_validated.get(label),
        })
        print(f"{label:<32}{mkey:<6}{cut:>6}  "
              f"{(str(loc) if loc is not None else '-'):>10}{(str(dist) if dist is not None else '-'):>7}  "
              f"{'YES' if corrob else 'no'}")

    out = {
        "arm": "kernel_v1 (pre-reset, pre-de-leak archive; CONFOUNDED, corroboration-only)",
        "n_readings": n,
        "caveat": ("template guidance + unknown model + regime confound; never pooled into the "
                   "twin denominator (OQ-26). Corroborates but cannot adjudicate."),
        "per_metric_diagnostics": diag,
        "corroboration_table": rows,
    }
    (AUDIT_DIR / "triangulation_kernel_v1.json").write_text(json.dumps(out, indent=2))
    print(f"\nwrote {(AUDIT_DIR / 'triangulation_kernel_v1.json').relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
