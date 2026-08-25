#!/usr/bin/env python3
"""|Δε| between same-seed stories across legs, against the engine's ε thresholds (2026-08-22).
Exact-agreement (paired_agreement.py) is harsh for a continuous field; this asks how FAR ε moves.
Usage: python3 audits/2026-08-21_flash_regime_vs_redraw/epsilon_distance.py legA legB [legA legB ...]"""
import json, re, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
def load(leg): return {r["id"]: r for r in json.load(open(ROOT/"outputs"/f"pipeline_output.{leg}.json"))["per_constraint"]}
def eps_pairs(A, B, ids):
    """|Δε| over pairs where BOTH sides carry a numeric ε. Absent (None) is excluded and
    COUNTED, never coerced to 0 (OQ-60: absent ≠ zero; the old `or 0` fabricated a 0.0).
    Returns (sorted diffs, n_excluded)."""
    d, excl = [], 0
    for i in ids:
        ea, eb = A[i].get("base_extractiveness"), B[i].get("base_extractiveness")
        if ea is None or eb is None: excl += 1; continue
        d.append(abs(ea - eb))
    return sorted(d), excl
if "--selftest" in sys.argv:
    A = {"x": {"base_extractiveness": None}, "y": {"base_extractiveness": 0.0}}
    B = {"x": {"base_extractiveness": 0.5}, "y": {"base_extractiveness": 0.0}}
    d, excl = eps_pairs(A, B, ["x", "y"])
    assert excl == 1, f"null side must be excluded-and-counted, got excl={excl}"
    assert d == [0.0], f"genuine 0.0 must be KEPT, got {d}"
    print("selftest OK: null excluded-and-counted (1), 0.0 retained ([0.0])"); sys.exit(0)
cfg = (ROOT/"prolog"/"config.pl").read_text()
th = {k: float(v) for k, v in re.findall(r"param\((\w*epsilon\w*),\s*([0-9.]+)\)", cfg)}
print("ε thresholds:", th)
args = sys.argv[1:] or ["flash2","flash3","flash_think","flash_think2","flash2","flash_think","stealth","nemotron"]
for a, b in zip(args[::2], args[1::2]):
    A, B = load(a), load(b); ids = [i for i in A if i in B]
    d, excl = eps_pairs(A, B, ids); n = len(d)
    if not n: print(f"{a} vs {b:14} n=0 (all {excl} shared ids excluded: ε absent on a side)"); continue
    q = lambda p: d[int(p*(n-1))]
    print(f"{a} vs {b:14} n={n:4} |Δε| median={q(.5):.2f} p75={q(.75):.2f} p90={q(.9):.2f} max={d[-1]:.2f} | ≥0.10: {sum(x>=0.10 for x in d)/n:.0%}  ≥0.20: {sum(x>=0.20 for x in d)/n:.0%}"
          + (f" | EXCLUDED (ε absent): {excl}" if excl else ""))
