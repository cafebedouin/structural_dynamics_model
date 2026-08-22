#!/usr/bin/env python3
"""|Δε| between same-seed stories across legs, against the engine's ε thresholds (2026-08-22).
Exact-agreement (paired_agreement.py) is harsh for a continuous field; this asks how FAR ε moves.
Usage: python3 audits/2026-08-21_flash_regime_vs_redraw/epsilon_distance.py legA legB [legA legB ...]"""
import json, re, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
def load(leg): return {r["id"]: r for r in json.load(open(ROOT/"outputs"/f"pipeline_output.{leg}.json"))["per_constraint"]}
cfg = (ROOT/"prolog"/"config.pl").read_text()
th = {k: float(v) for k, v in re.findall(r"param\((\w*epsilon\w*),\s*([0-9.]+)\)", cfg)}
print("ε thresholds:", th)
args = sys.argv[1:] or ["flash2","flash3","flash_think","flash_think2","flash2","flash_think","stealth","nemotron"]
for a, b in zip(args[::2], args[1::2]):
    A, B = load(a), load(b); ids = [i for i in A if i in B]
    d = sorted(abs((A[i].get("base_extractiveness") or 0) - (B[i].get("base_extractiveness") or 0)) for i in ids); n = len(d)
    q = lambda p: d[int(p*(n-1))]
    print(f"{a} vs {b:14} n={n:4} |Δε| median={q(.5):.2f} p75={q(.75):.2f} p90={q(.9):.2f} max={d[-1]:.2f} | ≥0.10: {sum(x>=0.10 for x in d)/n:.0%}  ≥0.20: {sum(x>=0.20 for x in d)/n:.0%}")
