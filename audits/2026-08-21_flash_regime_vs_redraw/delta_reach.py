#!/usr/bin/env python3
"""Do ε moves reach the engine's outputs? Per pair: change rate of the four-seat type vector
(perspectives: powerless/moderate/institutional/analytical), verdict_join and h1_band, conditioned
on |Δε| ≥ 0.10 vs < 0.10 (2026-08-22; OQ-343 b). Usage: delta_reach.py legA legB [...]"""
import json, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
def load(leg): return {r["id"]: r for r in json.load(open(ROOT/"outputs"/f"pipeline_output.{leg}.json"))["per_constraint"]}
def tvec(r):
    p = r.get("perspectives") or {}
    return tuple(sorted((k, (v.get("type") if isinstance(v, dict) else v)) for k, v in p.items()))
def eps(r): return r.get("base_extractiveness") or 0.0
args = sys.argv[1:] or ["flash2","flash3","flash_think","flash_think2","flash2","flash_think","stealth","nemotron"]
for a, b in zip(args[::2], args[1::2]):
    A, B = load(a), load(b); ids = [i for i in A if i in B]
    big = [i for i in ids if abs(eps(A[i]) - eps(B[i])) >= 0.10]; bs = set(big); small = [i for i in ids if i not in bs]
    def rate(S, f): return sum(1 for i in S if f(A[i]) != f(B[i])) / max(1, len(S))
    vj = lambda r: (r.get("verdict_join") or {}).get("verdict"); h1 = lambda r: r.get("h1_band")
    print(f"{a} vs {b:13} n={len(ids):4} | type-vector changed: all={rate(ids,tvec):.0%} |Δε|≥.10={rate(big,tvec):.0%} (n={len(big)}) <.10={rate(small,tvec):.0%}"
          f" | verdict: ≥.10={rate(big,vj):.0%} <.10={rate(small,vj):.0%} | h1: ≥.10={rate(big,h1):.0%} <.10={rate(small,h1):.0%}")
