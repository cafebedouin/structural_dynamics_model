#!/usr/bin/env python3
"""Direction, not just spread: per-leg mean ε / suppression and per-seat type mix, plus the paired
Δε sign on the regime pair (2026-08-22; OQ-343). Usage: seat_direction.py leg [leg ...] [--pair A B]"""
import json, sys, collections, statistics as st
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
def load(leg): return {r["id"]: r for r in json.load(open(ROOT/"outputs"/f"pipeline_output.{leg}.json"))["per_constraint"]}
argv = sys.argv[1:]; pair = None
if "--pair" in argv: i = argv.index("--pair"); pair = argv[i+1:i+3]; argv = argv[:i]
legs = argv or ["flash2","flash3","flash_think","flash_think2"]
for leg in legs:
    R = load(leg); n = len(R)
    eps = [r.get("base_extractiveness") or 0 for r in R.values()]
    sup = [r["suppression"] for r in R.values() if isinstance(r.get("suppression"), (int, float))]
    seat = collections.Counter()
    for r in R.values():
        for k, v in (r.get("perspectives") or {}).items(): seat[(k, v.get("type") if isinstance(v, dict) else v)] += 1
    print(f"{leg:13} n={n} mean ε={st.mean(eps):.3f} sd={st.pstdev(eps):.3f} | mean suppression={st.mean(sup):.3f}")
    for k in ("powerless", "moderate", "institutional", "analytical"):
        print(f"    {k:13} " + " ".join(f"{t}={c/n:.0%}" for (kk, t), c in sorted(seat.items(), key=lambda x: -x[1]) if kk == k))
if pair:
    A, B = load(pair[0]), load(pair[1]); ids = [i for i in A if i in B]
    d = [(B[i].get("base_extractiveness") or 0) - (A[i].get("base_extractiveness") or 0) for i in ids]
    print(f"\npaired Δε ({pair[1]} − {pair[0]}): mean={st.mean(d):+.3f}, up≥.05={sum(x>0.05 for x in d)/len(d):.0%} down≥.05={sum(x<-0.05 for x in d)/len(d):.0%}")
