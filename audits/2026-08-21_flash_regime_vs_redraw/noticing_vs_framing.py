#!/usr/bin/env python3
"""Noticing vs framing (2026-08-22; OQ-349). Paired Δε (thinking − off) grouped by the thinking-OFF
draw's claimed_type, the same split on the thinking-off redraw as the regression-to-mean CONTROL,
the benign stratum (off ε ≤ 0.15), and the named mountains raised ≥0.30 in BOTH thinking draws."""
import json, collections, statistics as st
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
def load(leg): return {r["id"]: r for r in json.load(open(ROOT/"outputs"/f"pipeline_output.{leg}.json"))["per_constraint"]}
A, A2, B, B2 = load("flash2"), load("flash3"), load("flash_think"), load("flash_think2")
e = lambda r: r.get("base_extractiveness") or 0
def split(X, Y, label):
    ids = [i for i in X if i in Y]; by = collections.defaultdict(list)
    for i in ids: by[X[i].get("claimed_type")].append(e(Y[i]) - e(X[i]))
    print(f"--- {label} ---")
    for ct, d in sorted(by.items(), key=lambda x: -len(x[1])):
        print(f"  {str(ct):13} n={len(d):4} mean Δε={st.mean(d):+.3f} up≥.05={sum(x>0.05 for x in d)/len(d):.0%} down≥.05={sum(x<-0.05 for x in d)/len(d):.0%}")
    b = [i for i in ids if e(X[i]) <= 0.15]; d = [e(Y[i]) - e(X[i]) for i in b]
    print(f"  benign (off ε≤.15) n={len(b)} mean Δε={st.mean(d):+.3f} raised≥.10={sum(x>=0.10 for x in d)/len(d):.0%} raised≥.30={sum(x>=0.30 for x in d)/len(d):.0%}")
split(A, B, "regime: flash2 → flash_think"); split(A2, B2, "regime replication: flash3 → flash_think2")
split(A, A2, "CONTROL thinking-off redraw: flash2 → flash3"); split(B, B2, "thinking-on redraw: flash_think → flash_think2")
rows = [(e(B[i]) - e(A[i]), e(B2[i]) - e(A2[i]), i) for i in A if A[i].get("claimed_type") == "mountain" and i in B and i in A2 and i in B2]
both = sorted([r for r in rows if r[0] >= 0.30 and r[1] >= 0.30], key=lambda r: -r[0])
print(f"\nmountains raised ≥0.30 in BOTH thinking draws: {len(both)} of {len(rows)}")
for d1, d2, i in both: print(f"  {d1:+.2f} {d2:+.2f} {i}")
