#!/usr/bin/env python3
"""OQ-345 before/after diff for a stakeholder-backfilled leg (2026-08-22).

BEFORE = outputs/pipeline_output.<leg>.prebackfill.json (the leg classified at HEAD before the
backfill); AFTER = outputs/pipeline_output.<leg>.json (reclassified at HEAD after). Stories are
stratified by whether their AFTER provenance_source carries `+stakeholder_backfill` (read from
the leg's .pl files). Reports, per stratum: n, same-seed agreement on h1_band / verdict_join /
signature / purity_band / claimed_type / ε, |Δε| summary, and h1_stakeholder null share before vs
after (the field the backfill exists to populate). The UNTOUCHED stratum is the control: it must
read ~100% agreement (same stories, same engine) — if it does not, the diff is measuring engine
drift, not the backfill.
Usage: backfill_diff.py haiku | flash
"""
import json, re, sys, statistics as st
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
leg = sys.argv[1] if len(sys.argv) > 1 else "haiku"
def load(p): return {r["id"]: r for r in json.load(open(p))["per_constraint"]}
B = load(ROOT/"outputs"/f"pipeline_output.{leg}.prebackfill.json"); A = load(ROOT/"outputs"/f"pipeline_output.{leg}.json")
tag = set()
for p in (ROOT/"prolog"/f"testsets_{leg}").glob("*.pl"):
    if "+stakeholder_backfill" in p.read_text(encoding="utf-8", errors="ignore"): tag.add(p.stem)
ids = [i for i in B if i in A]
F = {"h1_band": lambda r: r.get("h1_band"), "verdict": lambda r: (r.get("verdict_join") or {}).get("verdict"),
     "signature": lambda r: r.get("signature"), "purity_band": lambda r: r.get("purity_band"),
     "claimed_type": lambda r: r.get("claimed_type"), "ε": lambda r: r.get("base_extractiveness")}
print(f"leg={leg} before n={len(B)} after n={len(A)} shared={len(ids)} backfilled(tagged)={len(tag & set(ids))}")
for name, S in (("BACKFILLED stratum", [i for i in ids if i in tag]), ("UNTOUCHED stratum (control)", [i for i in ids if i not in tag])):
    if not S: print(f"{name}: n=0"); continue
    agree = {k: sum(1 for i in S if f(B[i]) == f(A[i])) / len(S) for k, f in F.items()}
    d = [abs((A[i].get("base_extractiveness") or 0) - (B[i].get("base_extractiveness") or 0)) for i in S]
    nb = sum(1 for i in S if B[i].get("h1_stakeholder") is None) / len(S); na = sum(1 for i in S if A[i].get("h1_stakeholder") is None) / len(S)
    print(f"{name}: n={len(S)} | agreement " + " ".join(f"{k}={v:.0%}" for k, v in agree.items())
          + f" | |Δε| median={st.median(d):.2f} ≥0.10={sum(x>=0.10 for x in d)/len(S):.0%}"
          + f" | h1_stakeholder null: before={nb:.0%} after={na:.0%}")
