#!/usr/bin/env python3
"""OQ-349 arm 1 (2026-08-23): do thinking traces reach for the framework's extractive vocabulary?
Population: nemotron_think stories with a persisted raw response (message.reasoning present),
paired with the thinking-off twin (testsets_nemotron) for Δε. Strata: RISERS (Δε ≥ +0.30),
FLAT (|Δε| < 0.05), FALLERS (Δε ≤ −0.30). Per stratum: share of traces containing framework terms,
mean count per trace, and where in the trace the first term appears (fraction of trace length).
Framing predicts risers invoke the vocabulary earlier and more; noticing predicts domain reasoning
arriving at extraction without it. Usage: reasoning_text_census.py [think_leg off_leg]"""
import json, re, sys, statistics as st
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
think, off = (sys.argv[1:3] + ["nemotron_think", "nemotron"])[:2]
def load(leg): return {r["id"]: r for r in json.load(open(ROOT/"outputs"/f"pipeline_output.{leg}.json"))["per_constraint"]}
T, O = load(think), load(off)
TERMS = re.compile(r"\b(snare|extract\w*|coerc\w*|false summit|naturali[sz]\w*|suppress\w*|tangled[_ ]rope|piton|scaffold|rope|mountain|beneficiar\w*|victim\w*|payer)\b", re.I)
rows = []
for cid in T:
    if cid not in O: continue
    p = ROOT/"outputs"/f"no_scope_runs_{think}"/"responses"/f"{cid}.json"
    if not p.exists(): continue
    msg = json.load(open(p))["choices"][0]["message"]; tr = msg.get("reasoning") or ""
    if not tr.strip(): continue
    d = (T[cid].get("base_extractiveness") or 0) - (O[cid].get("base_extractiveness") or 0)
    hits = [m.start() for m in TERMS.finditer(tr)]
    rows.append((d, len(tr), len(hits), (hits[0] / len(tr)) if hits else None))
def strat(name, f):
    S = [r for r in rows if f(r[0])]
    if not S: print(f"{name:28} n=0"); return
    has = [r for r in S if r[2] > 0]
    print(f"{name:28} n={len(S):4} | trace len median={int(st.median(r[1] for r in S)):6} chars | traces w/ framework terms={len(has)/len(S):.0%} | terms/trace mean={st.mean(r[2] for r in S):.1f} | first term at {st.median(r[3] for r in has) if has else float('nan'):.2f} of trace")
print(f"{think} vs {off}: {len(rows)} paired stories with a persisted reasoning trace")
strat("RISERS (Δε ≥ +0.30)", lambda d: d >= 0.30); strat("mild risers (+0.10..+0.30)", lambda d: 0.10 <= d < 0.30)
strat("FLAT (|Δε| < 0.05)", lambda d: abs(d) < 0.05); strat("FALLERS (Δε ≤ −0.30)", lambda d: d <= -0.30)

# --- second cut (same run): ORDER — type-commitment phrase vs first ε number in the trace ---
COMMIT = re.compile(r"\b(this is (a |an )?(clear |classic |textbook )?(snare|tangled[_ ]rope|mountain|false summit|rope|piton|scaffold)|classif\w+ (this |it )?as (a |an )?(snare|tangled[_ ]rope|mountain|rope|piton|scaffold)|(snare|tangled[_ ]rope|mountain|false summit)\b[^.]{0,40}\b(because|since))", re.I)
EPS = re.compile(r"(extractiveness|epsilon|ε)\W{0,12}(0?\.\d+|[01]\.\d+)", re.I)
def order_strat(name, f):
    S = []
    for cid in T:
        if cid not in O: continue
        p = ROOT/"outputs"/f"no_scope_runs_{think}"/"responses"/f"{cid}.json"
        if not p.exists(): continue
        tr = (json.load(open(p))["choices"][0]["message"].get("reasoning") or "")
        d = (T[cid].get("base_extractiveness") or 0) - (O[cid].get("base_extractiveness") or 0)
        if not f(d) or not tr.strip(): continue
        c = COMMIT.search(tr); e = EPS.search(tr)
        S.append((c.start() if c else None, e.start() if e else None, len(tr)))
    if not S: print(f"{name:28} n=0"); return
    both = [s for s in S if s[0] is not None and s[1] is not None]
    type_first = sum(1 for s in both if s[0] < s[1])
    print(f"{name:28} n={len(S):4} | has type-commitment={sum(1 for s in S if s[0] is not None)/len(S):.0%} has ε number={sum(1 for s in S if s[1] is not None)/len(S):.0%} | of {len(both)} with both: TYPE BEFORE ε = {type_first/len(both) if both else float('nan'):.0%}")
print("\n-- order: type commitment vs first ε number --")
order_strat("RISERS (Δε ≥ +0.30)", lambda d: d >= 0.30); order_strat("mild risers (+0.10..+0.30)", lambda d: 0.10 <= d < 0.30)
order_strat("FLAT (|Δε| < 0.05)", lambda d: abs(d) < 0.05); order_strat("FALLERS (Δε ≤ −0.30)", lambda d: d <= -0.30)
