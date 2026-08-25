#!/usr/bin/env python3
"""OQ-345 before/after diff for a stakeholder-backfilled leg (2026-08-22).

BEFORE = outputs/pipeline_output.<leg>.prebackfill.json (the leg classified at HEAD before the
backfill); AFTER defaults to the PRESERVED copy in outputs/_arms_oq345_2026-08-25/ — NOT the live
outputs/pipeline_output.<leg>.json, which the 2026-08-25 coherent reclassify (OQ-347) overwrote.
§9's pin: pipeline_output.haiku.json @ 0f432fb, pipeline_output.flash.json @ 2ce8e18 (md5s in
audits/2026-08-25_oq347_coherent_reclassify/preserved_arms.md). Pass --after to point elsewhere;
.gz paths are read transparently. Stories are stratified by whether their AFTER provenance_source
carries `+stakeholder_backfill` (read from the leg's .pl files). Reports, per stratum: n,
same-seed agreement on h1_band / verdict_join / signature / purity_band / claimed_type / ε,
|Δε| summary, and h1_stakeholder null share before vs after (the field the backfill exists to
populate). The UNTOUCHED stratum is the control: it must read ~100% agreement (same stories, same
engine) — if it does not, the diff is measuring engine drift, not the backfill.
Usage: backfill_diff.py {haiku|flash} [--after PATH] | backfill_diff.py --selftest
"""
import argparse, gzip, json, sys, statistics as st
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]


def eps_diffs(B, A, ids):
    """|Δε| over ids where BOTH sides carry a numeric ε. Absent (None) is excluded and COUNTED,
    never coerced to 0 (OQ-60: absent ≠ zero; the old `or 0` fabricated a 0.0)."""
    d, excl = [], 0
    for i in ids:
        eb, ea = B[i].get("base_extractiveness"), A[i].get("base_extractiveness")
        if eb is None or ea is None: excl += 1; continue
        d.append(abs(ea - eb))
    return d, excl


if "--selftest" in sys.argv:
    B = {"x": {"base_extractiveness": None}, "y": {"base_extractiveness": 0.0}}
    A = {"x": {"base_extractiveness": 0.5}, "y": {"base_extractiveness": 0.0}}
    d, excl = eps_diffs(B, A, ["x", "y"])
    assert excl == 1, f"null side must be excluded-and-counted, got excl={excl}"
    assert d == [0.0], f"genuine 0.0 must be KEPT, got {d}"
    print("selftest OK: null excluded-and-counted (1), 0.0 retained ([0.0])"); sys.exit(0)

ap = argparse.ArgumentParser()
ap.add_argument("leg", nargs="?", default="haiku", choices=["haiku", "flash"])
ap.add_argument("--after", default=None,
                help="AFTER-arm path (default: the preserved §9 arm, .gz). The live "
                     "outputs/pipeline_output.<leg>.json is the post-2026-08-25 coherent set — "
                     "passing it here compares against the wrong arm.")
args = ap.parse_args()
leg = args.leg
after_path = Path(args.after) if args.after else \
    ROOT / "outputs" / "_arms_oq345_2026-08-25" / f"pipeline_output.{leg}.json.gz"


def load(p):
    p = Path(p)
    opener = (lambda q: gzip.open(q, "rt")) if p.suffix == ".gz" else open
    with opener(p) as fh: d = json.load(fh)
    return {r["id"]: r for r in d["per_constraint"]}


B = load(ROOT/"outputs"/f"pipeline_output.{leg}.prebackfill.json"); A = load(after_path)
print(f"AFTER arm: {after_path}")
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
    d, excl = eps_diffs(B, A, S)
    nb = sum(1 for i in S if B[i].get("h1_stakeholder") is None) / len(S); na = sum(1 for i in S if A[i].get("h1_stakeholder") is None) / len(S)
    dmed = f"{st.median(d):.2f}" if d else "n/a"
    dge = f"{sum(x>=0.10 for x in d)/len(d):.0%}" if d else "n/a"
    print(f"{name}: n={len(S)} | agreement " + " ".join(f"{k}={v:.0%}" for k, v in agree.items())
          + f" | |Δε| median={dmed} ≥0.10={dge}"
          + (f" (EXCLUDED, ε absent: {excl})" if excl else "")
          + f" | h1_stakeholder null: before={nb:.0%} after={na:.0%}")
