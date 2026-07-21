#!/usr/bin/env python3
"""Deeper cross-model cuts on the five legs (2026-07-20) — quality-focused.

Consumes outputs/pipeline_output_<leg>_head.json (all at one engine HEAD). Adds, beyond the
marginals in five_leg_twin_comparison.py:

  A. Type confusion — kimi maxent_top_type vs each other model on shared seeds (does kimi
     collapse distinct structures onto one type?).
  B. Claimed-vs-computed type coherence — fraction where the story's AUTHORED claimed_type
     matches the engine's maxent_top_type. A model that declares a type the metrics don't
     support is authoring internally-incoherent constraints (a quality proxy).
  C. Authoring-richness proxies per leg (means/distributions): maxent_entropy (decisiveness),
     #omegas, #gaps + gap_status, #beneficiaries/#victims (stakeholder richness), arakelov_height.
  D. Committer axis: cs_pattern distribution, cs_instance_count, fraction with non-empty cs_verdicts.
  E. Band-3 homogeneity deep-dive (kimi): within band-3 stories, the perspective-type pattern
     (is it a stereotyped 3-of-4 disagreement?).

NOTE: the kimi leg is kimi-k2.6 (the batch-eligible model), NOT K3/"3.0". Findings characterize
k2.6 only.

Usage: python3 python/audits/five_leg_deeper_cuts.py
"""
import collections
import json
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "outputs"
LEGS = {
    "testsets": "pipeline_output_testsets_head.json",
    "haiku": "pipeline_output_haiku_head.json",
    "flash": "pipeline_output_flash_head.json",
    "sonnet": "pipeline_output_sonnet_head.json",
    "kimi": "pipeline_output_kimi_head.json",
}
TWINS = ["haiku", "flash", "sonnet", "kimi"]
TYPES = ["mountain", "rope", "scaffold", "snare", "tangled_rope", "piton", "naturalized", "unknown"]


def load():
    legs = {}
    for leg, fname in LEGS.items():
        p = OUT / fname
        if not p.exists():
            legs[leg] = None
            continue
        pc = json.loads(p.read_text()).get("per_constraint", [])
        legs[leg] = {r.get("id") or r.get("constraint_id"): r for r in pc}
    return legs


def _len(v):
    return len(v) if isinstance(v, (list, dict)) else 0


def mean(xs):
    xs = [x for x in xs if isinstance(x, (int, float))]
    return sum(xs) / len(xs) if xs else float("nan")


def main():
    legs = load()
    report = {}

    # ---- C. Authoring-richness proxies -------------------------------------
    print("=== C. AUTHORING-RICHNESS PROXIES (per-leg means) ===")
    print(f"  {'leg':8} {'N':>5} {'entropy':>8} {'#omega':>7} {'#gaps':>6} "
          f"{'#benef':>7} {'#victim':>8} {'arakelov':>9} {'claim=comp':>11}")
    rich = {}
    for leg in ["testsets"] + TWINS:
        recs = list(legs[leg].values()) if legs[leg] else []
        if not recs:
            continue
        coh = [1 if r.get("claimed_type") == r.get("maxent_top_type") else 0
               for r in recs if r.get("maxent_top_type") is not None]
        row = {
            "n": len(recs),
            "entropy": mean([r.get("maxent_entropy") for r in recs]),
            "omegas": mean([_len(r.get("omegas")) for r in recs]),
            "gaps": mean([_len(r.get("gaps")) for r in recs]),
            "benef": mean([_len(r.get("beneficiaries")) for r in recs]),
            "victims": mean([_len(r.get("victims")) for r in recs]),
            "arakelov": mean([r.get("arakelov_height") for r in recs]),
            "claim_eq_comp": (sum(coh) / len(coh)) if coh else float("nan"),
        }
        rich[leg] = row
        print(f"  {leg:8} {row['n']:>5} {row['entropy']:>8.3f} {row['omegas']:>7.2f} "
              f"{row['gaps']:>6.2f} {row['benef']:>7.2f} {row['victims']:>8.2f} "
              f"{row['arakelov']:>9.3f} {row['claim_eq_comp']:>10.1%}")
    report["richness"] = rich

    # ---- D. Committer axis -------------------------------------------------
    print("\n=== D. COMMITTER AXIS (cs_*) ===")
    cs = {}
    for leg in TWINS:
        recs = list(legs[leg].values()) if legs[leg] else []
        if not recs:
            continue
        pat = collections.Counter(r.get("cs_pattern") for r in recs)
        has_verdict = sum(1 for r in recs if _len(r.get("cs_verdicts")) > 0)
        cs[leg] = {
            "cs_pattern": dict(pat.most_common()),
            "cs_instance_count_mean": mean([r.get("cs_instance_count") for r in recs]),
            "pct_with_cs_verdict": has_verdict / len(recs),
        }
        print(f"  {leg:8} inst=<{cs[leg]['cs_instance_count_mean']:.2f}> "
              f"verdicts>0={cs[leg]['pct_with_cs_verdict']:.1%}  patterns={dict(pat.most_common(5))}")
    report["committer"] = cs

    # ---- A. Type confusion: kimi vs others on shared seeds -----------------
    print("\n=== A. TYPE CONFUSION (rows=other model's type, cols=kimi's) ===")
    report["confusion"] = {}
    for other in ["sonnet", "haiku", "flash"]:
        shared = set(legs["kimi"]) & set(legs[other])
        mat = collections.defaultdict(collections.Counter)
        for cid in shared:
            ot = legs[other][cid].get("maxent_top_type")
            kt = legs["kimi"][cid].get("maxent_top_type")
            mat[ot][kt] += 1
        report["confusion"][other] = {o: dict(c) for o, c in mat.items()}
        agree = sum(mat[t][t] for t in mat)
        print(f"\n  {other} -> kimi (shared {len(shared)}, diagonal-agree {agree}={agree/len(shared):.1%}):")
        present = [t for t in TYPES if t in mat or any(t in c for c in mat.values())]
        hdr = "    " + f"{other[:8]:>12} | " + " ".join(f"{t[:5]:>5}" for t in present)
        print(hdr)
        for o in present:
            if o not in mat:
                continue
            cells = " ".join(f"{mat[o].get(k,0):>5}" for k in present)
            print(f"    {o:>12} | {cells}")

    # ---- E. Band-3 homogeneity deep-dive (kimi) ----------------------------
    print("\n=== E. KIMI BAND-3 DEEP-DIVE (why so homogeneous?) ===")
    b3 = [r for r in legs["kimi"].values() if r.get("h1_band") == 3]
    print(f"  kimi band-3 stories: {len(b3)}")
    # perspective-type signature: the sorted tuple of the 4 perspective types
    persp_sig = collections.Counter()
    for r in b3:
        p = r.get("perspectives") or {}
        sig = tuple(sorted((k, p[k]) for k in ("powerless", "moderate", "institutional", "analytical") if k in p))
        persp_sig[tuple(v for _, v in sig)] += 1
    print("  top perspective-type patterns (powerless,moderate,institutional,analytical):")
    for pat, n in persp_sig.most_common(6):
        print(f"    {n:>4}  {pat}")
    # how many distinct patterns cover band-3?
    print(f"  distinct perspective patterns in band-3: {len(persp_sig)}")
    report["kimi_band3"] = {"n": len(b3), "distinct_patterns": len(persp_sig),
                            "top_patterns": [{"pattern": list(p), "n": n} for p, n in persp_sig.most_common(6)]}

    (OUT / "five_leg_deeper_cuts.json").write_text(json.dumps(report, indent=2))
    print(f"\nwrote {OUT / 'five_leg_deeper_cuts.json'}")


if __name__ == "__main__":
    main()
