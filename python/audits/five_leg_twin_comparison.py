#!/usr/bin/env python3
"""Five-leg cross-model comparison of prolog/testsets* (2026-07-19).

Consumes per-leg classify_corpus outputs (outputs/pipeline_output_<leg>.json, all at the
SAME code HEAD so the engine is held fixed) and reports, per leg and paired-by-seed:

  A. Marginals per leg: N, h1_band histogram (cohomological obstruction), verdict_join,
     maxent_top_type, signature, cs_pattern.
  B. Paired same-seed comparison across the twins (haiku/flash/sonnet/kimi share the
     never_generated seed pool; testsets is a 150-story mixed subset). Pairs by
     constraint_id (== filename). Reports the shared-seed intersection and, over it,
     agreement rate on h1_band and maxent_top_type.

Framing (CLAUDE.md 'Generation is stochastic'): same-seed cross-model differences are
EXPECTED redraws, not noise — read agreement as situation-fixed / disagreement as
seat-expressive (model-dependent), NOT as an error rate.

Usage: python3 python/audits/five_leg_twin_comparison.py
Writes: outputs/five_leg_comparison.json  (+ prints a summary table)
"""
import collections
import json
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "outputs"

# leg -> classify_corpus output file (produced fresh at HEAD by the driver script below)
LEGS = {
    "testsets": "pipeline_output_testsets_head.json",
    "haiku": "pipeline_output_haiku_head.json",
    "flash": "pipeline_output_flash_head.json",
    "sonnet": "pipeline_output_sonnet_head.json",
    "kimi": "pipeline_output_kimi_head.json",
}


def _load(leg, fname):
    p = OUT / fname
    if not p.exists():
        return None
    d = json.loads(p.read_text())
    pc = d.get("per_constraint", [])
    man = d.get("manifest", {})
    # index by constraint id
    by_id = {}
    for r in pc:
        cid = r.get("id") or r.get("constraint_id")
        if cid:
            by_id[cid] = r
    return {"manifest": man, "by_id": by_id}


def _hist(records, key):
    c = collections.Counter()
    for r in records:
        c[r.get(key)] += 1
    return dict(sorted(c.items(), key=lambda kv: (str(kv[0]))))


def main():
    legs = {}
    for leg, fname in LEGS.items():
        legs[leg] = _load(leg, fname)

    report = {"legs": {}, "code_commits": {}, "paired": {}}
    print("\n=== A. PER-LEG MARGINALS ===")
    for leg, data in legs.items():
        if not data:
            print(f"  {leg:9} MISSING ({LEGS[leg]})")
            report["legs"][leg] = None
            continue
        recs = list(data["by_id"].values())
        man = data["manifest"]
        report["code_commits"][leg] = man.get("code_commit_short")
        entry = {
            "n": len(recs),
            "code_commit": man.get("code_commit_short"),
            "h1_band": _hist(recs, "h1_band"),
            "verdict_join": _hist([{"v": (r.get("verdict_join") or {}).get("verdict")} for r in recs], "v"),
            "maxent_top_type": _hist(recs, "maxent_top_type"),
            "signature": _hist(recs, "signature"),
            "cs_pattern": _hist(recs, "cs_pattern"),
        }
        report["legs"][leg] = entry
        print(f"\n  --- {leg} (N={entry['n']}, commit={entry['code_commit']}) ---")
        print(f"    h1_band:        {entry['h1_band']}")
        print(f"    verdict_join:   {entry['verdict_join']}")
        print(f"    maxent_top_type:{entry['maxent_top_type']}")

    # code-state coherence check
    commits = {c for c in report["code_commits"].values() if c}
    report["code_state_coherent"] = (len(commits) <= 1)
    print(f"\n  code-state coherent across legs: {report['code_state_coherent']} ({commits})")

    # B. Paired same-seed comparison across the four twins (share the seed pool)
    print("\n=== B. PAIRED SAME-SEED (twins) ===")
    twin_names = [n for n in ("haiku", "flash", "sonnet", "kimi") if legs.get(n)]
    id_sets = {n: set(legs[n]["by_id"]) for n in twin_names}
    if len(twin_names) >= 2:
        shared = set.intersection(*id_sets.values()) if id_sets else set()
        report["paired"]["twins"] = twin_names
        report["paired"]["shared_seed_count"] = len(shared)
        print(f"  twins: {twin_names}")
        print(f"  shared constraint_ids (all twins): {len(shared)}")
        for a in ("h1_band", "maxent_top_type"):
            agree = sum(1 for cid in shared
                        if len({legs[n]["by_id"][cid].get(a) for n in twin_names}) == 1)
            rate = (agree / len(shared)) if shared else None
            report["paired"][f"{a}_all_agree"] = agree
            report["paired"][f"{a}_all_agree_rate"] = rate
            print(f"  all-{len(twin_names)} agree on {a}: {agree}/{len(shared)}"
                  + (f" ({rate:.1%})" if rate is not None else ""))
        # pairwise agreement matrix on h1_band
        print("  pairwise h1_band agreement:")
        for i, x in enumerate(twin_names):
            for y in twin_names[i + 1:]:
                common = id_sets[x] & id_sets[y]
                ag = sum(1 for cid in common
                         if legs[x]["by_id"][cid].get("h1_band") == legs[y]["by_id"][cid].get("h1_band"))
                r = (ag / len(common)) if common else None
                print(f"    {x:7}~{y:7}: {ag}/{len(common)}" + (f" ({r:.1%})" if r is not None else ""))

    (OUT / "five_leg_comparison.json").write_text(json.dumps(report, indent=2))
    print(f"\nwrote {OUT / 'five_leg_comparison.json'}")


if __name__ == "__main__":
    main()
