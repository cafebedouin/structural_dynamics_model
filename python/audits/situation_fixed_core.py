#!/usr/bin/env python3
"""situation_fixed_core.py — the situation-fixed core over the coherent leg set (OQ-347 step 4).

The core: the seed (story id) set on which EVERY same-model leg pair agrees on all three of
  h1_band  ∧  verdict_join.verdict  ∧  signature
— i.e. the classifications the situation fixes regardless of the draw. Same-model pairs are
DERIVED, never recalled: legs come from shared.corpus_legs.LIVE_LEGS (minus the live `testsets`
leg, which moves continuously and is no pair partner — plan 2026-08-25), and each leg's model is
the distinct story_provenance field-7 value off its own .pl files (OQ-78: a leg's model is not
its directory name). Loader idiom is paired_agreement.py's: outputs/pipeline_output.<leg>.json
(or --dir), key rows by filename-derived `id` (GAP-35: the only pairing structure),
print-and-skip a missing leg — but a skipped leg SHRINKS the pair set, so skips are REPORTED in
the output block, not just printed.

Null-safety (OQ-51): `h1_band` null = UNDETERMINED, never 0. A seed where every pair agrees but
some agreed value is null (h1_band None, or a None verdict/signature) is counted in its own
`agreed_on_null` column, NOT folded into the core. The core proper requires agreement on
NON-NULL values of all three fields.

Reports: core size, all-pairs intersection n, per-pair agreement, and the EXCLUDED seed ids
explicitly (so a post-rescue rerun is a comparison, not a fresh computation).

Usage: situation_fixed_core.py [--dir outputs] [--json PATH]
"""
from __future__ import annotations

import argparse
import collections
import itertools
import json
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
if str(REPO / "python") not in sys.path:
    sys.path.insert(0, str(REPO / "python"))

from audits.leg_diagnostic_table import PROV_RE  # multi-line-safe story_provenance/8 parse

FIELDS = {
    "h1_band": lambda r: r.get("h1_band"),
    "verdict": lambda r: (r.get("verdict_join") or {}).get("verdict"),
    "signature": lambda r: r.get("signature"),
}


def leg_models(leg: str) -> collections.Counter:
    """Distinct story_provenance model ids (field 7) on the leg's .pl files."""
    models = collections.Counter()
    for f in (REPO / "prolog" / leg).glob("*.pl"):
        m = PROV_RE.search(f.read_text(encoding="utf-8", errors="replace"))
        models[m.group(7) if m else "PROMPT_HASH_ABSENT"] += 1
    return models


def load(leg: str, out_dir: Path):
    p = out_dir / f"pipeline_output.{leg[len('testsets_'):]}.json"
    if not p.exists():
        print(f"  [{leg}] no output at {p} — skipped")
        return None
    d = json.load(open(p))
    return d["manifest"], {r["id"]: r for r in d["per_constraint"]}


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=str(REPO / "outputs"),
                    help="directory holding pipeline_output.<leg>.json (default: outputs/ — "
                         "the ADOPTED coherent set; pass outputs/coherent_<HEAD>/ pre-adoption)")
    ap.add_argument("--json", default=None, help="also write the full result as JSON here")
    args = ap.parse_args()
    out_dir = Path(args.dir)

    from shared.corpus_legs import LIVE_LEGS
    legs = [l for l in LIVE_LEGS if l != "testsets"]  # live leg: no stable pair partner

    data, skipped = {}, []
    for leg in legs:
        v = load(leg, out_dir)
        if v is None:
            skipped.append(leg)
            continue
        data[leg] = v

    commits = {m["code_commit_short"] for m, _ in data.values()}
    dirty = {leg for leg, (m, _) in data.items() if m.get("code_dirty")}
    print(f"legs loaded: {len(data)}/{len(legs)}  skipped: {skipped or 'none'}")
    print(f"engine commits present: {sorted(commits)}"
          + ("  <-- NOT coherent; reclassify before trusting this" if len(commits) > 1 else "  (coherent)"))
    if dirty:
        print(f"code_dirty legs: {sorted(dirty)}  <-- unreconstructable states")

    # Same-model grouping, derived per leg. A mixed-model leg cannot pair.
    model_of = {}
    for leg in data:
        models = leg_models(leg)
        real = [m for m in models if m != "PROMPT_HASH_ABSENT"]
        model_of[leg] = real[0] if len(real) == 1 else None
        print(f"  {leg:26} model={model_of[leg] or 'MIXED/' + str(dict(models))}")
    groups = collections.defaultdict(list)
    for leg, m in sorted(model_of.items()):
        if m is not None:
            groups[m].append(leg)
    pairs = [(a, b) for m, ls in sorted(groups.items()) if len(ls) > 1
             for a, b in itertools.combinations(ls, 2)]
    print(f"\nsame-model pairs ({len(pairs)}):")
    for a, b in pairs:
        print(f"  {a} <-> {b}   [{model_of[a]}]")
    if not pairs:
        print("NO same-model pairs derivable — core undefined.")
        return

    # All-pairs seed intersection (over legs that participate in >=1 pair).
    paired_legs = sorted({l for p in pairs for l in p})
    common = set.intersection(*(set(data[l][1]) for l in paired_legs))
    universe = set.union(*(set(data[l][1]) for l in paired_legs))
    excluded = sorted(universe - common)

    core, agreed_on_null, disagree = [], [], []
    per_pair_agree = {p: {k: 0 for k in FIELDS} for p in pairs}
    for i in sorted(common):
        all_agree, any_null = True, False
        for a, b in pairs:
            ra, rb = data[a][1][i], data[b][1][i]
            for k, f in FIELDS.items():
                va, vb = f(ra), f(rb)
                if va == vb:
                    per_pair_agree[(a, b)][k] += 1
                    if va is None:
                        any_null = True
                else:
                    all_agree = False
        if not all_agree:
            disagree.append(i)
        elif any_null:
            agreed_on_null.append(i)
        else:
            core.append(i)

    n = len(common)
    print(f"\nall-pairs intersection n = {n}  (universe {len(universe)}, "
          f"EXCLUDED {len(excluded)} — ids listed below)")
    print(f"per-pair agreement over the intersection:")
    for (a, b), cnt in per_pair_agree.items():
        print(f"  {a} <-> {b}: " + "  ".join(f"{k}={cnt[k]/n:.0%}" for k in FIELDS))
    print(f"\nSITUATION-FIXED CORE: {len(core)} of {n} "
          f"({len(core)/n:.1%})" if n else "\nSITUATION-FIXED CORE: n=0 (empty intersection)")
    print(f"agreed-but-on-null (own column, NOT core; OQ-51): {len(agreed_on_null)}")
    print(f"disagreeing on >=1 pair/field: {len(disagree)}")
    print(f"\nEXCLUDED seed ids ({len(excluded)}):")
    for i in excluded:
        print(f"  {i}")

    if args.json:
        json.dump({"out_dir": str(out_dir), "commits": sorted(commits),
                   "skipped_legs": skipped, "model_of": model_of,
                   "pairs": [list(p) for p in pairs], "intersection_n": n,
                   "core": core, "agreed_on_null": agreed_on_null,
                   "disagree": disagree, "excluded_ids": excluded},
                  open(args.json, "w"), indent=1)
        print(f"\n[json] {args.json}")


if __name__ == "__main__":
    main()
