#!/usr/bin/env python3
"""oq347_prereg_diff.py — the Phase-5 pre-registered diff (plan 2026-08-25).

Compares, per leg, the OLD artifact outputs/pipeline_output.<leg>.json (15 commits, mostly
code_dirty) against the NEW coherent output outputs/coherent_<HEAD7>/... — `per_constraint`
ONLY, indexed by `id` (never the whole file: `pipeline_run_at` re-stamps every run, so a
whole-file diff always differs even when behavior is preserved).

Two strata, two different checks:
  UNCHANGED legs (no Phase-1 rescue landed): expectation IDENTICAL — n_only_old, n_only_new,
    n_changed all 0. ANY diff is the finding (stop-and-ask trigger 1); attribute by stage-hash
    before calling it non-determinism.
  CHANGED legs (--changed): n_only_old == 0; n_only_new == the rescue's provenance-tagged count
    (--expect-new leg=N, from the driver's own artifact count, not the ladder); on PRE-EXISTING
    ids, changes touching h1_band / signature / claimed_type / base_extractiveness are a
    stop-and-ask (trigger 2); verdict_join changes are the known corpus-relative channel
    (OQ-345's untouched-stratum control: 97–98% verdict, ~100% elsewhere) — reported, not
    blocked; other corpus-relative keys (maxent/ensemble-fit components) are reported with
    counts.

Prints a per-leg table plus a changed-key histogram, and exits 1 if any stop condition fired.

Usage: oq347_prereg_diff.py --coherent-dir outputs/coherent_<HEAD7>
                            [--changed nemotron nemotron_think] [--expect-new nemotron=5 ...]
"""
from __future__ import annotations

import argparse
import collections
import json
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
if str(REPO / "python") not in sys.path:
    sys.path.insert(0, str(REPO / "python"))

from audits.leg_diagnostic_table import declared_roster, output_name_for

OUT = REPO / "outputs"
STOP_FIELDS = {"h1_band", "signature", "claimed_type", "base_extractiveness"}


def per_constraint(p: Path) -> dict:
    d = json.load(open(p))
    return {r["id"]: r for r in d["per_constraint"]}


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--coherent-dir", required=True)
    ap.add_argument("--changed", nargs="*", default=[],
                    help="legs whose corpus changed in Phase 1 (bare names, e.g. nemotron)")
    ap.add_argument("--expect-new", nargs="*", default=[],
                    help="leg=N: provenance-tagged rescue count per changed leg")
    args = ap.parse_args()
    cdir = Path(args.coherent_dir)
    changed = set(args.changed)
    expect_new = dict(s.split("=") for s in args.expect_new)
    expect_new = {k: int(v) for k, v in expect_new.items()}

    roster, _ = declared_roster()
    legs = [r["leg"] for r in roster if r["leg"] != "testsets"]  # testsets: not in the coherent set
    stops = []
    print(f"{'leg':26} {'stratum':9} {'n_old':>5} {'n_new':>5} {'only_old':>8} {'only_new':>8} {'changed':>7}  changed keys")
    for leg in legs:
        bare = leg[len("testsets_"):]
        old_p = OUT / output_name_for(leg)
        new_p = cdir / output_name_for(leg)
        if not old_p.exists() or not new_p.exists():
            print(f"{leg:26} MISSING {'OLD' if not old_p.exists() else 'NEW'}")
            stops.append(f"{leg}: missing artifact")
            continue
        O, N = per_constraint(old_p), per_constraint(new_p)
        only_old = sorted(set(O) - set(N))
        only_new = sorted(set(N) - set(O))
        shared = [i for i in O if i in N]
        keyhist = collections.Counter()
        changed_ids = []
        for i in shared:
            if O[i] != N[i]:
                keys = frozenset(k for k in set(O[i]) | set(N[i]) if O[i].get(k) != N[i].get(k))
                keyhist.update(keys)
                changed_ids.append((i, keys))
        stratum = "CHANGED" if bare in changed else "unchanged"
        keystr = ",".join(f"{k}:{v}" for k, v in keyhist.most_common(8)) or "-"
        print(f"{leg:26} {stratum:9} {len(O):>5} {len(N):>5} {len(only_old):>8} {len(only_new):>8} {len(changed_ids):>7}  {keystr}")

        if stratum == "unchanged":
            if only_old or only_new or changed_ids:
                stops.append(f"{leg}: UNCHANGED leg diff non-empty (only_old={len(only_old)}, "
                             f"only_new={len(only_new)}, changed={len(changed_ids)}) — trigger 1")
        else:
            if only_old:
                stops.append(f"{leg}: n_only_old={len(only_old)} != 0 ({only_old[:5]}...) — trigger 2")
            if bare in expect_new and len(only_new) != expect_new[bare]:
                stops.append(f"{leg}: n_only_new={len(only_new)} != expected rescue count "
                             f"{expect_new[bare]} — trigger 2")
            hot = [(i, k) for i, ks in changed_ids for k in ks if k in STOP_FIELDS]
            if hot:
                stops.append(f"{leg}: pre-existing-id changes touch {sorted({k for _, k in hot})} "
                             f"on {len({i for i, _ in hot})} ids — trigger 2 STOP")
            vj = sum(1 for _, ks in changed_ids if "verdict_join" in ks)
            if vj:
                print(f"{'':26} known channel: verdict_join changed on {vj} pre-existing ids "
                      f"(OQ-345 corpus-relative residue) — reported, not blocked")

    if stops:
        print("\nSTOP CONDITIONS FIRED:")
        for s in stops:
            print(f"  - {s}")
        sys.exit(1)
    print("\nAll legs within pre-registered expectations.")


if __name__ == "__main__":
    main()
