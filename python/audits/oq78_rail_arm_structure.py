#!/usr/bin/env python3
"""
OQ-78 follow-on: is the `.x8`/`.x2` rail ONE inherited habit, or TWO habits that co-occur?

EXPLORATORY CHARACTERIZATION — descriptive, not a pinned condition, no falsifier attached.
It exists to make the re-scoped rail question concrete before anything is pre-registered
against it. Reads only artifacts already on disk (the frozen twin-leg classify_corpus
outputs from the 2026-08-10 idiom close).

The rail held (91.7% -> 78.2%) while the 0.68 point mass dissolved (30.0% -> 7.3%), and
mass moved BETWEEN the rail's arms. Three readings are separable on data in hand:

  R1 ONE HABIT, seed-driven.   Arm choice is a property of the story. Two Claude models
     on the same seed should agree on the arm more often than their marginals predict.
  R2 TWO HABITS, model-driven. Each model carries its own arm preference; arm choice is
     independent across models given the marginals (concordance ~ chance).
  R3 ARTEFACT of dissolution.  The `.x2` mass is not a second habit at all — it is where
     the point mass went when 0.68 spread to its neighbours (0.62/0.72). Diagnostic: the
     `.x2` gain concentrates immediately adjacent to the vacated point mass.

R1 vs R2 is measured by matched-seed arm concordance against a within-model shuffle null
(shuffling breaks any seed-level signal while preserving each model's marginal exactly).
R3 is measured by where the `.x2` mass actually sits.

Usage:  python3 python/audits/oq78_rail_arm_structure.py
"""

import json
import random
import sys
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))
sys.path.insert(0, str(ROOT / "python" / "audits"))
from oq78_railband_crosstab import (load_leg, load_default_leg, load_archive,  # noqa: E402
                                    LEGS, matched_set, quantile)

RNG_SEED = 20260810
N_PERM = 4000
ARMS = (8, 2)


def arm_of(story):
    d = story.digit
    return d if d in ARMS else None


def main():
    rng = random.Random(RNG_SEED)
    out = {"note": ("EXPLORATORY characterization, not a pinned condition; reads the "
                    "frozen 2026-08-10 leg artifacts only"),
           "rng_seed": RNG_SEED}

    legs = {}
    for name, fn, _exp in LEGS:
        legs[name], _mf = load_leg(name, fn, name)
    default, _ = load_default_leg()
    archive = load_archive()
    pops = dict(legs)
    pops["default_derived_sonnet4.5"] = [
        s for s in default
        if s.author == "claude-sonnet-4-5-20250929" and s.kind == "derived"]
    pops["archive_kernel_v2_test2"] = archive

    # ---- 1. Arm split within the rail, overall and per claimed_type
    split = {}
    for nm, st in pops.items():
        on = [s for s in st if arm_of(s)]
        c = Counter(arm_of(s) for s in on)
        tot = len(on)
        by_type = {}
        for t in sorted({s.ctype for s in on if s.ctype}):
            cell = [s for s in on if s.ctype == t]
            if len(cell) < 30:          # descriptive floor; smaller cells listed only
                continue
            cc = Counter(arm_of(s) for s in cell)
            by_type[t] = {"n_on_rail": len(cell),
                          "share_x8": round(cc[8] / len(cell), 4)}
        split[nm] = {"n": len(st), "n_on_rail": tot,
                     "rail_share": round(tot / len(st), 4),
                     "share_x8_within_rail": round(c[8] / tot, 4) if tot else None,
                     "by_claimed_type_share_x8": by_type}
    out["arm_split"] = split

    # ---- 2. R1 vs R2: matched-seed arm concordance vs a within-model shuffle null
    #        Shuffling within a model preserves its marginal EXACTLY and destroys only the
    #        seed-level pairing, so any excess concordance is seed signal, not marginal
    #        agreement. (Two models both 90% .x8 agree 82% by chance — the null prices
    #        that in, which a raw agreement rate would not.)
    pair_rows = {}
    names = [n for n, _, _ in LEGS]
    mids = matched_set(legs, names)
    idx = {n: {s.cid: s for s in legs[n]} for n in names}
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            both = [m for m in mids if arm_of(idx[a][m]) and arm_of(idx[b][m])]
            if len(both) < 50:
                pair_rows[f"{a}x{b}"] = {"n_both_on_rail": len(both),
                                         "skipped": "fewer than 50 seeds on the rail in both"}
                continue
            va = [arm_of(idx[a][m]) for m in both]
            vb = [arm_of(idx[b][m]) for m in both]
            obs = sum(1 for x, y in zip(va, vb) if x == y) / len(both)
            draws = []
            for _ in range(N_PERM):
                sb = vb[:]
                rng.shuffle(sb)
                draws.append(sum(1 for x, y in zip(va, sb) if x == y) / len(both))
            draws.sort()
            p50, p95, p99 = (quantile(draws, .50), quantile(draws, .95),
                             quantile(draws, .99))
            pair_rows[f"{a}x{b}"] = {
                "n_both_on_rail": len(both),
                "observed_arm_concordance": round(obs, 4),
                "shuffle_null_p50": round(p50, 4), "shuffle_null_p95": round(p95, 4),
                "shuffle_null_p99": round(p99, 4),
                "excess_over_null_p50": round(obs - p50, 4),
                "exceeds_null_p99": obs > p99}
    out["matched_seed_arm_concordance"] = pair_rows

    # ---- 2b. PARTIAL THE TYPE CHANNEL. Arm choice tracks claimed_type (see arm_split),
    #      and two models on one seed often agree on the type — so raw concordance excess
    #      could be type agreement wearing a shared-habit costume. Restrict to seeds where
    #      BOTH models assigned the same claimed_type and shuffle WITHIN that type, which
    #      removes the type channel entirely. Surviving excess is arm signal proper.
    partial_rows = {}
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            both = [m for m in mids
                    if arm_of(idx[a][m]) and arm_of(idx[b][m])
                    and idx[a][m].ctype and idx[a][m].ctype == idx[b][m].ctype]
            if len(both) < 50:
                partial_rows[f"{a}x{b}"] = {"n_same_type_both_on_rail": len(both),
                                            "skipped": "fewer than 50 usable seeds"}
                continue
            by_t = defaultdict(list)
            for m in both:
                by_t[idx[a][m].ctype].append(m)
            obs = sum(1 for m in both
                      if arm_of(idx[a][m]) == arm_of(idx[b][m])) / len(both)
            draws = []
            for _ in range(N_PERM):
                hits = 0
                for t, ms in by_t.items():
                    vb = [arm_of(idx[b][m]) for m in ms]
                    rng.shuffle(vb)
                    hits += sum(1 for m, y in zip(ms, vb) if arm_of(idx[a][m]) == y)
                draws.append(hits / len(both))
            draws.sort()
            p50, p99 = quantile(draws, .50), quantile(draws, .99)
            partial_rows[f"{a}x{b}"] = {
                "n_same_type_both_on_rail": len(both),
                "types": {t: len(ms) for t, ms in sorted(by_t.items(),
                                                         key=lambda kv: -len(kv[1]))},
                "observed_arm_concordance": round(obs, 4),
                "within_type_shuffle_null_p50": round(p50, 4),
                "within_type_shuffle_null_p99": round(p99, 4),
                "excess_over_null_p50": round(obs - p50, 4),
                "exceeds_null_p99": obs > p99}
    out["arm_concordance_partialled_by_type"] = partial_rows
    out["partial_reading"] = (
        "excess surviving the WITHIN-TYPE shuffle is arm signal proper; excess that "
        "vanishes here was claimed_type agreement, not a shared arm habit")

    out["concordance_reading"] = (
        "excess over the shuffle null > 0 and above p99 => R1 (arm choice carries "
        "seed-level signal, one habit applied per story); concordance at the null => R2 "
        "(independent per-model arm preferences, two co-occurring habits)")

    # ---- 3. R3: where does the .x2 mass actually sit?
    #        If .x2 is the vacated point mass spreading to neighbours, the gain should
    #        concentrate at 0.62/0.72 rather than spreading across the whole .x2 arm.
    def val_profile(st, digit):
        vals = [round(s.eps, 2) for s in st if s.digit == digit]
        c = Counter(vals)
        tot = len(vals) or 1
        return {"n": len(vals),
                "top": [[v, n, round(n / tot, 4)] for v, n in c.most_common(6)],
                "share_adjacent_to_068": round(
                    sum(n for v, n in c.items() if v in (0.62, 0.72)) / tot, 4)}
    out["x2_mass_location"] = {nm: val_profile(st, 2) for nm, st in pops.items()}
    out["x8_mass_location"] = {nm: val_profile(st, 8) for nm, st in pops.items()}
    out["r3_reading"] = (
        "if .x2 were the dissolved point mass spreading to its neighbours, "
        "share_adjacent_to_068 (0.62 + 0.72) would dominate the .x2 arm in the legs "
        "where the point mass fell; a broadly spread .x2 arm falsifies R3")

    p = ROOT / "outputs" / "oq78_rail_arm_structure.json"
    p.write_text(json.dumps(out, indent=2), encoding="utf-8")
    print(json.dumps(out, indent=2))
    print(f"\nwrote {p}", file=sys.stderr)


if __name__ == "__main__":
    main()
