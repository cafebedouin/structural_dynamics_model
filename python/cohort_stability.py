#!/usr/bin/env python3
"""Cohort replicate stability table + within-vs-between distance (OQ-109 Phase C, item 4 probe).

Builds, over the replicate draws of cohort-zero stories, a PER-FIELD draw-stability table on
STRUCTURED fields only (per audits/2026-06-12_cohort_zero/SIGMA_SEAT_PREDICTION.md scope), plus a
within-vs-between pairwise distance. The table is the instrument the σ/seat falsifier
(cohort_sigma_seat_eval.py) and any n=1-per-story cross-story meta-analysis consume.

PATTERN-5 GUARD (the whole point): positive-agreement and agreement-in-absence are reported
SEPARATELY. A field where every draw is absent/empty ([], null, missing) is agreement-IN-ABSENCE
— it is NOT draw-stable evidence (it carries no value to be stable about). Only fields that AGREE
on a present, non-empty value score as positive-stable (σ-side evidence); fields that DIFFER on a
present value score as unstable (seat-side evidence). Absence-agreement is excluded from the σ/seat
partition test.

A "story" = one provenance.seeded_from value; its draws = provenance.draw indices. Draw 1 lives in
json/<id>_c0.json; draws >=2 in audits/2026-06-12_cohort_zero/replicates/<id>_c0_d<k>.json.

Usage:
  python3 python/cohort_stability.py                 # discover all, print + write artifacts
  python3 python/cohort_stability.py --only organization_floor
"""
import argparse
import json
import re
from collections import Counter
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
JSON_DIR = REPO / "json"
REPLICATE_DIR = REPO / "audits/2026-06-12_cohort_zero/replicates"
OUT_DIR = REPO / "audits/2026-06-12_cohort_zero"

# Sentinels distinct from any real value.
ABSENT = ("__ABSENT__",)   # field/key missing entirely
EMPTY = ("__EMPTY__",)     # present but empty container ([], "", {})


# ---------------------------------------------------------------------------
# Extractors: each returns a hashable normalized token, or ABSENT / EMPTY.
# ---------------------------------------------------------------------------
def _get(d, path):
    cur = d
    for k in path.split("."):
        if not isinstance(cur, dict) or k not in cur:
            return ABSENT
        cur = cur[k]
    return cur


def _norm_name(s):
    return re.sub(r"\s+", " ", str(s).strip().lower())


def scalar(path):
    def f(story):
        v = _get(story, path)
        if v is ABSENT:
            return ABSENT
        return round(float(v), 6)
    return f


def cat(path):
    def f(story):
        v = _get(story, path)
        if v is ABSENT:
            return ABSENT
        return str(v)
    return f


def prose_presence(path):
    def f(story):
        v = _get(story, path)
        if v is ABSENT:
            return ABSENT
        return EMPTY if not str(v).strip() else "PRESENT"
    return f


def nameset(path):
    def f(story):
        v = _get(story, path)
        if v is ABSENT:
            return ABSENT
        if not isinstance(v, list):
            return EMPTY if not v else "PRESENT"
        if len(v) == 0:
            return EMPTY
        return tuple(sorted(_norm_name(x) for x in v))
    return f


def cardinality(path):
    def f(story):
        v = _get(story, path)
        if v is ABSENT:
            return ABSENT
        if isinstance(v, list):
            return len(v)
        return ABSENT
    return f


def list_presence(key):
    def f(story):
        v = story.get(key, ABSENT)
        if v is ABSENT:
            return ABSENT
        if isinstance(v, (list, dict)) and len(v) == 0:
            return EMPTY
        return "PRESENT"
    return f


def stakeholder_roster_card(story):
    s = story.get("stakeholders")
    if s is None:
        return ABSENT
    return EMPTY if len(s) == 0 else len(s)


def stakeholder_role_multiset(story):
    s = story.get("stakeholders")
    if not s:
        return ABSENT if s is None else EMPTY
    return tuple(sorted(Counter(_norm_name(x.get("role", "")) for x in s).items()))


def stakeholder_attr_multiset(attr):
    def f(story):
        s = story.get("stakeholders")
        if not s:
            return ABSENT if s is None else EMPTY
        return tuple(sorted(Counter(_norm_name(x.get(attr, "")) for x in s).items()))
    return f


def measurement_count(story):
    m = story.get("measurements")
    if m is None:
        return ABSENT
    return EMPTY if len(m) == 0 else len(m)


def measurement_metric_set(story):
    m = story.get("measurements")
    if not m:
        return ABSENT if m is None else EMPTY
    return tuple(sorted({_norm_name(x.get("metric", "")) for x in m}))


def measurement_grid_shape(story):
    """Per-metric series length multiset (shape, not values)."""
    m = story.get("measurements")
    if not m:
        return ABSENT if m is None else EMPTY
    by_metric = Counter(_norm_name(x.get("metric", "")) for x in m)
    return tuple(sorted(by_metric.values()))


def omega_count(story):
    o = story.get("omegas")
    if o is None:
        return ABSENT
    return EMPTY if len(o) == 0 else len(o)


def omega_id_set(story):
    o = story.get("omegas")
    if not o:
        return ABSENT if o is None else EMPTY
    return tuple(sorted(_norm_name(x.get("id", "")) for x in o))


# ---------------------------------------------------------------------------
# Field spec. bucket per SIGMA_SEAT_PREDICTION.md; flags: seed / known (-in-advance).
# ---------------------------------------------------------------------------
FIELDS = [
    # (key, extractor, predicted_bucket, flags)
    ("base_properties.suppression", scalar("base_properties.suppression"), "sigma", []),
    ("base_properties.theater_ratio", scalar("base_properties.theater_ratio"), "sigma", []),
    ("base_properties.accessibility_collapse", scalar("base_properties.accessibility_collapse"), "sigma", []),
    ("base_properties.resistance", scalar("base_properties.resistance"), "sigma", []),
    ("base_properties.extractiveness", scalar("base_properties.extractiveness"), "sigma", ["known"]),
    ("base_properties.requires_active_enforcement", cat("base_properties.requires_active_enforcement"), "sigma", []),
    ("base_properties.emerges_naturally", cat("base_properties.emerges_naturally"), "sigma", []),
    ("base_properties.has_sunset_clause", cat("base_properties.has_sunset_clause"), "sigma", []),
    ("base_properties.beneficiaries", nameset("base_properties.beneficiaries"), "sigma", []),
    ("base_properties.victims", nameset("base_properties.victims"), "sigma", []),
    ("base_properties.vindicated_propositions", nameset("base_properties.vindicated_propositions"), "sigma", []),
    ("base_properties.claimed_type", cat("base_properties.claimed_type"), "seat", ["known"]),
    ("stakeholders.roster_card", stakeholder_roster_card, "sigma", []),
    ("stakeholders.role_multiset", stakeholder_role_multiset, "sigma", []),
    ("stakeholders.power_multiset", stakeholder_attr_multiset("power"), "sigma", []),
    ("stakeholders.time_horizon_multiset", stakeholder_attr_multiset("time_horizon"), "sigma", []),
    ("stakeholders.exit_options_multiset", stakeholder_attr_multiset("exit_options"), "sigma", []),
    ("stakeholders.spatial_scope_multiset", stakeholder_attr_multiset("spatial_scope"), "sigma", []),
    ("six_questions.coordination_function", prose_presence("six_questions.coordination_function"), "sigma", []),
    ("six_questions.transfer_function", prose_presence("six_questions.transfer_function"), "sigma", []),
    ("six_questions.absent_voices", prose_presence("six_questions.absent_voices"), "sigma", []),
    ("six_questions.disappearance_verdict", cat("six_questions.disappearance_verdict"), "seat", []),
    ("six_questions.founding_problem", prose_presence("six_questions.founding_problem"), "sigma", []),
    ("six_questions.founding_problem_status", cat("six_questions.founding_problem_status"), "seat", []),
    ("six_questions.founding_problem_corroboration", prose_presence("six_questions.founding_problem_corroboration"), "sigma", []),
    ("gain_flow", list_presence("gain_flow"), "sigma", []),
    ("fixing_cost", list_presence("fixing_cost"), "sigma", []),
    ("omegas.count", omega_count, "seat", []),
    ("omegas.id_set", omega_id_set, "seat", []),
    ("measurements.count", measurement_count, "seat", []),
    ("measurements.metric_set", measurement_metric_set, "seat", []),
    ("measurements.grid_shape", measurement_grid_shape, "seat", []),
    ("interval", list_presence("interval"), "seat", []),
    ("boltzmann.presence", list_presence("boltzmann"), "seat", []),
    ("network.presence", list_presence("network"), "seat", []),
    ("directionality_overrides.presence", list_presence("directionality_overrides"), "seat", []),
    ("cs_structure.presence", list_presence("cs_structure"), "seat", []),
    ("coercion_grid.presence", list_presence("coercion_grid"), "seat", []),
]

# Seed-supplied fields (stability = input echo, NOT sigma evidence) — declared in the prediction.
SEED_SUPPLIED = [
    ("base_properties.human_readable", cat("base_properties.human_readable")),
    ("base_properties.topic_domain", cat("base_properties.topic_domain")),
]


def classify_field(values):
    """values: list of extracted tokens across draws. Returns (status, kind)."""
    uniq = set(values)
    all_absent = all(v is ABSENT for v in values)
    all_empty = all(v is EMPTY for v in values)
    if all_absent:
        return "stable", "absence"        # agreement-in-absence (field missing in all)
    if all_empty:
        return "stable", "absence"        # agreement-in-absence (empty container in all)
    if len(uniq) == 1:
        # single shared value; could it be EMPTY-only? handled above. Present, non-empty.
        return "stable", "positive"
    # differs across draws
    has_absence = any(v in (ABSENT, EMPTY) for v in values)
    return "unstable", ("presence-flip" if has_absence else "value-shift")


def _tok(v):
    if v is ABSENT:
        return "<absent>"
    if v is EMPTY:
        return "<empty>"
    return v


def build_story_table(draws):
    """draws: list of (draw_idx, story_dict), >=2. Returns per-field rows."""
    rows = []
    for key, extractor, bucket, flags in FIELDS:
        vals = [extractor(s) for _, s in draws]
        status, kind = classify_field(vals)
        rows.append({
            "field": key, "predicted_bucket": bucket, "flags": flags,
            "status": status, "agreement_kind": kind,
            "values": [str(_tok(v)) for v in vals],
        })
    seed_rows = []
    for key, extractor in SEED_SUPPLIED:
        vals = [extractor(s) for _, s in draws]
        status, kind = classify_field(vals)
        seed_rows.append({"field": key, "status": status, "agreement_kind": kind,
                          "values": [str(_tok(v)) for v in vals]})
    return rows, seed_rows


def pair_distance(s1, s2):
    """Fraction of COMPARABLE fields (excluding both-absent/both-empty) that differ.
    Returns (distance, n_comparable, n_positive_agree, n_absence_agree)."""
    n_cmp = n_diff = n_pos = n_abs = 0
    for key, extractor, bucket, flags in FIELDS:
        a, b = extractor(s1), extractor(s2)
        a_void = a in (ABSENT, EMPTY)
        b_void = b in (ABSENT, EMPTY)
        if a_void and b_void:
            n_abs += 1          # agreement-in-absence — not comparable, not a positive match
            continue
        n_cmp += 1
        if a == b:
            n_pos += 1
        else:
            n_diff += 1
    dist = (n_diff / n_cmp) if n_cmp else None
    return dist, n_cmp, n_pos, n_abs


def discover():
    """Returns {story_id: [(draw_idx, path), ...]} grouped by provenance.seeded_from.
    Draws live in json/<id>_c0.json (corpus-member draw 1 of an _c0 story) and the replicate
    dir (every draw of a replicate-probe story, incl. kernel-seeded triples). Group by
    seeded_from; dedup on (seeded_from, draw) preferring the json/ corpus member."""
    groups, seen = {}, {}
    cands = list(JSON_DIR.glob("*_c0.json")) + sorted(REPLICATE_DIR.glob("*.json"))
    for p in cands:
        try:
            d = json.load(open(p))
        except Exception:
            continue
        prov = d.get("provenance", {})
        sid = prov.get("seeded_from")
        draw = prov.get("draw")
        if sid is None or sid == "none" or draw is None:
            continue
        if (sid, draw) in seen:      # first writer wins (json/ enumerated before replicates)
            continue
        seen[(sid, draw)] = p
        groups.setdefault(sid, []).append((draw, p))
    for sid in groups:
        groups[sid].sort()
    return groups


def selftest():
    """Positive control for the INSTRUMENT (operator-required pre-spend witness).
    A pair differing in exactly one present field flips exactly that field's bit; an
    all-absent field scores agreement-in-absence, never positive."""
    base = {
        "base_properties": {"suppression": 0.5, "theater_ratio": 0.2, "claimed_type": "snare",
                            "beneficiaries": ["a", "b"]},
        "stakeholders": [{"role": "x", "power": "p"}],
        "measurements": [{"metric": "m1"}], "omegas": [{"id": "o1"}],
        # gain_flow / fixing_cost / has_sunset_clause deliberately absent
    }
    import copy
    ok = True

    # (1) identical pair -> present fields positive-stable, absent fields absence
    d2 = copy.deepcopy(base)
    rows, _ = build_story_table([(1, base), (2, d2)])
    by = {r["field"]: r for r in rows}
    if by["base_properties.suppression"]["agreement_kind"] != "positive":
        ok = False; print("FAIL: identical suppression not positive-stable")
    if by["gain_flow"]["agreement_kind"] != "absence":
        ok = False; print("FAIL: absent gain_flow not agreement-in-absence")
    if by["base_properties.has_sunset_clause"]["agreement_kind"] != "absence":
        ok = False; print("FAIL: absent has_sunset_clause scored as present")

    # (2) differ in EXACTLY one field -> only that field unstable
    d3 = copy.deepcopy(base)
    d3["base_properties"]["suppression"] = 0.9
    rows2, _ = build_story_table([(1, base), (2, d3)])
    flipped = [r["field"] for r in rows2 if r["status"] == "unstable"]
    if flipped != ["base_properties.suppression"]:
        ok = False; print(f"FAIL: one-field change flipped {flipped}, expected only suppression")

    # (3) present-in-one, absent-in-other -> presence-flip unstable, NOT positive
    d4 = copy.deepcopy(base)
    d4["gain_flow"] = [{"x": 1}]
    rows3, _ = build_story_table([(1, base), (2, d4)])
    gf = next(r for r in rows3 if r["field"] == "gain_flow")
    if not (gf["status"] == "unstable" and gf["agreement_kind"] == "presence-flip"):
        ok = False; print(f"FAIL: present/absent gain_flow = {gf['status']}/{gf['agreement_kind']}")

    # (4) distance: all-absent fields not scored as positive agreement
    dist, nc, npos, nab = pair_distance(base, d2)
    if nab < 1:
        ok = False; print("FAIL: identical pair recorded 0 absence-agreements (gain_flow etc.)")
    if dist != 0.0:
        ok = False; print(f"FAIL: identical pair distance {dist} != 0")

    print("SELFTEST PASS" if ok else "SELFTEST FAIL")
    return ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--only", default="")
    ap.add_argument("--selftest", action="store_true")
    args = ap.parse_args()
    if args.selftest:
        import sys
        sys.exit(0 if selftest() else 1)
    groups = discover()
    if args.only:
        keep = set(args.only.split(","))
        groups = {k: v for k, v in groups.items() if k in keep}

    replicate_stories = {sid: dl for sid, dl in groups.items() if len(dl) >= 2}
    out = {"stories_total": len(groups),
           "stories_with_replicates": len(replicate_stories),
           "min_population_for_sigma_seat_verdict": {"stories": 3, "draws_per_story": 2},
           "per_story": {}, "within_vs_between": {}}

    print(f"=== COHORT STABILITY TABLE ===")
    print(f"stories discovered: {len(groups)}  | with >=2 draws (replicates): "
          f"{len(replicate_stories)}")
    print(f"PATTERN-5 GUARD ACTIVE: agreement-in-absence reported separately, never as "
          f"positive-stable.\n")

    loaded = {}  # sid -> [(draw, story_dict)]
    for sid, dl in replicate_stories.items():
        draws = [(idx, json.load(open(p))) for idx, p in dl]
        loaded[sid] = draws
        rows, seed_rows = build_story_table(draws)
        out["per_story"][sid] = {"n_draws": len(draws),
                                 "draw_indices": [i for i, _ in draws],
                                 "fields": rows, "seed_supplied": seed_rows}
        pos = [r["field"] for r in rows if r["status"] == "stable" and r["agreement_kind"] == "positive"]
        unstable = [r["field"] for r in rows if r["status"] == "unstable"]
        absence = [r["field"] for r in rows if r["agreement_kind"] == "absence"]
        print(f"--- {sid}  (draws {[i for i,_ in draws]}) ---")
        print(f"  positive-stable ({len(pos)}): {pos}")
        print(f"  UNSTABLE ({len(unstable)}): {unstable}")
        print(f"  agreement-in-absence ({len(absence)}, EXCLUDED from sigma/seat): {absence}")
        print(f"  seed-supplied (input echo, not evidence): "
              f"{[(r['field'], r['status']) for r in seed_rows]}\n")

    # within-vs-between distance
    within, between = [], []
    sids = list(loaded.keys())
    for sid in sids:
        ds = loaded[sid]
        for i in range(len(ds)):
            for j in range(i + 1, len(ds)):
                dist, nc, npos, nab = pair_distance(ds[i][1], ds[j][1])
                within.append({"story": sid, "pair": [ds[i][0], ds[j][0]],
                               "distance": dist, "n_comparable": nc,
                               "n_positive_agree": npos, "n_absence_agree": nab})
    for a in range(len(sids)):
        for b in range(a + 1, len(sids)):
            # use draw 1 of each (or first available) for the between baseline
            s1 = loaded[sids[a]][0][1]
            s2 = loaded[sids[b]][0][1]
            dist, nc, npos, nab = pair_distance(s1, s2)
            between.append({"stories": [sids[a], sids[b]],
                            "distance": dist, "n_comparable": nc,
                            "n_positive_agree": npos, "n_absence_agree": nab})

    out["within_vs_between"] = {"within_pairs": within, "between_pairs": between}
    print("=== WITHIN-vs-BETWEEN DISTANCE (Pattern-5: absence-agreement reported, not scored) ===")
    print(f"  within-draw pairs ({len(within)}):")
    for w in within:
        print(f"    {w['story']} draws {w['pair']}: dist={w['distance']:.3f} "
              f"(comparable={w['n_comparable']}, pos-agree={w['n_positive_agree']}, "
              f"absence-agree={w['n_absence_agree']})")
    if between:
        print(f"  between-story pairs ({len(between)}):")
        for b in between:
            print(f"    {b['stories']}: dist={b['distance']:.3f} "
                  f"(comparable={b['n_comparable']}, pos-agree={b['n_positive_agree']}, "
                  f"absence-agree={b['n_absence_agree']})")
    else:
        print("  between-story pairs: NONE — awaits >=2 replicate stories "
              "(within-vs-between separation is undefined at one story).")

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    (OUT_DIR / "stability_table.json").write_text(json.dumps(out, indent=2))
    print(f"\nwrote {OUT_DIR / 'stability_table.json'}")
    if len(replicate_stories) < 3:
        print(f"\nNOTE: {len(replicate_stories)} replicate story(ies) < 3 — this table is "
              f"DEGENERATE for the sigma/seat partition test. It witnesses the BUILDER, not a "
              f"verdict. cohort_sigma_seat_eval.py will refuse a verdict below min population.")


if __name__ == "__main__":
    main()
