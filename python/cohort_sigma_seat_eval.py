#!/usr/bin/env python3
"""σ/seat falsifier evaluator (OQ-109 Phase C, item 4 / SIGMA_SEAT_PREDICTION.md).

Two responsibilities, sharply separated by the spend gate:

PRE-SPEND (always available): a PARSE-CHECK witness — reproduce the frozen bucket assignment from
audits/2026-06-12_cohort_zero/SIGMA_SEAT_PREDICTION.md and confirm the live instrument
(cohort_stability.FIELDS) has not drifted from it. That is the whole pre-spend deliverable.

POST-SPEND (gated on the replicate set landing): the actual partition test — does the observed
draw-stability partition match the predicted σ/seat split, on non-seed-supplied, non-known,
in-scope fields. At n=1 replicate story the between-story half does not exist and the partition has
no denominator, so THE EVALUATOR REFUSES TO EMIT A VERDICT below the minimum population
(>=3 stories x >=2 draws). Printing "insufficient power" with a number attached would manufacture a
verification-shaped token where nothing was tested (operator ruling, 2026-06-12) — so it returns
"NO TEST / gated", not a degenerate verdict.

Usage:
  python3 python/cohort_sigma_seat_eval.py              # parse-check + population gate
  python3 python/cohort_sigma_seat_eval.py --parse-check-only
"""
import argparse
import json
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
PREDICTION_MD = REPO / "audits/2026-06-12_cohort_zero/SIGMA_SEAT_PREDICTION.md"
STABILITY_JSON = REPO / "audits/2026-06-12_cohort_zero/stability_table.json"
sys.path.insert(0, str(REPO / "python"))
from cohort_stability import FIELDS  # noqa: E402  (source of truth for the live buckets)

MIN_STORIES = 3
MIN_DRAWS = 2

# Explicit map: live instrument field key -> the md prediction-table row label that covers it.
# Compound md rows (e.g. one row for power/time_horizon/exit_options/spatial_scope) cover several
# instrument keys; this mapping is code-visible so a drift is a diff, not a silent rebucket.
# Values are the NORMALIZED md label-cell text (backticks stripped, whitespace collapsed) —
# see _norm_label. Compound md rows cover several instrument keys; mapping is code-visible so a
# real drift is a diff, not a silent rebucket.
FIELD_TO_MD = {
    "base_properties.suppression": "base_properties.suppression",
    "base_properties.theater_ratio": "base_properties.theater_ratio",
    "base_properties.accessibility_collapse": "base_properties.accessibility_collapse",
    "base_properties.resistance": "base_properties.resistance",
    "base_properties.extractiveness": "base_properties.extractiveness",
    "base_properties.requires_active_enforcement": "base_properties.requires_active_enforcement",
    "base_properties.emerges_naturally": "base_properties.emerges_naturally",
    "base_properties.has_sunset_clause": "base_properties.has_sunset_clause",
    "base_properties.beneficiaries": "base_properties.beneficiaries[] / victims[]",
    "base_properties.victims": "base_properties.beneficiaries[] / victims[]",
    "base_properties.vindicated_propositions": "base_properties.vindicated_propositions[]",
    "base_properties.claimed_type": "base_properties.claimed_type",
    "stakeholders.roster_card": "stakeholders[] roster + role (+ secondary_role, agent)",
    "stakeholders.role_multiset": "stakeholders[] roster + role (+ secondary_role, agent)",
    "stakeholders.power_multiset": "stakeholders[].power/time_horizon/exit_options/spatial_scope",
    "stakeholders.time_horizon_multiset": "stakeholders[].power/time_horizon/exit_options/spatial_scope",
    "stakeholders.exit_options_multiset": "stakeholders[].power/time_horizon/exit_options/spatial_scope",
    "stakeholders.spatial_scope_multiset": "stakeholders[].power/time_horizon/exit_options/spatial_scope",
    "six_questions.coordination_function": "six_questions.coordination_function / transfer_function",
    "six_questions.transfer_function": "six_questions.coordination_function / transfer_function",
    "six_questions.absent_voices": "six_questions.absent_voices",
    "six_questions.disappearance_verdict": "six_questions.disappearance_verdict",
    "six_questions.founding_problem": "six_questions.founding_problem",
    "six_questions.founding_problem_status": "six_questions.founding_problem_status",
    "six_questions.founding_problem_corroboration": "six_questions.founding_problem_corroboration",
    "gain_flow": "gain_flow",
    "fixing_cost": "fixing_cost",
    "omegas.count": "omegas[] (count + ids + type_class)",
    "omegas.id_set": "omegas[] (count + ids + type_class)",
    "measurements.count": "measurements[] (presence, metric selection, grid shape, point count)",
    "measurements.metric_set": "measurements[] (presence, metric selection, grid shape, point count)",
    "measurements.grid_shape": "measurements[] (presence, metric selection, grid shape, point count)",
    "interval": "interval",
    "boltzmann.presence": "boltzmann / network / directionality_overrides / cs_structure / coercion_grid (presence)",
    "network.presence": "boltzmann / network / directionality_overrides / cs_structure / coercion_grid (presence)",
    "directionality_overrides.presence": "boltzmann / network / directionality_overrides / cs_structure / coercion_grid (presence)",
    "cs_structure.presence": "boltzmann / network / directionality_overrides / cs_structure / coercion_grid (presence)",
    "coercion_grid.presence": "boltzmann / network / directionality_overrides / cs_structure / coercion_grid (presence)",
}


def _norm_label(cell):
    return re.sub(r"\s+", " ", cell.replace("`", "")).strip()


def parse_prediction_md():
    """Returns {normalized_label: {'bucket','known'}} from the frozen table.
    Splits on table cells (not first-backtick) so compound multi-backtick rows parse."""
    out = {}
    for line in PREDICTION_MD.read_text().splitlines():
        if not line.lstrip().startswith("|"):
            continue
        parts = [p.strip() for p in line.strip().strip("|").split("|")]
        if len(parts) < 3:
            continue
        label_cell, bucket_cell = parts[0], parts[1]
        label = _norm_label(label_cell)
        if not label or label.lower() == "field" or set(label_cell) <= set("-: "):
            continue  # header / separator row
        if "σ" in bucket_cell and ("seat" not in bucket_cell
                                   or bucket_cell.index("σ") < bucket_cell.index("seat")):
            bucket = "sigma"
        elif "seat" in bucket_cell:
            bucket = "seat"
        else:
            bucket = "?"
        known = "KNOWN-IN-ADVANCE" in bucket_cell
        out[label] = {"bucket": bucket, "known": known}
    return out


def parse_check():
    md = parse_prediction_md()
    print("=== PARSE-CHECK: frozen prediction table (SIGMA_SEAT_PREDICTION.md) ===")
    for label, info in md.items():
        print(f"  [{info['bucket']:5}]{' KNOWN' if info['known'] else '      '}  {label}")
    print(f"\n=== INSTRUMENT-vs-FROZEN drift check (cohort_stability.FIELDS) ===")
    ok = True
    for key, _ex, bucket, flags in FIELDS:
        md_label = FIELD_TO_MD.get(key)
        if md_label is None or md_label not in md:
            ok = False
            print(f"  DRIFT: {key} -> no covering md row ({md_label!r})")
            continue
        md_bucket = md[md_label]["bucket"]
        if md_bucket != bucket:
            ok = False
            print(f"  DRIFT: {key} instrument={bucket} but frozen={md_bucket} ({md_label})")
        md_known = md[md_label]["known"]
        inst_known = "known" in flags
        if md_known != inst_known:
            ok = False
            print(f"  DRIFT(known-flag): {key} instrument={inst_known} frozen={md_known}")
    print("  PARSE-CHECK PASS — instrument buckets reproduce the frozen prediction"
          if ok else "  PARSE-CHECK FAIL — instrument drifted from the frozen prediction")
    return ok


def population_gate():
    if not STABILITY_JSON.exists():
        print(f"\n=== POPULATION GATE ===\n  no stability_table.json — run cohort_stability.py first.")
        return False
    tbl = json.load(open(STABILITY_JSON))
    n_stories = tbl.get("stories_with_replicates", 0)
    n_ok = sum(1 for s in tbl.get("per_story", {}).values() if s.get("n_draws", 0) >= MIN_DRAWS)
    print(f"\n=== POPULATION GATE (min: {MIN_STORIES} stories x {MIN_DRAWS} draws) ===")
    print(f"  replicate stories with >={MIN_DRAWS} draws: {n_ok}")
    if n_ok < MIN_STORIES:
        print(f"  *** NO TEST — gated. {n_ok} < {MIN_STORIES} stories. ***")
        print(f"  The σ/seat partition test has no between-story denominator below minimum")
        print(f"  population; emitting a verdict here would be a counterfeit witness. The σ/seat")
        print(f"  line is OPEN (partial-with-named-residual) until the replicate spend lands.")
        return False
    print(f"  population sufficient — running the partition test.")
    return True


def run_verdict():
    """Post-spend partition test. Only reached when population_gate() passes."""
    tbl = json.load(open(STABILITY_JSON))
    md = parse_prediction_md()
    # contingency over non-seed, non-known, in-scope fields, aggregated across stories
    cells = {("sigma", "stable"): 0, ("sigma", "unstable"): 0,
             ("seat", "stable"): 0, ("seat", "unstable"): 0}
    known_rows = []
    for sid, sdata in tbl["per_story"].items():
        for r in sdata["fields"]:
            if "known" in r["flags"]:
                known_rows.append((sid, r["field"], r["status"]))
                continue
            if r["agreement_kind"] == "absence":
                continue  # Pattern-5: absence carries no σ/seat evidence
            cells[(r["predicted_bucket"], r["status"])] = \
                cells.get((r["predicted_bucket"], r["status"]), 0) + 1
    print("\n=== σ/seat PARTITION (non-seed, non-known, in-scope; absence excluded) ===")
    print(f"  predicted-σ   : stable(=σ-side) {cells[('sigma','stable')]}  "
          f"unstable(=seat-side) {cells[('sigma','unstable')]}")
    print(f"  predicted-seat: stable        {cells[('seat','stable')]}  "
          f"unstable             {cells[('seat','unstable')]}")
    print(f"  KNOWN-IN-ADVANCE (reported, NO blind credit): {known_rows}")
    print("  NOTE: match-beyond-chance test (Fisher/χ²) + per-field seat-boundary findings to be")
    print("  computed here when this path is first exercised post-spend.")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--parse-check-only", action="store_true")
    args = ap.parse_args()
    pc = parse_check()
    if args.parse_check_only:
        sys.exit(0 if pc else 1)
    if population_gate():
        run_verdict()


if __name__ == "__main__":
    main()
