# OQ-106 retire — `structural_coercive_intent` top verdict deleted (destructive-replace witness)

**Date:** 2026-06-12. **Ruling:** operator, 2026-06-12 — option (i) of the web-review
exchange: capture-as-design ratified as the piton intension (the snare/piton split's
computed `constraint_captured/1` gate carries the designed/decayed axis; origin-intent is
not type-constitutive), so the retire executes as written and GAP-08's revival condition
stays **generic** (a consuming research question → fresh preregistration), not armed.

**Commitment + kill condition (recorded with the ruling):** capture-as-design-proxy is
falsified by a corpus case where proxy and intuition split — capture holds but the
extraction is plainly emergent with no design even ex post, or design is documented
(authored alternative-rejection evidence) but capture is absent, AND the operator wants
those cases sorted differently. Such a case becomes the consuming research question that
arms GAP-08's revival condition.

## What was deleted (baseline commit `f3f1e99f`)

- `prolog/intent_engine.pl`: the `structural_coercive_intent/4` clause (4-condition
  conjunction), `collect_intent_evidence/1` (sole reader of the five `intent_*` tables in
  this module), `refine_confidence/3`, and now-dead local helpers `average_list/2`,
  `max_by_value/2`. `classify_interval/3` keeps the lower verdicts
  (increasing/decreasing/stable/open) and the gradient-fact guard (pre-retire control
  flow preserved: no gradient fact ⇒ fail ⇒ analysis-failed report).
- `prolog/config.pl` + `prolog/config_schema.pl` (bijection-checked pair, deleted
  together): `system_gradient_strong_threshold`, `beneficiary_gain_min`,
  `structural_suppression_min`, `structural_resistance_min`, docs-only
  `loser_loss_max_gain`. All five had no consumer outside the deleted clause
  (witnessed: param-name grep over live prolog/python — hits only in historical sweep
  result JSONs and dated docs).

**NOT deleted:** the `intent_*` tables themselves, their `narrative_ontology`
declarations, `scenario_manager` retractalls, `data_verification` checks, and
`signature_detection:has_viable_alternatives/2` (load-bearing OQ-43-ruled fail-closed NL
gate). Those are GAP-08's declared-absence substrate.

## Why dead (evidence basis for the ruling)

1. Threshold range-dead by arithmetic: 1.00 strict vs 0.98 max reachable G_sys
   (`audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md` §C3).
2. Conditions 2–4 read tables with zero facts in every corpus ever generated (GAP-08).
3. No downstream consumer even if it fired: token absent from live prolog/python/docs
   outside `intent_engine.pl`; `report_generator.pl:22` imports intent_engine
   `except([classify_interval/3])` and substitutes its own pattern-only version.

## Witness (*prove before you replace*: a destructive replace owes the diff)

- `suite_baseline_f3f1e99f.txt` — full validation suite at baseline (exit 0, GOOD,
  1325 lines, 5 `[INTENT]` lines all OPEN/no_gradient_data).
- `suite_postretire_run1.txt` — same suite post-retire (exit 0, GOOD, 1325 lines).
- `diff_baseline_vs_postretire.txt` — only `[ELAPSED]` timing jitter and three
  "Previously defined at" warning-attribution lines differ. `[INTENT]` lines
  byte-identical; all lines excluding ELAPSED/warning-attribution byte-identical.
- `diff_samecode_run1_vs_run2.txt` — **positive control for the residue**: the same
  post-retire code run twice shows the same warning-attribution drift, establishing it
  as pre-existing run-noise (cohort-zero testset redefinition warnings, OQ-116
  territory), not an effect of the retire.
