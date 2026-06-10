# Imputation-Shim Blast-Radius Census

**Date:** 2026-06-09. **Trigger:** all five Polaris-run constraint reports open with
`[FIXED] Imputed 24–28 missing vectors using domain priors`; operator asked for the failure
class and a fix that does not paper over. **Companion OQ:** OQ-93 (ISSUES.md); adjudication
instance written into OQ-44.

## The class (one line, with witness)

**Unmigrated consumer contract**: the DR-AUDIT harness still enforces the archived prompt-era
data contract — 32 leveled grid points per interval,
`{accessibility_collapse, stakes_inflation, suppression, resistance} × {structural,
organizational, class, individual} × {T0, Tn}` (`data_repair.pl:274-275`,
`data_verification.pl:66-67`) — while the live generation contract
(`schemas/constraint_story_schema.json` `$defs/MeasurementMetric`) permits exactly
`{theater_ratio, base_extractiveness, suppression_requirement}`, unleveled. **The vocabulary
intersection is empty: 0 of 32 grid points are authorable by any schema-conforming story,
ever, corpus-wide.** `data_repair.pl` + `scenario_manager:inject_minimal_measurements` are the
shim converting the mismatch into `[FIXED]` since the JSON-template migration (sibling of
`mandatrophy_resolved`, OQ-83/A7: same migration event, producer side dangled there, consumer
side dangles here).

Witness for the provenance of the demand: `grep -rn stakes_inflation schemas/ prompts/ agent/
json/` hits ONLY `prompts/archives/` (`prompt_data.md:42`, `prompt_original.md:42`:
`% Metric ∈ {accessibility_collapse, stakes_inflation, suppression, resistance}`). Positive
control: `suppression_requirement` in the same surfaces fires in the live schema and prompt.

Not a generator bug (output conforms to its schema), not engine miswiring (the harness works
as designed for the v3.4 corpus), not a Polaris-run malfunction.

## Where the shim runs

`scenario_manager:load_and_run/2` (scenario_manager.pl:93-117) is the ONLY call site of
`data_repair:repair_interval/1` (positive control: `grep -rn "repair_interval(" prolog/*.pl`
→ definition sites in data_repair.pl + the one call at scenario_manager.pl:111; the
`use_module(data_repair)` imports in product_site_export.pl, json_report.pl, orbit_report.pl,
fingerprint_report.pl have **no call site** — vestigial). Two consumers of load_and_run:

1. **Per-constraint reports** — `enhanced_report.py:run_prolog_report` →
   `run_scenario('testsets/<id>.pl', '<id>')` (enhanced_report.py:104-130; stdout embedded
   raw in `outputs/constraint_reports/*_report.md`, no parsing of the `[FIXED]` line).
2. **Validation suite** — `validation_suite.pl:73` calls `load_and_run` per testset; the suite
   runs inside `run_pipeline.py:287`.

**Main classification pipeline is CLEAN**: `run_pipeline.py`'s export goals
(json_report/product_site_export via corpus_loader) never call repair, so
`pipeline_output.json` and all classifications are authored-fed.

## Injection mechanics (the 24-vs-28 split, and a latent bug)

`scenario_manager:inject_minimal_measurements/1` (scenario_manager.pl:124-131) asserts 8
`m_gen` facts — 4 structural-level metrics × **hardcoded times [0, 10]**, ignoring the
declared interval:

- Interval 0–10 (topology_selection, solar_integration_mechanism): all 8 land on grid slots →
  repair imputes **24**.
- Interval 0–50 / 0–20 (transfer_gap_physics, thermal_dissipation_constraint): only the 4
  facts at T=0 land on-grid → repair imputes **28**, and the 4 facts at T=10 sit as **stray
  mid-interval fabrications** that feed `coercion_gradient` (gradient computed between a
  category-prior constant and 0.5) and enter every unbound-metric time-point collection.

So the grid's three-bucket decomposition is: **authored 0 (always, see class statement) +
injected-0.5 (m_gen, 4 or 8) + imputed-from-priors (repair_m_*, 24 or 28)**. A binary
authored/imputed split would launder the injected bucket into "authored" (operator
correction, 2026-06-09).

## Consumer table (consumer → product → diet → recommendation)

| # | Consumer | Product | Diet | Recommendation |
|---|---|---|---|---|
| 1 | `data_verification:verify_interval_completeness` (test_harness Step 1 gate) | `[OK] Verification passed.` | **Imputed+injected — guaranteed pass.** Stage 2 certifies what Stage 1 manufactured; gate is a no-op post-repair (Pattern 5, purest form) | CARRY with provenance: gate line states post-repair status. Fail-closed deferred to OQ-44 ruling |
| 2 | `coercion_projection` (vector/magnitude/gradient) | κ magnitudes, gradients | **100% fabricated** (0/32 authorable) | Wire-or-gap adjudication (OQ-93). NOTE: unanswerable from existing reports — all prior-flavored; "wire" requires a prototype run with authored grid data first |
| 3 | `pattern_analysis` (system gradient, completeness, prelim pattern) | gradient ≈ 0 over constant priors; `compute_completeness` = N/8, **always 8/8 post-repair** | fabricated | Same as #2. Completeness score measures the imputer, not the data |
| 4 | `intent_engine:analyze_intent` → `[INTENT] Result: stable (Confidence: high)` | intent verdict per report | **Doubly vacuous**: Pattern from gradient over constant priors (only `stable` reachable); Conditions 2–4 read the `intent_*` family, empty corpus-wide (OQ-36/43); Confidence `high` = `DataScore ≥ DH` where DataScore is the manufactured 8/8 | Flag in-report (Phase 2). The INTENT subsystem has never computed on authored data — candidate gate-off, deferred to OQ-93 |
| 5 | `report_generator.pl:91` (Kappa display in full report) | per-level κ at Tn | fabricated | Flag in-report (Phase 2) |
| 6 | `logical_fingerprint:has_temporal_data` (:188), `structural_void(drifting_without_limit)` (:252) | fingerprint properties | **Mixed**: vacuously true for every constraint in repair paths (injected facts satisfy the existence check); authored-fed in the main pipeline (fingerprint_report runs without repair) | Note in OQ-93; no immediate change (main-pipeline outputs clean) |
| 7 | **MaxEnt classifier** (`get_constraint_metrics`, maxent_classifier.pl:250-254) | the 0.95–0.98 confidence figures, distributions, shadow types | **AUTHORED** (`base_extractiveness`, `get_raw_suppression`, theater `constraint_metric`) | CLEAN — see "Operator rider" below |
| 8 | `dr_type` / `validate_per_index` (declared-vs-computed mismatches) | INDEX OK/MISMATCH lines | authored scalars | CLEAN |
| 9 | `drl_lifecycle` drift, `drift_events`, `drl_composition` temporal series | drift reports, trajectories | authored (bound-atom metric queries; compound grid terms don't unify) | CLEAN. Minor hazard: unbound-metric time-point collectors (`temporal_residual:39`, `transition_paths:100`, `drl_composition:159`) would absorb grid timepoints if ever run post-repair; currently main-pipeline-only |
| 10 | `json_report:write_drift_trajectory` (:729 unbound-metric findall) | drift_trajectory in pipeline_output.json | authored in practice (runs pre-repair) | CLEAN; latent hazard if ever run post-repair — grid metrics would appear as series keys |
| 11 | `validation_suite` / `run_dynamic_suite` (run_pipeline.py:287 gate) | suite pass/fail | rides the shim (its verification gate passes by manufacture); its discriminating content (per-index validation, dr_type) is authored-fed | Note in OQ-93; pass-counts unaffected by Phase 2 |
| 12 | `data_verification:check_paired_measurements` | pairing INFO lines | authored vocabulary (`extractiveness`/`suppression_requirement`) | CLEAN |

## Operator rider answer (explicit, per 2026-06-09 ruling)

**The headline MaxEnt confidences are NOT grid-fed.** `get_constraint_metrics/4` reads authored
scalars only. The Claude-web caveat ("0.95–0.98 measures internal consistency over largely
invented vectors") was therefore **overstated for the MaxEnt figures** — the Polaris essay-side
residue (false-summit reading of the transfer gap, snare-to-individuals reading of the topology
choice) was filtered through a caveat stronger than the facts required, and survives a
weaker one. The caveat IS literally true for the `[INTENT]` line, the κ display, and the
`[OK] Verification passed.` gate line — all fabrication-fed. Re-adjudication of the essay
residue is the operator's, not this audit's.

## Witnesses in this directory

- `census.md` (this file).
- Witness commands and outputs are inline above (grep transcripts reproduced in the session
  log; key ones: the `stakes_inflation` archive-only grep + `suppression_requirement` positive
  control; the `repair_interval(` single-call-site grep; `MeasurementMetric` enum dump;
  scenario_manager.pl:124-131 hardcoded-times read; maxent_classifier.pl:250-254 read).
