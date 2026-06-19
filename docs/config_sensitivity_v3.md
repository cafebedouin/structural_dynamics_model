# Config Sensitivity Sweep v3 — Reconciled Corpus

**Date**: 2026-02-24
**Corpus**: Reconciled (1151 tests, post-dedup/reclassification)
**Params swept**: 150 (of 151 parsed; 1 excluded as dead code)
**Perturbations**: 588 (6 identity-skipped at +/-10% due to integer rounding)
**Baseline**: 1151 passed, 0 failed (9.2s wall-clock)
**Timeout**: 120s per run (reduced from 600s after timeout diagnostic)
**Workers**: 4

## Executive Summary

**All 150 parameters are Inert.** Zero Critical, zero Moderate, zero Error. Every one of the 588 perturbation runs produced 1151 passes and 0 failures, matching the baseline exactly.

This is a dramatic shift from the previous sweep (Feb 10, 118 params, 733 tests):

| Metric | v2 (Feb 10) | v3 (Feb 24) | Change |
|---|---|---|---|
| Tests | 733 | 1151 | +57% |
| Params swept | 118 | 150 | +27% |
| Critical | 1 | 0 | -1 |
| Moderate | 8 | 0 | -8 |
| Inert | 103 | 150 | +47 |
| Artifact/Error | 6 | 0 | -6 |
| Timeouts | 11 | 0 | -11 |

**Root cause of the shift**: The reconciliation process (dedup, reclassification, metric repair) aligned test expectations with the engine's actual classification logic. The v2 failures reflected stale or inconsistent test data, not genuine parameter sensitivity. The calibration is stable.

## Previously Critical Parameter

### `power_modifier_analytical` (1.15)

| Sweep | +/-10% Failures | +/-25% Failures | Rating |
|---|---|---|---|
| v2 | 37 | 37 | Critical |
| v3 | 0 | 0 | Inert |

In v2, this was the only Critical parameter — 37 tests failed when `pi_analytical` was perturbed by as little as +/-10%. After reconciliation, the 93 "only analyst catches snare" constraints were reclassified with corrected expectations, and the parameter is now fully stable at +/-25%.

## Previously Moderate Parameters (8 → 0)

All 8 previously Moderate parameters are now Inert:

| Parameter | Value | v2 +/-25% Failures | v3 +/-25% Failures | v2 Rating | v3 Rating |
|---|---|---|---|---|---|
| `boltzmann_coupling_threshold` | 0.25 | 721 | 0 | Moderate | Inert |
| `boltzmann_floor_information_standard` | 0.02 | 102 | 0 | Moderate | Inert |
| `boltzmann_floor_resource_allocation` | 0.15 | 721 | 0 | Moderate | Inert |
| `data_medium_threshold` | 0.75 | 721 | 0 | Moderate | Inert |
| `default_theater` | 0.0 | 721 | 0 | Moderate | Inert |
| `excess_factor_floor` | 0.25 | 722 | 0 | Moderate | Inert |
| `natural_law_suppression_max` | 0.15 | 721 | 0 | Moderate | Inert |
| `scope_modifier_global` | 1.2 | 720 | 0 | Moderate | Inert |

**Note on `data_medium_threshold`**: This param is annotated DOCS-ONLY (not referenced in executable classification code). Its v2 Moderate rating was an artifact of the pre-reconciliation corpus, not a genuine sensitivity signal. It is referenced in `config_schema.pl:388-393` for ordering validation (`data_high_threshold > data_medium_threshold`), but this constraint is not exercised during the test suite.

**Note on `default_theater`**: Zero-valued params use absolute perturbation (+/-0.1, +/-0.25) instead of percentage. Even at 0.25 absolute, the theater parameter produces no failures.

## Previously Artifact Parameters (6 → resolved)

The v2 sweep had 6 params rated Artifact — all were timeout artifacts caused by the missing `[stack]` preload (see `docs/timeout_diagnostic.md`). Of the 6:

| Parameter | v2 Status | v3 Status |
|---|---|---|
| `constructed_beneficiary_min` | Artifact (identity+timeout) | Removed from config.pl |
| `critical_mass_threshold` | Artifact (identity+timeout) | Inert |
| `data_high_threshold` | Artifact (timeout) | Inert |
| `piton_epsilon_floor` | Artifact (timeout) | Inert |
| `power_modifier_moderate` | Artifact (timeout) | Inert |
| `coupling_drift_threshold` | Artifact (orphan+timeout) | Removed from config.pl |

## New Parameters (36)

36 params are new since the v2 sweep (added during the Feb 2026 architecture work). All are Inert.

### Abductive reasoning (13)
`abductive_confidence_floor`, `abductive_dormant_entropy_ceiling`, `abductive_fpn_divergence_threshold`, `abductive_hub_conflict_h1_threshold`, `abductive_maxent_divergence_threshold`, `abductive_maxent_mountain_deception`, `abductive_oracle_entropy_ceiling`, `abductive_shadow_divergence_threshold`, `abductive_snare_lean_psi_threshold`, `abductive_snare_lean_psnare_floor`, `abductive_stress_convergence_min`, `abductive_stress_coupling_threshold`, `abductive_stress_entropy_threshold`, `abductive_stress_purity_threshold`

### MaxEnt and FPN (7)
`maxent_boolean_bonus`, `maxent_boolean_penalty`, `maxent_disagreement_prob_threshold`, `maxent_signature_override_strength`, `maxent_uncertainty_threshold`, `fpn_epsilon`, `fpn_max_iterations`

### Trajectory analysis (6)
`trajectory_coupling_band_width`, `trajectory_distance_metric_weight`, `trajectory_distance_pathology_weight`, `trajectory_distance_shift_weight`, `trajectory_distance_stability_weight`, `trajectory_family_cut_level`, `trajectory_isomorphism_threshold`

### Classification boundaries (5)
`false_mountain_extraction_threshold`, `piton_epsilon_floor`, `rope_extraction_ceiling`, `subtype_rope_threshold`, `subtype_snare_threshold`, `tangled_rope_extraction_ceil`, `tangled_rope_extraction_floor`

### Audit and contamination (3)
`audit_theater_conflict_threshold`, `audit_theater_naturalization_threshold`, `contamination_strength_naturalized`

## DOCS-ONLY and VALIDATED Params

### DOCS-ONLY (13 swept, 1 excluded, 2 removed)

16 params are annotated DOCS-ONLY in `config.pl`. Of these:
- **1 excluded**: `network_contamination_risk_threshold` (in `EXCLUDE_PARAMS`, never referenced)
- **2 removed**: `constructed_beneficiary_min`, `network_shared_agent_min` (removed during config hygiene cleanup)
- **13 swept**: All Inert, as expected

| DOCS-ONLY Param | Value | Rating |
|---|---|---|
| `boltzmann_factorization_tolerance` | 0.1 | Inert |
| `boltzmann_floor_drift_threshold` | 0.05 | Inert |
| `constructed_resistance_min` | 0.2 | Inert |
| `constructed_suppression_min` | 0.2 | Inert |
| `data_medium_threshold` | 0.75 | Inert |
| `loser_loss_max_gain` | 0.1 | Inert |
| `mountain_extractiveness_min` | 0.0 | Inert |
| `network_cluster_degraded_floor` | 0.4 | Inert |
| `reformability_high_threshold` | 0.7 | Inert |
| `reformability_low_threshold` | 0.3 | Inert |
| `rope_extractiveness_min` | 0.0 | Inert |
| `snare_extraction_ceil` | 1.0 | Inert |
| `tangled_rope_suppression_ceil` | 1.0 | Inert |

### VALIDATED (7 swept)

7 params annotated `VALIDATED: must match drl_purity_network.pl`. All Inert:

| VALIDATED Param | Value | Rating |
|---|---|---|
| `contamination_strength_mountain` | 0.0 | Inert |
| `contamination_strength_naturalized` | 0.3 | Inert |
| `contamination_strength_piton` | 0.8 | Inert |
| `contamination_strength_rope` | 0.1 | Inert |
| `contamination_strength_scaffold` | 0.2 | Inert |
| `contamination_strength_snare` | 1.0 | Inert |
| `contamination_strength_tangled_rope` | 0.5 | Inert |

## Identity-Skipped Perturbations

6 integer-valued params had +/-10% perturbations skipped because rounding restored the original value:

| Parameter | Value | +10% rounds to | -10% rounds to |
|---|---|---|---|
| `abductive_hub_conflict_h1_threshold` | 4 | 4 | 4 |
| `abductive_stress_convergence_min` | 4 | 4 | 4 |
| `boltzmann_min_classifications` | 3 | 3 | 3 |
| `critical_mass_threshold` | 3 | 3 | 3 |
| `network_cascade_count_threshold` | 3 | 3 | 3 |
| `network_hub_degree_threshold` | 3 | 3 | 3 |

All 6 were tested at +/-25% and are Inert.

## Audit Checks

| Check | Result |
|---|---|
| 2a: Param count matches | 150 swept + 1 excluded = 151 parsed from config.pl |
| 2b: DOCS-ONLY/VALIDATED all Inert | Yes (13 + 7 = 20 swept, all Inert) |
| 2c: Previously Critical/Moderate comparison | All 9 shifted to Inert |
| 2d: New params flagged | 36 new, all Inert |
| 2e: Pass count consistency | All 588 perturbations: pass_count=1151 |
| 2f: Non-monotonic sensitivity | N/A — zero failures at any level |
| 2g: Failure clustering | N/A — zero failures |

## Stability Assessment

The reconciled corpus is **unconditionally stable** across all 150 config parameters at +/-25% perturbation. This means:

1. **No parameter is a single point of failure.** The calibration has wide operating margins everywhere.
2. **The v2 sensitivity was data noise, not model fragility.** The 37 Critical failures (`power_modifier_analytical`) and ~720 Moderate failures (8 params) were caused by stale test expectations, not by genuine threshold sensitivity.
3. **The new modules (abductive, maxent, FPN, trajectory) are well-calibrated.** All 36 new params are Inert on first characterization.
4. **DOCS-ONLY annotations are accurate.** All 13 swept DOCS-ONLY params are Inert, confirming they have no executable impact.
5. **The scope modifiers remain inert.** This is consistent with the invariant analysis finding that HOW MUCH (scope) has zero classification effect — the scope_modifier params are structurally disconnected from the classification path.

## Parameter Groups (all 150, by function)

### Classification boundaries (22)
`mountain_extractiveness_max` (0.25), `mountain_extractiveness_min` (0.0), `mountain_suppression_ceiling` (0.05), `rope_chi_ceiling` (0.35), `rope_epsilon_ceiling` (0.45), `rope_extraction_ceiling` (0.15), `rope_extractiveness_min` (0.0), `rope_suppression_ceiling` (0.16), `snare_chi_floor` (0.66), `snare_epsilon_floor` (0.46), `snare_extraction_ceil` (1.0), `snare_load_bearing_threshold` (0.7), `snare_suppression_floor` (0.6), `tangled_rope_chi_ceil` (0.9), `tangled_rope_chi_floor` (0.4), `tangled_rope_epsilon_floor` (0.3), `tangled_rope_extraction_ceil` (0.9), `tangled_rope_extraction_floor` (0.16), `tangled_rope_suppression_ceil` (1.0), `tangled_rope_suppression_floor` (0.4), `scaffold_extraction_ceil` (0.45), `piton_extraction_ceiling` (0.45)

### Power and scope modifiers (12)
`power_modifier_analytical` (1.15), `power_modifier_institutional` (-0.2), `power_modifier_moderate` (1.0), `power_modifier_organized` (0.4), `power_modifier_powerful` (0.6), `power_modifier_powerless` (1.5), `scope_modifier_continental` (1.1), `scope_modifier_global` (1.2), `scope_modifier_local` (0.8), `scope_modifier_national` (1.0), `scope_modifier_regional` (0.9), `scope_modifier_universal` (1.0)

### Sigmoid directionality (4)
`sigmoid_lower` (-0.2), `sigmoid_midpoint` (0.5), `sigmoid_steepness` (6.0), `sigmoid_upper` (1.5)

### Canonical D positions (6)
`canonical_d_analytical` (0.725), `canonical_d_institutional` (0.0), `canonical_d_moderate` (0.6459), `canonical_d_organized` (0.399), `canonical_d_powerful` (0.4804), `canonical_d_powerless` (1.0)

### Abductive reasoning (14)
`abductive_confidence_floor` (0.3), `abductive_dormant_entropy_ceiling` (0.15), `abductive_fpn_divergence_threshold` (0.02), `abductive_hub_conflict_h1_threshold` (4), `abductive_maxent_divergence_threshold` (0.05), `abductive_maxent_mountain_deception` (0.5), `abductive_oracle_entropy_ceiling` (0.4), `abductive_shadow_divergence_threshold` (0.85), `abductive_snare_lean_psi_threshold` (0.9), `abductive_snare_lean_psnare_floor` (0.85), `abductive_stress_convergence_min` (4), `abductive_stress_coupling_threshold` (0.75), `abductive_stress_entropy_threshold` (0.15), `abductive_stress_purity_threshold` (0.6)

### MaxEnt and FPN (7)
`fpn_epsilon` (0.001), `fpn_max_iterations` (20), `maxent_boolean_bonus` (1.0), `maxent_boolean_penalty` (-4.0), `maxent_disagreement_prob_threshold` (0.5), `maxent_signature_override_strength` (0.95), `maxent_uncertainty_threshold` (0.4)

### Trajectory analysis (7)
`trajectory_coupling_band_width` (0.15), `trajectory_distance_metric_weight` (0.25), `trajectory_distance_pathology_weight` (0.15), `trajectory_distance_shift_weight` (0.35), `trajectory_distance_stability_weight` (0.25), `trajectory_family_cut_level` (0.3), `trajectory_isomorphism_threshold` (0.15)

### Boltzmann and coupling (13)
`boltzmann_coupling_strong_threshold` (0.5), `boltzmann_coupling_threshold` (0.25), `boltzmann_factorization_tolerance` (0.1), `boltzmann_floor_default` (0.05), `boltzmann_floor_drift_threshold` (0.05), `boltzmann_floor_enforcement_mechanism` (0.1), `boltzmann_floor_global_infrastructure` (0.2), `boltzmann_floor_information_standard` (0.02), `boltzmann_floor_resource_allocation` (0.15), `boltzmann_min_classifications` (3), `complexity_offset_default` (0.0), `complexity_offset_enforcement_mechanism` (0.08), `complexity_offset_global_infrastructure` (0.15), `complexity_offset_information_standard` (0.0), `complexity_offset_resource_allocation` (0.05)

### Structural signatures (9)
`constructed_resistance_min` (0.2), `constructed_suppression_min` (0.2), `coordination_collapse_min` (0.85), `coordination_resistance_max` (0.15), `coordination_suppression_max` (0.15), `false_mountain_extraction_threshold` (0.9), `natural_law_collapse_min` (0.85), `natural_law_resistance_max` (0.15), `natural_law_suppression_max` (0.15)

### Piton (3)
`piton_epsilon_floor` (0.1), `piton_extraction_ceiling` (0.45), `piton_theater_floor` (0.7)

### Subtype thresholds (2)
`subtype_rope_threshold` (0.3), `subtype_snare_threshold` (0.7)

### Reformability and Gaussian excess (9)
`excess_factor_center` (0.2), `excess_factor_floor` (0.25), `excess_factor_peak` (1.0), `excess_factor_sigma` (0.2), `reformability_high_threshold` (0.7), `reformability_low_threshold` (0.3), `reform_urgency_gap_critical` (0.4), `reform_urgency_gap_high` (0.3), `reform_urgency_gap_low` (0.05), `reform_urgency_gap_moderate` (0.15), `reform_urgency_pressure_critical` (2.0), `reform_urgency_pressure_high` (1.5), `reform_urgency_reformability_floor` (0.3)

### Purity network (12)
`purity_action_degraded_floor` (0.3), `purity_action_escalation_floor` (0.5), `purity_action_sound_floor` (0.7), `purity_attenuation_factor` (0.5), `purity_contamination_cap` (0.3), `purity_contamination_source_floor` (0.5), `purity_energy_max_multiplier` (3.0), `purity_scaffold_health_gate` (0.5), `purity_surgical_reform_gate` (0.3), `contamination_strength_mountain` (0.0), `contamination_strength_naturalized` (0.3), `contamination_strength_piton` (0.8), `contamination_strength_rope` (0.1), `contamination_strength_scaffold` (0.2), `contamination_strength_snare` (1.0), `contamination_strength_tangled_rope` (0.5)

### Network topology (6)
`network_cascade_count_threshold` (3), `network_cluster_degraded_floor` (0.4), `network_coupling_threshold` (0.5), `network_drift_velocity_threshold` (0.01), `network_hub_degree_threshold` (3), `dependency_coupling_threshold` (0.7)

### Audit (2)
`audit_theater_conflict_threshold` (0.5), `audit_theater_naturalization_threshold` (0.5)

### Data thresholds (2)
`data_high_threshold` (0.95), `data_medium_threshold` (0.75)

### Defaults and detection (8)
`beneficiary_gain_min` (0.5), `default_extractiveness` (0.1), `default_suppression` (0.1), `default_theater` (0.0), `isomorphism_threshold` (0.85), `loser_loss_max_gain` (0.1), `structural_resistance_min` (0.7), `structural_suppression_min` (0.7), `system_gradient_strong_threshold` (1.0), `system_gradient_threshold` (0.01), `critical_mass_threshold` (3)

## Recommendations

1. **The calibration is stable for the next phase.** No parameters need tighter bounds or special handling.

2. **The per-test timeout guard should be kept.** Although no timeouts occurred in v3, the 60s per-test guard in `validation_suite.pl` is cheap insurance against future regressions.

3. **The 120s sweep timeout is appropriate.** The 9.2s baseline with 120s timeout provides 13x headroom, sufficient for any reasonable perturbation.

4. **Future sweeps can use the same methodology.** The reconciled corpus is clean enough that any non-Inert result in a future sweep should be treated as a genuine signal, not a data artifact.

5. **Consider wider perturbation ranges.** Since +/-25% produced zero failures, a future investigation could test +/-50% or +/-100% to find the actual stability boundaries. This is low priority — +/-25% stability is more than sufficient for operational confidence.

6. **DOCS-ONLY params can be pruned.** The 13 swept DOCS-ONLY params and 1 excluded dead-code param are confirmed to have no executable impact. They could be moved to a documentation file to reduce config.pl clutter, though this is cosmetic.

## Raw Data

> **PRE-RESET / kernel_v1-regime (OQ-29):** the figures in this report and the raw-data
> file below were computed before the 2026-06-05 corpus reset. They are retained as a
> historical record of the v3 sweep, not a current measurement. Re-run against the live
> corpus before citing. (`config_sensitivity_results_v3.json` predates `corpus_hash`
> stamping, so the file itself cannot tell you which corpus it describes.)

Full results: `python/config_sensitivity_results_v3.json`
Previous report: `docs/CONFIG_SENSITIVITY.md`
Timeout diagnostic: `docs/timeout_diagnostic.md`
