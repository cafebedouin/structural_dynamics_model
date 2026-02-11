# Config Sensitivity Sweep Results

## Overview

- **Date**: 2026-02-10
- **Baseline**: 733 tests passed, 0 failed
- **Methodology**: Each of 118 numeric `param/2` facts in `prolog/config.pl` was perturbed at ±10% and ±25% of its current value. For zero-valued params, perturbation uses an absolute delta equal to the percentage level. Each perturbation ran the full 733-test validation suite via a Prolog retract/asserta overlay.
- **Total runs**: 472 (118 params × 4 perturbations)
- **Workers**: 4 parallel, 600s timeout per run
- **Summary**: 7 Critical, 8 Moderate, 103 Inert

## Critical Parameters

Do not modify without full test suite validation.

| Parameter | Current | ±10% Failures | Explanation |
|-----------|---------|---------------|-------------|
| `power_modifier_moderate` | 1.0 | 720 | Baseline power multiplier — all chi calculations are scaled relative to this value, so any deviation cascades through every classification |
| `data_high_threshold` | 0.95 | 721 | High-confidence data integrity gate — lowering to 0.855 causes nearly all test cases to fail quality filtering |
| `piton_epsilon_floor` | 0.10 | 721 | Piton classification requires ε > 0.10 (Rule Z); raising to 0.11 excludes test cases sitting exactly at the boundary |
| `coupling_drift_threshold` | 0.10 | 720 | Minimum coupling change to register drift — lowering to 0.09 triggers false drift events across the corpus |
| `constructed_beneficiary_min` | 2 | 722 | Integer param where ±10% crosses a discrete boundary (2 → round(1.8) = 2, but the perturbation arithmetic yields 1.8 which Prolog treats as non-integer, breaking the ≥ 2 integer comparison) |
| `critical_mass_threshold` | 3 | 720 | Integer param where ±10% crosses a discrete boundary (3 → 2.7 as float disrupts the integer coalition-count comparison) |
| `power_modifier_analytical` | 1.15 | 37 | Analytical clarity multiplier — 37 failures (not catastrophic) indicate a subset of test cases with agents near the analytical/moderate boundary |

Note: `critical_mass_threshold` and `constructed_beneficiary_min` are integer params where ±10% perturbation produces a float that crosses a discrete comparison boundary. Their "Critical" rating reflects arithmetic rounding behavior, not calibration fragility. These params are stable at their integer values.

## Moderate Parameters

Safe at ±10%, failures appear at ±25%. These have adequate margin for current calibration but should not be moved aggressively.

| Parameter | Current | Safe Range | ±25% Failures | Explanation |
|-----------|---------|------------|---------------|-------------|
| `boltzmann_coupling_threshold` | 0.25 | ~0.22–0.31 | 721 | Primary coupling/factorization gate — floor is ~0.19-0.22 based on -25% break at 0.1875. Stable at ±10%. Current value has 10-25% downward margin before classifications shift. |
| `boltzmann_floor_information_standard` | 0.02 | ~0.015–0.025 | 102 | Soft degradation — only 102/733 failures at -25% (0.015), indicating a subset of information-standard test cases near the floor |
| `boltzmann_floor_resource_allocation` | 0.15 | ~0.135–0.175 | 721 | Raising to 0.1875 pushes the "necessary cost" floor above actual extraction values in resource-allocation test cases |
| `data_medium_threshold` | 0.75 | ~0.675–0.9375 | 721 | Medium confidence gate — breaks when lowered to 0.5625 at -25% |
| `default_theater` | 0.0 | 0.0 only | 721 | Zero-valued param where +25% perturbation sets theater to 0.25, which crosses the piton theater gate and misclassifies non-theater constraints |
| `excess_factor_floor` | 0.25 | ~0.225–0.28 | 722 | Gaussian reformability floor asymptote — raising to 0.3125 compresses the Gaussian curve's dynamic range, breaking reformability scoring |
| `natural_law_suppression_max` | 0.15 | ~0.125–0.19 | 721 | Natural law signature ceiling — lowering to 0.1125 excludes test cases with suppression scores near the boundary |
| `scope_modifier_global` | 1.2 | ~1.08–1.44 | 720 | Global scope amplification factor — at -25% (0.9) it falls below national baseline (1.0), inverting the verification-difficulty ordering |

## Inert Parameters

Stable at ±25% perturbation. "Inert" means the test suite does not distinguish perturbations at this scale — not that the parameter has no effect. These params either have wide operating margins or are tested only indirectly by the current suite.

### Sigmoid directionality (4)
`sigmoid_lower`, `sigmoid_midpoint`, `sigmoid_steepness`, `sigmoid_upper`

### Canonical D positions (6)
`canonical_d_analytical`, `canonical_d_institutional`, `canonical_d_moderate`, `canonical_d_organized`, `canonical_d_powerful`, `canonical_d_powerless`

### Power modifiers (4)
`power_modifier_institutional`, `power_modifier_organized`, `power_modifier_powerful`, `power_modifier_powerless`

### Scope modifiers (5)
`scope_modifier_continental`, `scope_modifier_local`, `scope_modifier_national`, `scope_modifier_regional`, `scope_modifier_universal`

### Classification boundaries — Mountain (3)
`mountain_extractiveness_max`, `mountain_extractiveness_min`, `mountain_suppression_ceiling`

### Classification boundaries — Rope (4)
`rope_chi_ceiling`, `rope_epsilon_ceiling`, `rope_extractiveness_min`, `rope_suppression_ceiling`

### Classification boundaries — Snare (5)
`snare_chi_floor`, `snare_epsilon_floor`, `snare_extraction_ceil`, `snare_load_bearing_threshold`, `snare_suppression_floor`

### Classification boundaries — Tangled Rope (5)
`tangled_rope_chi_ceil`, `tangled_rope_chi_floor`, `tangled_rope_epsilon_floor`, `tangled_rope_suppression_ceil`, `tangled_rope_suppression_floor`

### Classification boundaries — Scaffold (1)
`scaffold_extraction_ceil`

### Classification boundaries — Piton (2)
`piton_extraction_ceiling`, `piton_theater_floor`

### Structural signatures (7)
`constructed_resistance_min`, `constructed_suppression_min`, `coordination_collapse_min`, `coordination_resistance_max`, `coordination_suppression_max`, `natural_law_collapse_min`, `natural_law_resistance_max`

### Boltzmann and coupling (11)
`boltzmann_coupling_strong_threshold`, `boltzmann_factorization_tolerance`, `boltzmann_floor_default`, `boltzmann_floor_enforcement_mechanism`, `boltzmann_floor_global_infrastructure`, `boltzmann_min_classifications`, `complexity_offset_default`, `complexity_offset_enforcement_mechanism`, `complexity_offset_global_infrastructure`, `complexity_offset_information_standard`, `complexity_offset_resource_allocation`

### Reformability and Gaussian (5)
`excess_factor_center`, `excess_factor_peak`, `excess_factor_sigma`, `reformability_high_threshold`, `reformability_low_threshold`

### Reform urgency (7)
`reform_urgency_gap_critical`, `reform_urgency_gap_high`, `reform_urgency_gap_low`, `reform_urgency_gap_moderate`, `reform_urgency_pressure_critical`, `reform_urgency_pressure_high`, `reform_urgency_reformability_floor`

### Drift detection (1)
`boltzmann_floor_drift_threshold`

### Purity (9)
`purity_action_degraded_floor`, `purity_action_escalation_floor`, `purity_action_sound_floor`, `purity_attenuation_factor`, `purity_contamination_cap`, `purity_contamination_source_floor`, `purity_energy_max_multiplier`, `purity_scaffold_health_gate`, `purity_surgical_reform_gate`

### Network and contamination (14)
`contamination_strength_mountain`, `contamination_strength_piton`, `contamination_strength_rope`, `contamination_strength_scaffold`, `contamination_strength_snare`, `contamination_strength_tangled_rope`, `network_cascade_count_threshold`, `network_cluster_degraded_floor`, `network_contamination_risk_threshold`, `network_coupling_threshold`, `network_drift_hub_escalation`, `network_drift_velocity_threshold`, `network_hub_degree_threshold`, `network_shared_agent_min`

### Defaults (2)
`default_extractiveness`, `default_suppression`

### Intent and detection (6)
`beneficiary_gain_min`, `loser_loss_max_gain`, `structural_resistance_min`, `structural_suppression_min`, `system_gradient_strong_threshold`, `system_gradient_threshold`

### Other (2)
`dependency_coupling_threshold`, `isomorphism_threshold`

## Timeouts

11 runs timed out at the 600s limit. Timeout runs produce 0 pass / 0 fail, so the param's rating reflects only its non-timeout perturbations.

| Parameter | Perturbation | Perturbed Value | Notes |
|-----------|-------------|-----------------|-------|
| `sigmoid_midpoint` | -25% | 0.375 | Potential infinite loop — sigmoid inflection shift may cause non-terminating convergence |
| `snare_load_bearing_threshold` | -25% | 0.525 | Potential infinite loop — lowered threshold may trigger recursive load-bearing reclassification |
| `canonical_d_organized` | +25% | 0.49875 | |
| `system_gradient_strong_threshold` | -25% | 0.75 | |
| `natural_law_resistance_max` | +25% | 0.1875 | |
| `excess_factor_center` | -10% | 0.18 | |
| `dependency_coupling_threshold` | +25% | 0.875 | |
| `reform_urgency_pressure_high` | +10% | 1.65 | |
| `purity_action_degraded_floor` | -10% | 0.27 | |
| `network_contamination_risk_threshold` | -10% | 2.0 | Integer param — perturbed to float 1.8, same discrete-boundary issue |
| `network_drift_velocity_threshold` | -25% | 0.0075 | |

`sigmoid_midpoint` -25% and `snare_load_bearing_threshold` -25% are the highest-priority timeout investigations: both involve feedback loops (sigmoid convergence and load-bearing reclassification) that may not terminate under perturbation.

## Methodology Notes

- Perturbation uses a Prolog `retract/asserta` overlay file — `config.pl` is never modified during the sweep.
- Each run executes the full validation suite (733 tests) in an isolated SWI-Prolog process.
- For zero-valued params, perturbation uses an absolute delta (e.g., ±0.10, ±0.25) since percentage of zero is zero.
- Raw results including per-perturbation pass/fail counts are stored in `python/config_sensitivity_results.json`.
- Rerun with: `python3 python/config_sensitivity_sweep.py --workers 4`
