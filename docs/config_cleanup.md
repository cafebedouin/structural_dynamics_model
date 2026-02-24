# Config Cleanup Report

Pre-sensitivity-sweep hygiene: contamination strength bypass fix, dead parameter removal, docs-only annotation.

## Part 1: Contamination Strength — Validation Approach

### Decision

**Keep hardcoded facts + startup validator.** Dynamic wiring rejected.

### Call-Site Analysis

11 call sites across 4 files:

| File | Call sites | First arg binding |
|---|---|---|
| `drl_purity_network.pl` | 2 (lines 219, 246) | Bound from `dr_type/3` |
| `network_dynamics.pl` | 2 (lines 107, 141) | Bound from `dr_type/3` |
| `drl_fpn.pl` | 2 (lines 230, 252) | Bound from `fpn_type_cache/3` or `dr_type/3` |
| `giant_component_analysis.pl` | 5 (lines 590, 891, 916, 1004, 1085) | Bound from `gc_node_type/3` |

All 11 sites have bound first argument. No enumeration calls. Dynamic `atom_concat → param/2` would work but rejected because:

1. **Catch-all safety**: `type_contamination_strength(_, 0.0)` provides 0.0 default for unknown types. Dynamic lookup would fail.
2. **Cut optimization**: `:-!` cuts enable first-argument indexing on a hot code path.
3. **Semantic fit**: These are architectural constants (type properties), not tunable thresholds.

### Implementation

- `config_validation.pl` §6: New `config_violation/1` clause checks each of 7 types. Uses `current_predicate(drl_purity_network:type_contamination_strength/2)` guard so it only fires during `validate_config_postcorpus` (after all modules loaded), not during `initialization(validate_config)`.
- `drl_purity_network.pl`: Cross-reference comment above `type_contamination_strength/2` explains why values are hardcoded and points to the validator.
- `config.pl`: 7 params annotated `% VALIDATED: must match drl_purity_network.pl`.
- `config_schema.pl`: 7 param_spec descriptions tagged `[validated-mirror]`.

### Sweep Implication

If the sensitivity sweep changes a `contamination_strength_*` param, the validator halts at startup with a clear error message. This forces the sweep operator to update both the config param AND the hardcoded predicate, making the change intentional.

## Part 2: Dead Parameters Removed

4 parameters with zero references outside config.pl and config_schema.pl:

| Parameter | Former line | Value |
|---|---|---|
| `temporal_metric_name` | config.pl:25 | `time_horizon` |
| `exit_metric_name` | config.pl:26 | `exit_options` |
| `power_metric_name` | config.pl:27 | `agent_power` |
| `scope_metric_name` | config.pl:28 | `spatial_scope` |

Removed from both `config.pl` and `config_schema.pl`. Section 1 "General Metric Naming" retains 3 active params: `suppression_metric_name`, `extractiveness_metric_name`, `theater_metric_name`.

## Part 3: Docs-Only Annotation Inventory

16 parameters annotated `% DOCS-ONLY: not referenced in executable code`:

| Parameter | Section | Value |
|---|---|---|
| `loser_loss_max_gain` | Intent | 0.10 |
| `data_medium_threshold` | Intent | 0.75 |
| `mountain_extractiveness_min` | DR Thresholds | 0.0 |
| `rope_extractiveness_min` | DR Thresholds | 0.0 |
| `tangled_rope_suppression_ceil` | DR Thresholds | 1.00 |
| `snare_extraction_ceil` | DR Thresholds | 1.00 |
| `constructed_suppression_min` | Structural Signature | 0.20 |
| `constructed_resistance_min` | Structural Signature | 0.20 |
| `constructed_beneficiary_min` | Structural Signature | 2 |
| `boltzmann_factorization_tolerance` | Boltzmann | 0.10 |
| `reformability_high_threshold` | Boltzmann | 0.70 |
| `reformability_low_threshold` | Boltzmann | 0.30 |
| `boltzmann_floor_drift_threshold` | Boltzmann | 0.05 |
| `network_shared_agent_min` | Purity Network | 1 |
| `network_contamination_risk_threshold` | Purity Network | 2 |
| `network_cluster_degraded_floor` | Purity Network | 0.40 |

7 parameters annotated `% VALIDATED: must match drl_purity_network.pl`:

| Parameter | Value |
|---|---|
| `contamination_strength_snare` | 1.0 |
| `contamination_strength_piton` | 0.8 |
| `contamination_strength_tangled_rope` | 0.5 |
| `contamination_strength_scaffold` | 0.2 |
| `contamination_strength_rope` | 0.1 |
| `contamination_strength_mountain` | 0.0 |
| `contamination_strength_naturalized` | 0.3 |

Matching `[docs-only]` and `[validated-mirror]` tags added to `config_schema.pl` description strings.

## Part 4: Pipeline Verification

```
Pipeline complete: 36/36 steps OK in 58.6s
```

- No config violation errors
- No `config_violations.log` generated
- All 36 pipeline steps passed

## Parameter Count Summary

| Category | Count |
|---|---|
| Total `param/2` facts in config.pl | 166 |
| Removed (dead) | 4 |
| Annotated DOCS-ONLY | 16 |
| Annotated VALIDATED (mirror) | 7 |
| Active runtime params | 143 |

Previous total: 170. After cleanup: 166 defined, 143 active.

## Files Modified

| File | Changes |
|---|---|
| `prolog/config.pl` | -4 dead params, +23 annotations |
| `prolog/config_schema.pl` | -4 param_spec entries, +23 description tags |
| `prolog/config_validation.pl` | +§6 contamination strength validator |
| `prolog/drl_purity_network.pl` | Expanded cross-reference comment |
