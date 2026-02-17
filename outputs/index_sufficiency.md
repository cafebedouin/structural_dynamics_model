# Index Sufficiency Test

## Executive Summary

**Verdict:** MIXED — Mostly sufficient but classification failures need investigation.

### Collision Breakdown

| Category | Count | Rate |
|----------|------:|-----:|
| Classification failures (unknown/opaque) | 63 | 11.0% of collisions |
| Expected perspectival variance | 501 | 87.7% of collisions |
| Genuine collisions | 7 | 0.7% of constraints |
| **Total collisions** | **571** | across 1036 constraints |

- **Non-mountain anomaly rate:** 0.8% (7 of 908 non-mountain constraints)

### Data Quality: Domain Label Consolidation

The following domain labels were normalized at load time:

- logistics → logistical (1 constraints)
- mathematics → mathematical (2 constraints)
- physics → physical (3 constraints)
- psychology → psychological (1 constraints)
- technology → technological (1 constraints)

## Classification Failures

**63 collisions** involve `unknown` or `indexically_opaque` types — these represent real classification insufficiency.

| Constraint ID | Types | Domain | Signature |
|---------------|-------|--------|-----------|
| absorbing_markov_chain_trap | snare, unknown | technological | false_ci_rope |
| artificial_snow_2026 | snare, unknown | environmental | false_ci_rope |
| astm_d638_tensile_testing | snare, unknown | technological | false_ci_rope |
| big_data_astrophysics_arbitrage | tangled_rope, unknown | technological | false_ci_rope |
| burden_of_proof_legal_criminal | rope, unknown | political | false_ci_rope |
| cap_theorem | mountain, unknown | technological | natural_law |
| central_limit_theorem | mountain, unknown | mathematical | natural_law |
| clawderberg_recursive_slop | piton, unknown | technological | false_ci_rope |
| cma | tangled_rope, unknown | technological | false_ci_rope |
| college_admissions_market | tangled_rope, unknown | social | false_ci_rope |
| cost_of_observation | snare, unknown | philosophical | false_ci_rope |
| cs_ecmo_bridge | tangled_rope, unknown | technological | false_ci_rope |
| education_unbundling_implementation | tangled_rope, unknown | technological | false_ci_rope |
| emergency_oversight_bureau | tangled_rope, unknown | political | false_ci_rope |
| erasmus_rejoining_scaffold | scaffold, unknown | political | false_ci_rope |
| ergo_storage_rent | tangled_rope, unknown | technological | false_ci_rope |
| exoplanetary_habitability_arbitrage | rope, unknown | technological | false_ci_rope |
| factional_instability | tangled_rope, unknown | political | false_ci_rope |
| fiscal_equalization_friction | rope, unknown | economic | false_ci_rope |
| four_color_theorem_topological_bound | mountain, unknown | mathematical | natural_law |

*...and 43 more*

## Expected Perspectival Variance

**501 collisions** are explained by agent_power perspective shifts — the framework working as designed.

| Transition | Count |
|------------|------:|
| snare ↔ tangled_rope | 400 |
| rope ↔ tangled_rope | 27 |
| piton ↔ snare | 20 |
| mountain ↔ tangled_rope | 13 |
| piton ↔ tangled_rope | 11 |
| rope ↔ scaffold | 9 |
| mountain ↔ rope | 7 |
| piton ↔ rope | 6 |
| scaffold ↔ tangled_rope | 5 |
| mountain ↔ snare | 2 |

## Genuine Collisions

**7 collisions** remain unexplained — same index config produces different types without perspectival justification.

| Constraint ID | Index Config | Types | Domain | Purity |
|---------------|--------------|-------|--------|--------|
| biological_curiosity | ('institutional', 'generational', ' | rope, scaffold | biological | pristine |
| collective_stupidity_2026 | ('institutional', 'generational', ' | piton, rope | social | borderline |
| israeli_settlement_policy_authority_restriction | ('analytical', 'civilizational', 'a | snare, tangled_rope | political | contaminated |
| nsw_transmission_bottleneck | ('institutional', 'generational', ' | rope, scaffold | technological | contaminated |
| openbsd_netiquette_protocol | ('analytical', 'civilizational', 'a | snare, tangled_rope | technological | contaminated |
| shadow_fleet_sanctions_evasion | ('analytical', 'civilizational', 'a | snare, tangled_rope | geopolitical | contaminated |
| theatrical_neutrality | ('institutional', 'generational', ' | piton, rope | unknown | - |

## All Collision Patterns

**Total collisions:** 571 across 520 constraints

### Most Common Type Transitions

| Transition | Frequency |
|------------|----------:|
| snare ↔ tangled_rope | 403 |
| tangled_rope ↔ unknown | 29 |
| rope ↔ tangled_rope | 27 |
| piton ↔ snare | 20 |
| snare ↔ unknown | 15 |
| mountain ↔ tangled_rope | 13 |
| rope ↔ scaffold | 11 |
| piton ↔ tangled_rope | 11 |
| piton ↔ rope | 8 |
| mountain ↔ rope | 7 |

### Collisions by Domain

| Domain | Collisions | Constraints Affected |
|--------|----------:|--------------------:|
| technological | 147 | 138 |
| economic | 140 | 129 |
| political | 94 | 86 |
| social | 84 | 67 |
| geopolitical | 32 | 31 |
| biological | 11 | 9 |
| mathematical | 7 | 7 |
| organizational | 7 | 6 |
| scientific | 6 | 6 |
| philosophical | 5 | 4 |
| psychological | 5 | 5 |
| religious | 5 | 5 |
| legal | 4 | 4 |
| environmental | 2 | 2 |
| institutional | 2 | 2 |
| investigation | 2 | 1 |
| linguistic | 2 | 2 |
| logistical | 2 | 2 |
| military | 2 | 2 |
| astrophysical | 1 | 1 |
| bio_industrial | 1 | 1 |
| cognitive | 1 | 1 |
| corporate_governance | 1 | 1 |
| health | 1 | 1 |
| informational | 1 | 1 |
| logical | 1 | 1 |
| magical | 1 | 1 |
| medical | 1 | 1 |
| physical | 1 | 1 |
| statistical | 1 | 1 |
| unknown | 1 | 1 |

## Domain Sufficiency Analysis

How well do indices explain variance in each domain?

| Domain | Constraints | Avg Variance | Sufficiency |
|--------|----------:|-------------:|:-----------:|
| investigation | 2 | 0.70 | Medium |
| institutional | 3 | 0.61 | Medium |
| bio_industrial | 1 | 0.60 | Medium |
| military | 2 | 0.58 | Medium |
| logistical | 2 | 0.57 | Medium |
| socio_political | 1 | 0.57 | Medium |
| medical | 4 | 0.56 | Medium |
| environmental | 6 | 0.55 | Medium |
| social | 133 | 0.54 | Medium |
| cognitive | 4 | 0.54 | Medium |
| informational | 2 | 0.54 | Medium |
| organizational | 9 | 0.53 | Medium |
| religious | 9 | 0.52 | Medium |
| economic | 210 | 0.52 | Medium |
| philosophical | 11 | 0.51 | Medium |
| psychological | 10 | 0.51 | Medium |
| biological | 16 | 0.50 | Medium |
| corporate_governance | 1 | 0.50 | Medium |
| infrastructure | 1 | 0.50 | Medium |
| political | 154 | 0.50 | Medium |
| technological | 282 | 0.49 | Medium |
| legal | 11 | 0.47 | Medium |
| health | 5 | 0.47 | Medium |
| geopolitical | 48 | 0.44 | Medium |
| unknown | 5 | 0.42 | Medium |
| linguistic | 3 | 0.41 | Medium |
| scientific | 19 | 0.39 | Low |
| analytical | 1 | 0.38 | Low |
| systems_engineering | 1 | 0.38 | Low |
| physical | 6 | 0.37 | Low |
| ecological | 2 | 0.36 | Low |
| magical | 1 | 0.33 | Low |
| epistemological | 2 | 0.33 | Low |
| astrophysical | 1 | 0.33 | Low |
| logical | 1 | 0.33 | Low |
| atmospheric_science | 1 | 0.33 | Low |
| mathematical | 63 | 0.32 | Low |
| logic | 1 | 0.29 | Low |
| statistical | 1 | 0.29 | Low |
| sociological | 1 | 0.29 | Low |

**Note:** Higher variance = indices capture more differences (good)

## Stability Anomalies (Mountains Excluded)

Non-mountain constraints tested across 6+ index configs that always produce the same type.
These may indicate misclassification (mountains are excluded since they SHOULD be invariant).

| Constraint ID | Configs | Type | Domain | Notes |
|---------------|--------:|------|--------|-------|
| copyleft_viral_licensing | 7 | rope | technological | enforced |
| microrobot_manipulation | 7 | rope | technological | enforced |
| vertebrate_turning_point_2026 | 7 | rope | biological | enforced |
| wikipedia_noncommercial_model | 7 | rope | technological | enforced |
| cuny_light_2026 | 6 | rope | technological | enforced |
| perseverance_rover_autonomy | 6 | rope | technological | enforced |
| rare_earth_coop_2026 | 6 | rope | economic | enforced |

## Recommendations

1. **Classification failures (11.0%):** Investigate specific index configurations producing `unknown` types. These represent real gaps in the classification engine.

2. **Data quality:** Domain label normalization consolidated 8 constraints across 5 variant labels. Consider standardizing domain labels upstream in the test corpus.

3. **Perspectival health:** 87.7% of collisions are expected perspectival variance (agent_power driving type shifts). This confirms the index system is capturing real structural dynamics.

