# Index Sufficiency Test

## Executive Summary

**Verdict:** SUFFICIENT — Indices explain variance well. High perspectival rate confirms agent_power is working as designed.

### Collision Breakdown

| Category | Count | Rate |
|----------|------:|-----:|
| Classification failures (unknown/opaque) | 1 | 0.2% of collisions |
| Expected perspectival variance | 527 | 98.3% of collisions |
| Genuine collisions | 8 | 0.8% of constraints |
| **Total collisions** | **536** | across 1036 constraints |

- **Non-mountain anomaly rate:** 0.8% (7 of 908 non-mountain constraints)

### Data Quality: Domain Label Consolidation

The following domain labels were normalized at load time:

- logistics → logistical (1 constraints)
- mathematics → mathematical (2 constraints)
- physics → physical (3 constraints)
- psychology → psychological (1 constraints)
- technology → technological (1 constraints)

## Classification Failures

**1 collisions** involve `unknown` or `indexically_opaque` types — these represent real classification insufficiency.

| Constraint ID | Types | Domain | Signature |
|---------------|-------|--------|-----------|
| trump_epa_greenhouse_gas_reversal | indexically_opaque, rope | political | false_ci_rope |

## Expected Perspectival Variance

**527 collisions** are explained by agent_power perspective shifts — the framework working as designed.

| Transition | Count |
|------------|------:|
| snare ↔ tangled_rope | 414 |
| rope ↔ tangled_rope | 33 |
| piton ↔ snare | 20 |
| piton ↔ tangled_rope | 15 |
| mountain ↔ tangled_rope | 13 |
| rope ↔ scaffold | 9 |
| scaffold ↔ tangled_rope | 7 |
| mountain ↔ rope | 7 |
| piton ↔ rope | 6 |
| mountain ↔ snare | 2 |

## Genuine Collisions

**8 collisions** remain unexplained — same index config produces different types without perspectival justification.

| Constraint ID | Index Config | Types | Domain | Purity |
|---------------|--------------|-------|--------|--------|
| astm_d638_tensile_testing | ('powerless', 'biographical', 'trap | snare, tangled_rope | technological | sound |
| biological_curiosity | ('institutional', 'generational', ' | rope, scaffold | biological | pristine |
| collective_stupidity_2026 | ('institutional', 'generational', ' | piton, rope | social | borderline |
| israeli_settlement_policy_authority_restriction | ('analytical', 'civilizational', 'a | snare, tangled_rope | political | contaminated |
| nsw_transmission_bottleneck | ('institutional', 'generational', ' | rope, scaffold | technological | contaminated |
| openbsd_netiquette_protocol | ('analytical', 'civilizational', 'a | snare, tangled_rope | technological | contaminated |
| shadow_fleet_sanctions_evasion | ('analytical', 'civilizational', 'a | snare, tangled_rope | geopolitical | contaminated |
| theatrical_neutrality | ('institutional', 'generational', ' | piton, rope | unknown | - |

## All Collision Patterns

**Total collisions:** 536 across 492 constraints

### Most Common Type Transitions

| Transition | Frequency |
|------------|----------:|
| snare ↔ tangled_rope | 418 |
| rope ↔ tangled_rope | 33 |
| piton ↔ snare | 20 |
| piton ↔ tangled_rope | 15 |
| mountain ↔ tangled_rope | 13 |
| rope ↔ scaffold | 11 |
| piton ↔ rope | 8 |
| scaffold ↔ tangled_rope | 7 |
| mountain ↔ rope | 7 |
| mountain ↔ snare | 2 |

### Collisions by Domain

| Domain | Collisions | Constraints Affected |
|--------|----------:|--------------------:|
| economic | 136 | 125 |
| technological | 134 | 128 |
| political | 90 | 84 |
| social | 78 | 62 |
| geopolitical | 32 | 31 |
| biological | 11 | 9 |
| organizational | 7 | 6 |
| philosophical | 5 | 4 |
| psychological | 5 | 5 |
| scientific | 5 | 5 |
| legal | 4 | 4 |
| mathematical | 4 | 4 |
| religious | 4 | 4 |
| environmental | 2 | 2 |
| institutional | 2 | 2 |
| linguistic | 2 | 2 |
| logistical | 2 | 2 |
| military | 2 | 2 |
| astrophysical | 1 | 1 |
| bio_industrial | 1 | 1 |
| cognitive | 1 | 1 |
| corporate_governance | 1 | 1 |
| health | 1 | 1 |
| informational | 1 | 1 |
| investigation | 1 | 1 |
| magical | 1 | 1 |
| medical | 1 | 1 |
| physical | 1 | 1 |
| unknown | 1 | 1 |

## Domain Sufficiency Analysis

How well do indices explain variance in each domain?

| Domain | Constraints | Avg Variance | Sufficiency |
|--------|----------:|-------------:|:-----------:|
| institutional | 3 | 0.61 | Medium |
| bio_industrial | 1 | 0.60 | Medium |
| military | 2 | 0.58 | Medium |
| logistical | 2 | 0.57 | Medium |
| socio_political | 1 | 0.57 | Medium |
| medical | 4 | 0.56 | Medium |
| environmental | 6 | 0.55 | Medium |
| cognitive | 4 | 0.54 | Medium |
| informational | 2 | 0.54 | Medium |
| organizational | 9 | 0.53 | Medium |
| social | 133 | 0.52 | Medium |
| psychological | 10 | 0.51 | Medium |
| economic | 210 | 0.51 | Medium |
| corporate_governance | 1 | 0.50 | Medium |
| investigation | 2 | 0.50 | Medium |
| infrastructure | 1 | 0.50 | Medium |
| religious | 9 | 0.50 | Medium |
| political | 154 | 0.49 | Medium |
| philosophical | 11 | 0.49 | Medium |
| biological | 16 | 0.49 | Medium |
| health | 5 | 0.47 | Medium |
| legal | 11 | 0.46 | Medium |
| technological | 282 | 0.46 | Medium |
| geopolitical | 48 | 0.44 | Medium |
| unknown | 5 | 0.38 | Low |
| analytical | 1 | 0.38 | Low |
| systems_engineering | 1 | 0.38 | Low |
| ecological | 2 | 0.36 | Low |
| linguistic | 3 | 0.35 | Low |
| scientific | 19 | 0.33 | Low |
| magical | 1 | 0.33 | Low |
| astrophysical | 1 | 0.33 | Low |
| atmospheric_science | 1 | 0.33 | Low |
| physical | 6 | 0.27 | Low |
| epistemological | 2 | 0.25 | Low |
| mathematical | 63 | 0.24 | Low |
| logical | 1 | 0.17 | Low |
| logic | 1 | 0.14 | Low |
| statistical | 1 | 0.14 | Low |
| sociological | 1 | 0.14 | Low |

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

1. **Data quality:** Domain label normalization consolidated 8 constraints across 5 variant labels. Consider standardizing domain labels upstream in the test corpus.

2. **Perspectival health:** 98.3% of collisions are expected perspectival variance (agent_power driving type shifts). This confirms the index system is capturing real structural dynamics.

