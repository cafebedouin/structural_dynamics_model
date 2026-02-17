# Index Sufficiency Test

## Executive Summary

**Verdict:** INSUFFICIENT - Indices do not explain all variance. New categories recommended.

- **Collision Rate:** 50.2%
- **Anomaly Rate:** 2.7%

### Evidence Against Index Sufficiency

- 50.2% of constraints have index collisions
- Most common collision: snare ↔ tangled_rope (403 cases)

## Index Collisions

**Total collisions detected:** 571
**Constraints affected:** 520

### Most Common Type Transitions

| Transition | Frequency |
|------------|----------|
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
|--------|-----------|---------------------|
| technological        |        147 |                  138 |
| economic             |        140 |                  129 |
| political            |         94 |                   86 |
| social               |         84 |                   67 |
| geopolitical         |         32 |                   31 |
| biological           |         11 |                    9 |
| mathematical         |          7 |                    7 |
| organizational       |          7 |                    6 |
| scientific           |          6 |                    6 |
| philosophical        |          5 |                    4 |
| religious            |          5 |                    5 |
| legal                |          4 |                    4 |
| psychological        |          4 |                    4 |
| environmental        |          2 |                    2 |
| institutional        |          2 |                    2 |
| investigation        |          2 |                    1 |
| linguistic           |          2 |                    2 |
| military             |          2 |                    2 |
| astrophysical        |          1 |                    1 |
| bio_industrial       |          1 |                    1 |
| cognitive            |          1 |                    1 |
| corporate_governance |          1 |                    1 |
| health               |          1 |                    1 |
| informational        |          1 |                    1 |
| logical              |          1 |                    1 |
| logistical           |          1 |                    1 |
| logistics            |          1 |                    1 |
| magical              |          1 |                    1 |
| medical              |          1 |                    1 |
| physical             |          1 |                    1 |
| psychology           |          1 |                    1 |
| statistical          |          1 |                    1 |
| unknown              |          1 |                    1 |

### Collision Examples

Cases where same index configuration produces multiple types:

| Constraint ID | Index Config | Types Produced | Domain |
|---------------|--------------|----------------|--------|
| absorbing_markov_chain_trap    | ('powerless', 'biographical', ... | unknown, snare       | technological |
| abstraction_boundary_overrun   | ('analytical', 'civilizational... | snare, tangled_rope  | technological |
| ad_synaptic_deficit            | ('analytical', 'civilizational... | snare, tangled_rope  | biological |
| adaptive_lag_trap              | ('analytical', 'civilizational... | snare, tangled_rope  | economic   |
| adversarial_surface_inflation  | ('analytical', 'civilizational... | snare, tangled_rope  | technological |
| agency_atrophy                 | ('analytical', 'civilizational... | snare, tangled_rope  | technological |
| agent_opt_2026                 | ('analytical', 'civilizational... | snare, piton         | political  |
| ai_adoption_stigma             | ('analytical', 'civilizational... | snare, tangled_rope  | technological |
| ai_cognitive_diversity_arbitrage | ('powerless', 'biographical', ... | snare, tangled_rope  | technological |
| ai_compute_capital_moat        | ('analytical', 'civilizational... | snare, tangled_rope  | technological |
| ai_edu_decentralization        | ('powerless', 'biographical', ... | mountain, tangled_rope | technological |
| ai_nonconsensual_content_facilitation | ('analytical', 'civilizational... | snare, tangled_rope  | technological |
| ai_performance_watermark       | ('analytical', 'civilizational... | snare, tangled_rope  | technological |
| ai_professional_displacement   | ('analytical', 'civilizational... | snare, tangled_rope  | economic   |
| ai_religion_regulation         | ('analytical', 'civilizational... | snare, tangled_rope  | technological |

## Domain Sufficiency Analysis

How well do indices explain variance in each domain?

| Domain | Constraints | Avg Variance | Sufficiency |
|--------|-------------|--------------|-------------|
| investigation        |           2 |         0.70 | Medium      |
| psychology           |           1 |         0.67 | Medium      |
| institutional        |           3 |         0.61 | Medium      |
| bio_industrial       |           1 |         0.60 | Medium      |
| military             |           2 |         0.58 | Medium      |
| logistics            |           1 |         0.57 | Medium      |
| logistical           |           1 |         0.57 | Medium      |
| socio_political      |           1 |         0.57 | Medium      |
| medical              |           4 |         0.56 | Medium      |
| environmental        |           6 |         0.55 | Medium      |
| social               |         133 |         0.54 | Medium      |
| cognitive            |           4 |         0.54 | Medium      |
| informational        |           2 |         0.54 | Medium      |
| organizational       |           9 |         0.53 | Medium      |
| religious            |           9 |         0.52 | Medium      |
| economic             |         210 |         0.52 | Medium      |
| philosophical        |          11 |         0.51 | Medium      |
| biological           |          16 |         0.50 | Medium      |
| corporate_governance |           1 |         0.50 | Medium      |
| infrastructure       |           1 |         0.50 | Medium      |
| political            |         154 |         0.50 | Medium      |
| psychological        |           9 |         0.49 | Medium      |
| technological        |         281 |         0.49 | Medium      |
| legal                |          11 |         0.47 | Medium      |
| health               |           5 |         0.47 | Medium      |
| geopolitical         |          48 |         0.44 | Medium      |
| unknown              |           5 |         0.42 | Medium      |
| linguistic           |           3 |         0.41 | Medium      |
| scientific           |          19 |         0.39 | Low         |
| physics              |           3 |         0.39 | Low         |
| analytical           |           1 |         0.38 | Low         |
| systems_engineering  |           1 |         0.38 | Low         |
| physical             |           3 |         0.36 | Low         |
| ecological           |           2 |         0.36 | Low         |
| magical              |           1 |         0.33 | Low         |
| epistemological      |           2 |         0.33 | Low         |
| astrophysical        |           1 |         0.33 | Low         |
| logical              |           1 |         0.33 | Low         |
| atmospheric_science  |           1 |         0.33 | Low         |
| mathematical         |          61 |         0.32 | Low         |
| mathematics          |           2 |         0.29 | Low         |
| logic                |           1 |         0.29 | Low         |
| statistical          |           1 |         0.29 | Low         |
| sociological         |           1 |         0.29 | Low         |
| technology           |           1 |         0.17 | Low         |

**Note:** Higher variance = indices capture more differences (good)

## Stability Anomalies

Constraints tested across many index configs but always produce same type.
These may indicate need for new categories beyond current 4 indices.

| Constraint ID | Configs | Type | Domain | Notes |
|---------------|---------|------|--------|-------|
| cantor_set_topology            |       8 | mountain        | mathematical | natural |
| treaty_land_entrenchment       |       8 | mountain        | legal      | natural |
| basel_problem_convergence      |       7 | mountain        | mathematical | natural |
| cantors_diagonal_argument      |       7 | mountain        | technological | natural |
| constraint_ftc                 |       7 | mountain        | technological | enforced |
| constraint_lorenz_sensitivity  |       7 | mountain        | technological | enforced |
| constraint_pythagorean         |       7 | mountain        | mathematical | enforced |
| constraint_sylow               |       7 | mountain        | technological | enforced |
| continuum_hypothesis_undecidability |       7 | mountain        | mathematical | natural |
| copyleft_viral_licensing       |       7 | rope            | technological | enforced |
| large_cardinals_inaccessibility |       7 | mountain        | technological | enforced |
| microrobot_manipulation        |       7 | rope            | technological | enforced |
| noether_theorem                |       7 | mountain        | technological | enforced |
| suslin_hypothesis_proof_limits |       7 | mountain        | mathematical | enforced |
| vertebrate_turning_point_2026  |       7 | rope            | biological | enforced |
| whitehead_incompleteness       |       7 | mountain        | mathematical | enforced |
| wikipedia_noncommercial_model  |       7 | rope            | technological | enforced |
| constraint_borsuk_ulam         |       6 | mountain        | mathematical | enforced |
| constraint_nonstandard_arithmetic |       6 | mountain        | technological | enforced |
| cuny_light_2026                |       6 | rope            | technology | enforced |

## Recommendations

1. **High collision rate detected.** Consider:
   - Adding 5th index dimension
   - Creating hybrid categories (Tangled Rope, Piton, etc.)
   - Refining existing index definitions

