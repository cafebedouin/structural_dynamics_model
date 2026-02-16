# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 1026
- **Constraints with multiple index configs:** 1022 (99.6%)
- **High variance (>0.5):** 748 (72.9%)
- **Stable (ratio=1.0):** 5 (0.5%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |     5 |    0.5% | ai_scholar_citation_trap, evolutionary_knowledg... |
| 0.7-0.9         |   538 |   52.4% | CG_IsraelGaza_20231012, MOLTBOT_RELIGION, abstr... |
| 0.5-0.6         |   287 |   28.0% | absorbing_markov_chains, ai_compute_capital_moa... |
| 0.3-0.4         |    54 |    5.3% | asean_ceasefire_2011, astm_d638_tensile_testing... |
| <0.3            |   140 |   13.6% | 26usc469_real_estate_exemption, 8k_tv_limit_202... |
| null            |     2 |    0.2% | unknown, fnl_trace_diagnostic |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| legal                |  10 |         0.81 |           80.0% |
| informational        |   2 |         0.80 |          100.0% |
| infrastructure       |   1 |         0.80 |          100.0% |
| logistical           |   1 |         0.80 |          100.0% |
| logistics            |   1 |         0.80 |          100.0% |
| socio_political      |   1 |         0.80 |          100.0% |
| cognitive            |   4 |         0.79 |          100.0% |
| psychological        |   9 |         0.78 |          100.0% |
| organizational       |   9 |         0.77 |          100.0% |
| environmental        |   5 |         0.77 |          100.0% |
| institutional        |   3 |         0.77 |          100.0% |
| medical              |   4 |         0.76 |          100.0% |
| bio_industrial       |   1 |         0.75 |          100.0% |
| corporate_governance |   1 |         0.75 |          100.0% |
| military             |   2 |         0.75 |          100.0% |
| psychology           |   1 |         0.75 |          100.0% |
| philosophical        |  12 |         0.74 |           75.0% |
| religious            |   9 |         0.72 |          100.0% |
| social               | 132 |         0.71 |           84.8% |
| political            | 152 |         0.71 |           85.5% |
| economic             | 206 |         0.68 |           83.5% |
| technological        | 280 |         0.66 |           70.4% |
| biological           |  16 |         0.66 |           75.0% |
| health               |   5 |         0.65 |           60.0% |
| investigation        |   2 |         0.62 |           50.0% |
| systems_engineering  |   1 |         0.60 |          100.0% |
| physics              |   2 |         0.57 |           50.0% |
| geopolitical         |  48 |         0.57 |           56.2% |
| ecological           |   2 |         0.55 |           50.0% |
| Political            |   1 |         0.50 |            0.0% |
| Social               |   1 |         0.50 |            0.0% |
| astrophysical        |   1 |         0.50 |            0.0% |
| atmospheric_science  |   1 |         0.50 |            0.0% |
| magical              |   1 |         0.50 |            0.0% |
| scientific           |  19 |         0.47 |           42.1% |
| linguistic           |   3 |         0.47 |           33.3% |
| physical             |   3 |         0.42 |           33.3% |
| analytical           |   1 |         0.40 |            0.0% |
| mathematics          |   2 |         0.38 |            0.0% |
| mathematical         |  61 |         0.34 |           14.8% |
| technology           |   1 |         0.33 |            0.0% |
| Physics              |   1 |         0.25 |            0.0% |
| epistemological      |   1 |         0.25 |            0.0% |
| logic                |   1 |         0.25 |            0.0% |
| logical              |   1 |         0.25 |            0.0% |
| sociological         |   1 |         0.25 |            0.0% |
| statistical          |   1 |         0.20 |            0.0% |

## Key Findings

1. **Domain variance spread:** legal shows highest variance (0.81), while statistical shows lowest (0.20)

2. **High volatility:** 72.9% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| 26usc469_real_estate_exemption | 3.00 |       1 |     3 | economic   | rope         |
| 8k_tv_limit_2026               | 2.00 |       1 |     2 | technological | rope         |
| ai_driven_surveillance_sensor_layer | 2.00 |       2 |     4 | technological | tangled_rope |
| algorithmic_bias               | 2.00 |       2 |     4 | technological | snare        |
| magna_carta_liberties          | 2.00 |       2 |     4 | political  | rope         |
| adverse_possession             | 1.50 |       2 |     3 | economic   | snare        |
| ai_edu_decentralization        | 1.50 |       2 |     3 | technological | rope         |
| amish_technological_renunciation | 1.50 |       2 |     3 | social     | snare        |
| asshole_filter_2015            | 1.50 |       2 |     3 | psychological | snare        |
| authoritarian_power_paradox    | 1.50 |       2 |     3 | political  | snare        |

### Detailed Examples

**1. 26usc469_real_estate_exemption**
- Domain: economic
- Variance: 3.00
- Produces 3 different types across 1 index configurations
- Type distribution: {'snare': 1, 'rope': 2, 'tangled_rope': 1}

**2. 8k_tv_limit_2026**
- Domain: technological
- Variance: 2.00
- Produces 2 different types across 1 index configurations
- Type distribution: {'snare': 1, 'piton': 3}

**3. ai_driven_surveillance_sensor_layer**
- Domain: technological
- Variance: 2.00
- Produces 4 different types across 2 index configurations
- Type distribution: {'snare': 2, 'tangled_rope': 1, 'rope': 1, 'mountain': 1}

## Suspicious Stability

Constraints with many index configs but low variance (possible modeling issues):

| Constraint ID | Configs | Types | Variance | Domain |
|---------------|---------|-------|----------|--------|
| great_mongolian_road_economic_dependency |       7 |     2 |     0.29 | economic        |
| us_embargo_cuba                |       7 |     2 |     0.29 | political       |
| banach_fixed_point_theorem     |       5 |     1 |     0.20 | technological   |
| cantor_set_topology            |       5 |     1 |     0.20 | mathematical    |
| conways_game_of_life_dynamics  |       5 |     1 |     0.20 | mathematical    |
| copyleft_viral_licensing       |       5 |     1 |     0.20 | technological   |
| dldr_information_policy        |       5 |     1 |     0.20 | technological   |
| ehrenfest_barrier              |       5 |     1 |     0.20 | scientific      |
| gauss_bonnet_topology          |       5 |     1 |     0.20 | mathematical    |
| heine_borel_theorem            |       5 |     1 |     0.20 | mathematical    |

**Note:** These constraints have many perspective configurations but produce the same type. This might indicate:
- The constraint is genuinely invariant (e.g., physical laws)
- Index dimensions are not affecting classification
- Potential data quality issue

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 99.8% | Core data for variance analysis |
| variance_ratio | 99.8% | Calculated from classifications |
| domain | 99.7% | Affects domain breakdown analysis |
