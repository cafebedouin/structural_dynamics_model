# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 1026
- **Constraints with multiple index configs:** 1023 (99.7%)
- **High variance (>0.5):** 799 (77.9%)
- **Stable (ratio=1.0):** 6 (0.6%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |     6 |    0.6% | ai_scholar_citation_trap, evolutionary_knowledg... |
| 0.7-0.9         |   537 |   52.3% | CG_IsraelGaza_20231012, MOLTBOT_RELIGION, abstr... |
| 0.5-0.6         |   266 |   25.9% | 26usc469_real_estate_exemption, absorbing_marko... |
| 0.3-0.4         |    49 |    4.8% | asean_ceasefire_2011, beehiiv_platform_model, b... |
| <0.3            |   166 |   16.2% | 8k_tv_limit_2026, adverse_possession, ai_driven... |
| null            |     2 |    0.2% | unknown, fnl_trace_diagnostic |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| analytical           |   2 |         1.50 |          100.0% |
| magical              |   1 |         1.50 |          100.0% |
| mathematics          |   2 |         1.50 |          100.0% |
| artistic             |   2 |         1.15 |          100.0% |
| philosophical        |  13 |         0.88 |           84.6% |
| psychological        |   9 |         0.87 |          100.0% |
| legal                |  11 |         0.81 |           72.7% |
| cognitive            |   3 |         0.80 |          100.0% |
| informational        |   2 |         0.80 |          100.0% |
| infrastructure       |   1 |         0.80 |          100.0% |
| logistical           |   1 |         0.80 |          100.0% |
| logistics            |   1 |         0.80 |          100.0% |
| socio_political      |   1 |         0.80 |          100.0% |
| biological           |  16 |         0.80 |           87.5% |
| organizational       |   9 |         0.77 |          100.0% |
| environmental        |   5 |         0.77 |          100.0% |
| institutional        |   3 |         0.77 |          100.0% |
| medical              |   4 |         0.76 |          100.0% |
| bio_industrial       |   1 |         0.75 |          100.0% |
| corporate_governance |   1 |         0.75 |          100.0% |
| military             |   2 |         0.75 |          100.0% |
| psychology           |   1 |         0.75 |          100.0% |
| social               | 134 |         0.75 |           85.1% |
| technological        | 284 |         0.74 |           75.4% |
| mathematical         |  56 |         0.73 |           46.4% |
| political            | 152 |         0.72 |           86.2% |
| religious            |   9 |         0.72 |          100.0% |
| economic             | 205 |         0.70 |           84.9% |
| health               |   5 |         0.65 |           60.0% |
| investigation        |   2 |         0.62 |           50.0% |
| systems_engineering  |   1 |         0.60 |          100.0% |
| physics              |   2 |         0.57 |           50.0% |
| geopolitical         |  48 |         0.57 |           56.2% |
| ecological           |   2 |         0.55 |           50.0% |
| scientific           |  19 |         0.51 |           52.6% |
| Political            |   1 |         0.50 |            0.0% |
| Social               |   1 |         0.50 |            0.0% |
| astrophysical        |   1 |         0.50 |            0.0% |
| atmospheric_science  |   1 |         0.50 |            0.0% |
| linguistic           |   3 |         0.47 |           33.3% |
| physical             |   3 |         0.42 |           33.3% |
| technology           |   1 |         0.33 |            0.0% |
| Physics              |   1 |         0.25 |            0.0% |
| logical              |   1 |         0.25 |            0.0% |

## Key Findings

1. **Domain variance spread:** analytical shows highest variance (1.50), while logical shows lowest (0.25)

2. **High volatility:** 77.9% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| 8k_tv_limit_2026               | 2.00 |       1 |     2 | technological | rope         |
| ai_driven_surveillance_sensor_layer | 2.00 |       2 |     4 | technological | tangled_rope |
| algorithmic_bias               | 2.00 |       2 |     4 | technological | snare        |
| magna_carta_liberties          | 2.00 |       2 |     4 | political  | rope         |
| sadhu_integrity_protocol       | 2.00 |       2 |     4 | social     | rope         |
| adverse_possession             | 1.50 |       2 |     3 | economic   | snare        |
| ai_edu_decentralization        | 1.50 |       2 |     3 | technological | rope         |
| amish_technological_renunciation | 1.50 |       2 |     3 | social     | snare        |
| asshole_filter_2015            | 1.50 |       2 |     3 | psychological | snare        |
| astm_d638_tensile_testing      | 1.50 |       2 |     3 | technological | rope         |

### Detailed Examples

**1. 8k_tv_limit_2026**
- Domain: technological
- Variance: 2.00
- Produces 2 different types across 1 index configurations
- Type distribution: {'snare': 1, 'piton': 3}

**2. ai_driven_surveillance_sensor_layer**
- Domain: technological
- Variance: 2.00
- Produces 4 different types across 2 index configurations
- Type distribution: {'snare': 2, 'tangled_rope': 1, 'rope': 1, 'mountain': 1}

**3. algorithmic_bias**
- Domain: technological
- Variance: 2.00
- Produces 4 different types across 2 index configurations
- Type distribution: {'snare': 2, 'rope': 1, 'tangled_rope': 2, 'mountain': 1}

## Suspicious Stability

Constraints with many index configs but low variance (possible modeling issues):

| Constraint ID | Configs | Types | Variance | Domain |
|---------------|---------|-------|----------|--------|
| great_mongolian_road_economic_dependency |       7 |     2 |     0.29 | economic        |
| us_embargo_cuba                |       7 |     2 |     0.29 | political       |
| banach_fixed_point_theorem     |       5 |     1 |     0.20 | technological   |
| cantor_set_topology            |       5 |     1 |     0.20 | mathematical    |
| conways_game_of_life_dynamics  |       5 |     1 |     0.20 | mathematical    |
| dldr_information_policy        |       5 |     1 |     0.20 | technological   |
| ehrenfest_barrier              |       5 |     1 |     0.20 | scientific      |
| gauss_bonnet_topology          |       5 |     1 |     0.20 | mathematical    |
| heine_borel_theorem            |       5 |     1 |     0.20 | mathematical    |
| microrobot_manipulation        |       5 |     1 |     0.20 | technological   |

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
