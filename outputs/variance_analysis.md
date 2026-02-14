# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 754
- **Constraints with multiple index configs:** 751 (99.6%)
- **High variance (>0.5):** 653 (86.6%)
- **Stable (ratio=1.0):** 6 (0.8%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |     6 |    0.8% | ai_scholar_citation_trap, evolutionary_knowledg... |
| 0.7-0.9         |   490 |   65.0% | CG_IsraelGaza_20231012, MOLTBOT_RELIGION, abstr... |
| 0.5-0.6         |   113 |   15.0% | 26usc469_real_estate_exemption, ai_professional... |
| 0.3-0.4         |    14 |    1.9% | cuny_light_2026, fed_shutdown_2026, kjv_linguis... |
| <0.3            |   130 |   17.2% | 8k_tv_limit_2026, adverse_possession, ai_driven... |
| null            |     1 |    0.1% | unknown |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| analytical           |   2 |         1.50 |          100.0% |
| magical              |   1 |         1.50 |          100.0% |
| mathematics          |   2 |         1.50 |          100.0% |
| investigation        |   2 |         1.38 |          100.0% |
| artistic             |   2 |         1.15 |          100.0% |
| philosophical        |  12 |         0.89 |           83.3% |
| psychological        |   9 |         0.87 |          100.0% |
| mathematical         |  44 |         0.86 |           59.1% |
| technological        | 207 |         0.85 |           87.9% |
| legal                |  10 |         0.83 |           70.0% |
| cognitive            |   3 |         0.80 |          100.0% |
| informational        |   2 |         0.80 |          100.0% |
| infrastructure       |   1 |         0.80 |          100.0% |
| logistical           |   1 |         0.80 |          100.0% |
| logistics            |   1 |         0.80 |          100.0% |
| socio_political      |   1 |         0.80 |          100.0% |
| biological           |  16 |         0.80 |           87.5% |
| organizational       |   9 |         0.77 |          100.0% |
| economic             | 125 |         0.77 |           91.2% |
| environmental        |   5 |         0.77 |          100.0% |
| political            | 115 |         0.77 |           93.0% |
| institutional        |   3 |         0.77 |          100.0% |
| medical              |   4 |         0.76 |          100.0% |
| social               | 121 |         0.76 |           87.6% |
| bio_industrial       |   1 |         0.75 |          100.0% |
| corporate_governance |   1 |         0.75 |          100.0% |
| military             |   2 |         0.75 |          100.0% |
| physical             |   1 |         0.75 |          100.0% |
| physics              |   1 |         0.75 |          100.0% |
| psychology           |   1 |         0.75 |          100.0% |
| geopolitical         |   9 |         0.74 |          100.0% |
| religious            |   9 |         0.72 |          100.0% |
| health               |   5 |         0.65 |           60.0% |
| systems_engineering  |   1 |         0.60 |          100.0% |
| Social               |   1 |         0.50 |            0.0% |
| astrophysical        |   1 |         0.50 |            0.0% |
| atmospheric_science  |   1 |         0.50 |            0.0% |
| ecological           |   1 |         0.50 |            0.0% |
| scientific           |  13 |         0.50 |           53.8% |
| linguistic           |   3 |         0.47 |           33.3% |
| technology           |   1 |         0.33 |            0.0% |
| logical              |   1 |         0.25 |            0.0% |

## Key Findings

1. **Domain variance spread:** analytical shows highest variance (1.50), while logical shows lowest (0.25)

2. **High volatility:** 86.6% of constraints show high variance (>0.5)

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
| fnl_shadow_probe               | 2.00 |       1 |     2 | investigation | tangled_rope |
| adverse_possession             | 1.50 |       2 |     3 | economic   | snare        |
| ai_edu_decentralization        | 1.50 |       2 |     3 | technological | rope         |
| amish_technological_renunciation | 1.50 |       2 |     3 | social     | snare        |
| asshole_filter_2015            | 1.50 |       2 |     3 | psychological | snare        |

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
| cantor_set_topology            |       5 |     1 |     0.20 | mathematical    |
| conways_game_of_life_dynamics  |       5 |     1 |     0.20 | mathematical    |
| dldr_information_policy        |       5 |     1 |     0.20 | technological   |
| ehrenfest_barrier              |       5 |     1 |     0.20 | scientific      |
| gauss_bonnet_topology          |       5 |     1 |     0.20 | mathematical    |
| quantum_measurement_gap        |       5 |     1 |     0.20 | scientific      |
| three_body_unpredicability     |       5 |     1 |     0.20 | technological   |

**Note:** These constraints have many perspective configurations but produce the same type. This might indicate:
- The constraint is genuinely invariant (e.g., physical laws)
- Index dimensions are not affecting classification
- Potential data quality issue

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 99.9% | Core data for variance analysis |
| variance_ratio | 99.9% | Calculated from classifications |
| domain | 99.6% | Affects domain breakdown analysis |
