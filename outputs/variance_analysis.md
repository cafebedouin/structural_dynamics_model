# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 734
- **Constraints with multiple index configs:** 726 (98.9%)
- **High variance (>0.5):** 676 (92.1%)
- **Stable (ratio=1.0):** 7 (1.0%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |     7 |    1.0% | ai_scholar_citation_trap, evolutionary_knowledg... |
| 0.7-0.9         |   479 |   65.3% | CG_IsraelGaza_20231012, MOLTBOT_RELIGION, abstr... |
| 0.5-0.6         |    81 |   11.0% | 26usc469_real_estate_exemption, alternative_sov... |
| 0.3-0.4         |     6 |    0.8% | cuny_light_2026, fed_shutdown_2026, north_korea... |
| <0.3            |   160 |   21.8% | 8k_tv_limit_2026, adverse_possession, ai_driven... |
| null            |     1 |    0.1% | unknown |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| magical              |   1 |         3.00 |          100.0% |
| investigation        |   2 |         1.88 |          100.0% |
| analytical           |   2 |         1.50 |          100.0% |
| mathematics          |   2 |         1.50 |          100.0% |
| mathematical         |  42 |         1.38 |           90.5% |
| artistic             |   2 |         1.15 |          100.0% |
| physics              |   2 |         1.12 |          100.0% |
| technological        | 205 |         0.96 |           94.1% |
| legal                |   9 |         0.95 |           77.8% |
| biological           |  15 |         0.95 |           93.3% |
| philosophical        |  12 |         0.89 |           83.3% |
| psychological        |   9 |         0.87 |          100.0% |
| religious            |   9 |         0.84 |          100.0% |
| economic             | 120 |         0.84 |           92.5% |
| political            | 113 |         0.82 |           92.9% |
| social               | 119 |         0.82 |           91.6% |
| cognitive            |   3 |         0.80 |          100.0% |
| informational        |   2 |         0.80 |          100.0% |
| infrastructure       |   1 |         0.80 |          100.0% |
| logistical           |   1 |         0.80 |          100.0% |
| logistics            |   1 |         0.80 |          100.0% |
| socio_political      |   1 |         0.80 |          100.0% |
| systems_engineering  |   1 |         0.80 |          100.0% |
| organizational       |   9 |         0.77 |          100.0% |
| environmental        |   5 |         0.77 |          100.0% |
| institutional        |   3 |         0.77 |          100.0% |
| geopolitical         |   8 |         0.76 |          100.0% |
| medical              |   4 |         0.76 |          100.0% |
| astrophysical        |   1 |         0.75 |          100.0% |
| bio_industrial       |   1 |         0.75 |          100.0% |
| corporate_governance |   1 |         0.75 |          100.0% |
| military             |   2 |         0.75 |          100.0% |
| physical             |   1 |         0.75 |          100.0% |
| psychology           |   1 |         0.75 |          100.0% |
| scientific           |   9 |         0.75 |           77.8% |
| health               |   5 |         0.70 |           80.0% |
| linguistic           |   2 |         0.62 |           50.0% |
| atmospheric_science  |   1 |         0.50 |            0.0% |
| ecological           |   1 |         0.50 |            0.0% |
| technology           |   1 |         0.33 |            0.0% |
| logical              |   2 |         0.25 |            0.0% |

## Key Findings

1. **Domain variance spread:** magical shows highest variance (3.00), while logical shows lowest (0.25)

2. **High volatility:** 92.1% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| magna_carta_liberties          | 4.00 |       1 |     4 | political  | rope         |
| base_pair_complementarity      | 3.00 |       1 |     3 | biological | mountain     |
| berkshire_compounding_culture  | 3.00 |       1 |     3 | economic   | rope         |
| cinderella_midnight_deadline   | 3.00 |       1 |     3 | magical    | mountain     |
| comitatus_bond                 | 3.00 |       1 |     3 | social     | rope         |
| fnl_shadow_probe               | 3.00 |       1 |     3 | investigation | mountain     |
| 8k_tv_limit_2026               | 2.00 |       1 |     2 | technological | rope         |
| ai_driven_surveillance_sensor_layer | 2.00 |       2 |     4 | technological | tangled_rope |
| ai_professional_displacement   | 2.00 |       2 |     4 | economic   | mountain     |
| algorithmic_bias               | 2.00 |       2 |     4 | technological | snare        |

### Detailed Examples

**1. magna_carta_liberties**
- Domain: political
- Variance: 4.00
- Produces 4 different types across 1 index configurations
- Type distribution: {'mountain': 2, 'rope': 2, 'snare': 2, 'tangled_rope': 1}

**2. base_pair_complementarity**
- Domain: biological
- Variance: 3.00
- Produces 3 different types across 1 index configurations
- Type distribution: {'mountain': 2, 'rope': 2, 'snare': 1}

**3. berkshire_compounding_culture**
- Domain: economic
- Variance: 3.00
- Produces 3 different types across 1 index configurations
- Type distribution: {'rope': 2, 'mountain': 1, 'snare': 1}

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 99.9% | Core data for variance analysis |
| variance_ratio | 99.9% | Calculated from classifications |
| domain | 99.6% | Affects domain breakdown analysis |
