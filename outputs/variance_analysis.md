# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 339
- **Constraints with multiple index configs:** 330 (97.3%)
- **High variance (>0.5):** 308 (90.9%)
- **Stable (ratio=1.0):** 8 (2.4%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |     8 |    2.4% | CG_IsraelGaza_20231012, ai_scholar_citation_tra... |
| 0.7-0.9         |   140 |   41.3% | MOLTBOT_RELIGION, academic_fashion_modernism_20... |
| 0.5-0.6         |    34 |   10.0% | 26usc469_real_estate_exemption, biological_curi... |
| 0.3-0.4         |     3 |    0.9% | cuny_light_2026, fed_shutdown_2026, rare_earth_... |
| <0.3            |   153 |   45.1% | adverse_possession, ai_driven_surveillance_sens... |
| null            |     1 |    0.3% | unknown |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| magical              |   1 |         3.00 |          100.0% |
| analytical           |   2 |         1.50 |          100.0% |
| mathematics          |   2 |         1.50 |          100.0% |
| physics              |   1 |         1.50 |          100.0% |
| mathematical         |  39 |         1.39 |           94.9% |
| technological        | 109 |         1.15 |           94.5% |
| artistic             |   2 |         1.12 |          100.0% |
| philosophical        |   6 |         1.12 |          100.0% |
| biological           |   8 |         1.07 |           87.5% |
| psychological        |   4 |         1.06 |           75.0% |
| economic             |  43 |         1.03 |           90.7% |
| political            |  36 |         1.01 |           94.4% |
| legal                |   7 |         1.00 |           71.4% |
| social               |  53 |         0.99 |           88.7% |
| religious            |   4 |         0.94 |          100.0% |
| scientific           |   6 |         0.83 |           83.3% |
| environmental        |   1 |         0.80 |          100.0% |
| geopolitical         |   1 |         0.80 |          100.0% |
| organizational       |   1 |         0.80 |          100.0% |
| medical              |   2 |         0.78 |          100.0% |
| astrophysical        |   1 |         0.67 |          100.0% |
| health               |   3 |         0.67 |           66.7% |
| physical             |   1 |         0.67 |          100.0% |
| ecological           |   1 |         0.50 |            0.0% |
| linguistic           |   1 |         0.50 |            0.0% |
| technology           |   1 |         0.33 |            0.0% |
| logical              |   1 |         0.25 |            0.0% |

## Key Findings

1. **Domain variance spread:** magical shows highest variance (3.00), while logical shows lowest (0.25)

2. **High volatility:** 90.9% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| base_pair_complementarity      | 3.00 |       1 |     3 | biological | mountain     |
| berkshire_compounding_culture  | 3.00 |       1 |     3 | economic   | rope         |
| cinderella_midnight_deadline   | 3.00 |       1 |     3 | magical    | mountain     |
| comitatus_bond                 | 3.00 |       1 |     3 | social     | rope         |
| dldr_information_policy        | 3.00 |       1 |     3 | technological | tangled_rope |
| magna_carta_liberties          | 3.00 |       1 |     3 | political  | rope         |
| omelet_perfection_complexity   | 3.00 |       1 |     3 | social     | tangled_rope |
| postman_survival_protocol      | 3.00 |       1 |     3 | social     | tangled_rope |
| ai_driven_surveillance_sensor_layer | 2.00 |       2 |     4 | technological | tangled_rope |
| ai_professional_displacement   | 2.00 |       2 |     4 | economic   | mountain     |

### Detailed Examples

**1. base_pair_complementarity**
- Domain: biological
- Variance: 3.00
- Produces 3 different types across 1 index configurations
- Type distribution: {'mountain': 2, 'rope': 2, 'snare': 1}

**2. berkshire_compounding_culture**
- Domain: economic
- Variance: 3.00
- Produces 3 different types across 1 index configurations
- Type distribution: {'rope': 2, 'mountain': 1, 'snare': 1}

**3. cinderella_midnight_deadline**
- Domain: magical
- Variance: 3.00
- Produces 3 different types across 1 index configurations
- Type distribution: {'mountain': 2, 'rope': 2, 'snare': 2}

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 99.7% | Core data for variance analysis |
| variance_ratio | 99.7% | Calculated from classifications |
| domain | 99.4% | Affects domain breakdown analysis |
