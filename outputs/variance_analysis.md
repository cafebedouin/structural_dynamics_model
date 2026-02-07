# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 298
- **Constraints with multiple index configs:** 289 (97.0%)
- **High variance (>0.5):** 285 (95.6%)
- **Stable (ratio=1.0):** 8 (2.7%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |     8 |    2.7% | 26usc469_real_estate_exemption, creative_common... |
| 0.7-0.9         |    60 |   20.1% | academic_fashion_modernism_2026, ad_fus_coordin... |
| 0.5-0.6         |    11 |    3.7% | ergo_autolykos_asic_resistance, gold_piton_2026... |
| 0.3-0.4         |     2 |    0.7% | cuny_light_2026, rare_earth_coop_2026 |
| <0.3            |   216 |   72.5% | academic_peer_review_gatekeeping, academic_tenu... |
| null            |     1 |    0.3% | unknown |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| analytical           |   2 |         3.00 |          100.0% |
| magical              |   1 |         3.00 |          100.0% |
| mathematics          |   2 |         2.50 |          100.0% |
| mathematical         |  39 |         2.30 |           94.9% |
| artistic             |   2 |         2.25 |          100.0% |
| religious            |   4 |         1.88 |          100.0% |
| technological        |  93 |         1.81 |          100.0% |
| legal                |   7 |         1.79 |          100.0% |
| philosophical        |   5 |         1.60 |          100.0% |
| scientific           |   6 |         1.58 |          100.0% |
| political            |  26 |         1.58 |           96.2% |
| economic             |  33 |         1.58 |           90.9% |
| social               |  49 |         1.52 |           95.9% |
| biological           |   8 |         1.51 |           87.5% |
| physics              |   1 |         1.50 |          100.0% |
| psychological        |   4 |         1.44 |          100.0% |
| ecological           |   1 |         1.25 |          100.0% |
| linguistic           |   1 |         1.25 |          100.0% |
| geopolitical         |   1 |         1.20 |          100.0% |
| health               |   3 |         1.08 |           66.7% |
| environmental        |   1 |         0.80 |          100.0% |
| organizational       |   1 |         0.80 |          100.0% |
| medical              |   2 |         0.78 |          100.0% |
| astrophysical        |   1 |         0.67 |          100.0% |
| physical             |   1 |         0.67 |          100.0% |
| technology           |   1 |         0.33 |            0.0% |
| logical              |   1 |         0.25 |            0.0% |

## Key Findings

1. **Domain variance spread:** analytical shows highest variance (3.00), while logical shows lowest (0.25)

2. **High volatility:** 95.6% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| omelet_perfection_complexity   | 6.00 |       1 |     6 | social     | rope         |
| postman_survival_protocol      | 6.00 |       1 |     6 | social     | rope         |
| comitatus_bond                 | 5.00 |       1 |     5 | social     | rope         |
| ai_driven_surveillance_sensor_layer | 3.50 |       2 |     7 | technological | tangled_rope |
| algorithmic_bias               | 3.50 |       2 |     7 | technological | snare        |
| ai_edu_decentralization        | 3.00 |       2 |     6 | technological | rope         |
| ai_professional_displacement   | 3.00 |       2 |     6 | economic   | mountain     |
| asce_7_22_seismic_design       | 3.00 |       2 |     6 | technological | rope         |
| authoritarian_power_paradox    | 3.00 |       2 |     6 | political  | snare        |
| base_pair_complementarity      | 3.00 |       1 |     3 | biological | rope         |

### Detailed Examples

**1. omelet_perfection_complexity**
- Domain: social
- Variance: 6.00
- Produces 6 different types across 1 index configurations
- Type distribution: {'rope': 1, 'snare': 1, 'mountain': 1, 'T1': 1, 'T2': 1, 'T3': 1}

**2. postman_survival_protocol**
- Domain: social
- Variance: 6.00
- Produces 6 different types across 1 index configurations
- Type distribution: {'rope': 1, 'snare': 1, 'mountain': 1, 'Type1': 1, 'Type2': 1, 'Type3': 1}

**3. comitatus_bond**
- Domain: social
- Variance: 5.00
- Produces 5 different types across 1 index configurations
- Type distribution: {'mountain': 1, 'rope': 1, 'snare': 2, 'Type1': 1, 'Type2': 1}

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 99.7% | Core data for variance analysis |
| variance_ratio | 99.7% | Calculated from classifications |
| domain | 99.3% | Affects domain breakdown analysis |
