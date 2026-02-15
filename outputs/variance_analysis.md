# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 1026
- **Constraints with multiple index configs:** 0 (0.0%)
- **High variance (>0.5):** 1024 (99.8%)
- **Stable (ratio=1.0):** 113 (11.0%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |   113 |   11.0% | local_vs_global_optima, english_chinese_tense_s... |
| 0.7-0.9         |     0 |    0.0% | - |
| 0.5-0.6         |     0 |    0.0% | - |
| 0.3-0.4         |     0 |    0.0% | - |
| <0.3            |   911 |   88.8% | 26usc469_real_estate_exemption, 8k_tv_limit_202... |
| null            |     2 |    0.2% | unknown, fnl_trace_diagnostic |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| informational        |   2 |         4.00 |          100.0% |
| infrastructure       |   1 |         4.00 |          100.0% |
| logistical           |   1 |         4.00 |          100.0% |
| logistics            |   1 |         4.00 |          100.0% |
| socio_political      |   1 |         4.00 |          100.0% |
| cognitive            |   4 |         3.75 |          100.0% |
| organizational       |   9 |         3.44 |          100.0% |
| environmental        |   5 |         3.40 |          100.0% |
| institutional        |   3 |         3.33 |          100.0% |
| medical              |   4 |         3.25 |          100.0% |
| social               | 132 |         3.02 |          100.0% |
| political            | 152 |         3.02 |          100.0% |
| bio_industrial       |   1 |         3.00 |          100.0% |
| corporate_governance |   1 |         3.00 |          100.0% |
| military             |   2 |         3.00 |          100.0% |
| psychological        |   9 |         3.00 |          100.0% |
| psychology           |   1 |         3.00 |          100.0% |
| religious            |   9 |         3.00 |          100.0% |
| systems_engineering  |   1 |         3.00 |          100.0% |
| economic             | 206 |         2.99 |          100.0% |
| geopolitical         |  48 |         2.96 |          100.0% |
| philosophical        |  12 |         2.92 |          100.0% |
| legal                |  10 |         2.90 |          100.0% |
| biological           |  16 |         2.81 |          100.0% |
| technological        | 280 |         2.74 |          100.0% |
| health               |   5 |         2.60 |          100.0% |
| ecological           |   2 |         2.50 |          100.0% |
| investigation        |   2 |         2.50 |          100.0% |
| physics              |   2 |         2.50 |          100.0% |
| scientific           |  19 |         2.16 |          100.0% |
| Political            |   1 |         2.00 |          100.0% |
| Social               |   1 |         2.00 |          100.0% |
| analytical           |   1 |         2.00 |          100.0% |
| astrophysical        |   1 |         2.00 |          100.0% |
| atmospheric_science  |   1 |         2.00 |          100.0% |
| linguistic           |   3 |         2.00 |          100.0% |
| magical              |   1 |         2.00 |          100.0% |
| physical             |   3 |         1.67 |          100.0% |
| mathematics          |   2 |         1.50 |          100.0% |
| mathematical         |  61 |         1.49 |          100.0% |
| Physics              |   1 |         1.00 |          100.0% |
| epistemological      |   1 |         1.00 |          100.0% |
| logic                |   1 |         1.00 |          100.0% |
| logical              |   1 |         1.00 |          100.0% |
| sociological         |   1 |         1.00 |          100.0% |
| statistical          |   1 |         1.00 |          100.0% |
| technology           |   1 |         1.00 |          100.0% |

## Key Findings

1. **Domain variance spread:** informational shows highest variance (4.00), while technology shows lowest (1.00)

2. **High volatility:** 99.8% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| evolutionary_knowledge         | 5.00 |       1 |     5 | biological | tangled_rope |
| emergency_powers_ratchet       | 4.00 |       1 |     4 | political  | tangled_rope |
| scientific_paradigm_lifecycle  | 4.00 |       1 |     4 | scientific | tangled_rope |
| israeli_settlement_policy_authority_restriction | 4.00 |       1 |     4 | political  | tangled_rope |
| model_autonomy_creep           | 4.00 |       1 |     4 | technological | tangled_rope |
| cross_domain_coupling_spiral   | 4.00 |       1 |     4 | technological | tangled_rope |
| capital_misallocation_spiral   | 4.00 |       1 |     4 | economic   | tangled_rope |
| asymmetric_burden_distribution | 4.00 |       1 |     4 | economic   | piton        |
| semantic_overload_friction     | 4.00 |       1 |     4 | technological | tangled_rope |
| china_taiwan_reunification_mandate | 4.00 |       1 |     4 | political  | tangled_rope |

### Detailed Examples

**1. evolutionary_knowledge**
- Domain: biological
- Variance: 5.00
- Produces 5 different types across 1 index configurations
- Type distribution: {'snare': 2, 'rope': 2, 'tangled_rope': 1, 'piton': 1, 'mountain': 1}

**2. emergency_powers_ratchet**
- Domain: political
- Variance: 4.00
- Produces 4 different types across 1 index configurations
- Type distribution: {'snare': 2, 'rope': 2, 'tangled_rope': 2, 'scaffold': 1}

**3. scientific_paradigm_lifecycle**
- Domain: scientific
- Variance: 4.00
- Produces 4 different types across 1 index configurations
- Type distribution: {'snare': 2, 'rope': 2, 'piton': 2, 'tangled_rope': 1}

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 99.8% | Core data for variance analysis |
| variance_ratio | 99.8% | Calculated from classifications |
| domain | 99.7% | Affects domain breakdown analysis |
