# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 727
- **Constraints with multiple index configs:** 10 (1.4%)
- **High variance (>0.5):** 725 (99.7%)
- **Stable (ratio=1.0):** 12 (1.7%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |    12 |    1.7% | c_physical_blue_wavelength, clt_convergence_202... |
| 0.7-0.9         |     9 |    1.2% | CG_IsraelGaza_20231012, MOLTBOT_RELIGION, abstr... |
| 0.5-0.6         |     1 |    0.1% | 26usc469_real_estate_exemption |
| 0.3-0.4         |     0 |    0.0% | - |
| <0.3            |   704 |   96.8% | 8k_tv_limit_2026, emergency_powers_ratchet, erg... |
| null            |     1 |    0.1% | unknown |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| artistic             |   2 |         4.00 |          100.0% |
| cognitive            |   3 |         4.00 |          100.0% |
| informational        |   2 |         4.00 |          100.0% |
| infrastructure       |   1 |         4.00 |          100.0% |
| logistical           |   1 |         4.00 |          100.0% |
| logistics            |   1 |         4.00 |          100.0% |
| socio_political      |   1 |         4.00 |          100.0% |
| systems_engineering  |   1 |         4.00 |          100.0% |
| analytical           |   2 |         3.50 |          100.0% |
| mathematics          |   2 |         3.50 |          100.0% |
| organizational       |   9 |         3.44 |          100.0% |
| environmental        |   5 |         3.40 |          100.0% |
| institutional        |   3 |         3.33 |          100.0% |
| mathematical         |  42 |         3.26 |          100.0% |
| geopolitical         |   8 |         3.25 |          100.0% |
| technological        | 205 |         3.18 |          100.0% |
| philosophical        |  12 |         3.17 |          100.0% |
| religious            |   9 |         3.11 |          100.0% |
| social               | 118 |         3.11 |          100.0% |
| political            | 112 |         3.10 |          100.0% |
| biological           |  15 |         3.05 |          100.0% |
| economic             | 118 |         3.05 |          100.0% |
| astrophysical        |   1 |         3.00 |          100.0% |
| bio_industrial       |   1 |         3.00 |          100.0% |
| corporate_governance |   1 |         3.00 |          100.0% |
| ecological           |   1 |         3.00 |          100.0% |
| linguistic           |   2 |         3.00 |          100.0% |
| magical              |   1 |         3.00 |          100.0% |
| military             |   2 |         3.00 |          100.0% |
| physical             |   1 |         3.00 |          100.0% |
| physics              |   2 |         3.00 |          100.0% |
| psychological        |   9 |         3.00 |          100.0% |
| psychology           |   1 |         3.00 |          100.0% |
| legal                |   9 |         2.83 |           88.9% |
| health               |   5 |         2.80 |          100.0% |
| scientific           |   9 |         2.78 |          100.0% |
| medical              |   4 |         2.45 |          100.0% |
| atmospheric_science  |   1 |         2.00 |          100.0% |
| logical              |   2 |         1.00 |          100.0% |
| technology           |   1 |         1.00 |          100.0% |

## Key Findings

1. **Domain variance spread:** artistic shows highest variance (4.00), while technology shows lowest (1.00)

2. **High volatility:** 99.7% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| evolutionary_knowledge         | 5.00 |       1 |     5 | biological | tangled_rope |
| emergency_powers_ratchet       | 4.00 |       1 |     4 | political  | tangled_rope |
| rfc9293_state_machine          | 4.00 |       1 |     4 | technological | mountain     |
| model_autonomy_creep           | 4.00 |       1 |     4 | technological | tangled_rope |
| quine_self_replication         | 4.00 |       1 |     4 | technological | mountain     |
| cross_domain_coupling_spiral   | 4.00 |       1 |     4 | technological | tangled_rope |
| capital_misallocation_spiral   | 4.00 |       1 |     4 | economic   | tangled_rope |
| goldbach_conjecture            | 4.00 |       1 |     4 | mathematical | mountain     |
| asymmetric_burden_distribution | 4.00 |       1 |     4 | economic   | piton        |
| semantic_overload_friction     | 4.00 |       1 |     4 | technological | tangled_rope |

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

**3. rfc9293_state_machine**
- Domain: technological
- Variance: 4.00
- Produces 4 different types across 1 index configurations
- Type distribution: {'mountain': 1, 'rope': 1, 'snare': 1, 'scaffold': 1}

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 99.9% | Core data for variance analysis |
| variance_ratio | 99.9% | Calculated from classifications |
| domain | 99.7% | Affects domain breakdown analysis |
