# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 467
- **Constraints with multiple index configs:** 233 (49.9%)
- **High variance (>0.5):** 463 (99.1%)
- **Stable (ratio=1.0):** 59 (12.6%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |    59 |   12.6% | bay_of_pigs_operational_silo, birthday_paradox_... |
| 0.7-0.9         |     0 |    0.0% | - |
| 0.5-0.6         |     0 |    0.0% | - |
| 0.3-0.4         |     0 |    0.0% | - |
| <0.3            |   404 |   86.5% | academic_tenure_system, adverse_possession, ai_... |
| null            |     4 |    0.9% | ancient_grudge, midnight_deadline, self_surpassing |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| educational          |   1 |         5.00 |          100.0% |
| environmental        |   1 |         5.00 |          100.0% |
| health               |   1 |         5.00 |          100.0% |
| ontological          |   1 |         5.00 |          100.0% |
| religious            |  10 |         4.15 |          100.0% |
| psychological        |   7 |         4.00 |          100.0% |
| biological           |   8 |         3.56 |          100.0% |
| medical              |   3 |         3.50 |          100.0% |
| scientific           |   5 |         3.50 |          100.0% |
| social               |  63 |         3.11 |          100.0% |
| analytical           |   2 |         3.00 |          100.0% |
| artistic             |   1 |         3.00 |          100.0% |
| biological_environmental |   1 |         3.00 |          100.0% |
| economic_social      |   1 |         3.00 |          100.0% |
| linguistic           |   1 |         3.00 |          100.0% |
| logical              |   1 |         3.00 |          100.0% |
| magical              |   1 |         3.00 |          100.0% |
| organizational       |   2 |         3.00 |          100.0% |
| philosophical        |   8 |         3.00 |          100.0% |
| political_technological |   1 |         3.00 |          100.0% |
| economic             |  63 |         2.98 |          100.0% |
| political            |  52 |         2.96 |          100.0% |
| legal                |   7 |         2.93 |          100.0% |
| technological        | 114 |         2.78 |          100.0% |
| institutional        |   2 |         2.75 |          100.0% |
| mathematical         |  46 |         2.51 |          100.0% |
| mathematics          |   3 |         2.50 |          100.0% |
| systemic             |   1 |         2.50 |          100.0% |
| corporate_governance |   2 |         1.50 |          100.0% |
| geopolitical         |   2 |         1.50 |          100.0% |
| military             |   1 |         1.50 |          100.0% |
| physics              |   1 |         1.50 |          100.0% |
| narrative            |  14 |         1.00 |          100.0% |
| technical            |   5 |         1.00 |          100.0% |

## Key Findings

1. **Domain variance spread:** educational shows highest variance (5.00), while technical shows lowest (1.00)

2. **High volatility:** 99.1% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| shobies_existential_commitment | 6.00 |       1 |     6 | social     | N/A          |
| empty_tomb_transformation      | 6.00 |       1 |     6 | religious  | N/A          |
| layered_brain_processing       | 6.00 |       1 |     6 | technological | N/A          |
| neural_interoperability        | 6.00 |       1 |     6 | technological | N/A          |
| quantum_nonlocality_2026       | 6.00 |       1 |     6 | technological | N/A          |
| e2ee_digital_privacy_2026      | 6.00 |       1 |     6 | technological | N/A          |
| s1_visa                        | 6.00 |       1 |     6 | economic   | mountain     |
| dionysaic_frenzy               | 6.00 |       1 |     6 | religious  | mountain     |
| ergo_sig_usd_protocol          | 6.00 |       1 |     6 | economic   | rope         |
| quellcrist_falconer_justice    | 6.00 |       1 |     6 | political  | N/A          |

### Detailed Examples

**1. shobies_existential_commitment**
- Domain: social
- Variance: 6.00
- Produces 6 different types across 1 index configurations
- Type distribution: {'rope': 1, 'noose': 1, 'mountain': 1, 'Type1': 1, 'Type2': 1, 'Type3': 1}

**2. empty_tomb_transformation**
- Domain: religious
- Variance: 6.00
- Produces 6 different types across 1 index configurations
- Type distribution: {'noose': 1, 'rope': 2, 'mountain': 1, 'T1': 1, 'T2': 1, 'T3': 1}

**3. layered_brain_processing**
- Domain: technological
- Variance: 6.00
- Produces 6 different types across 1 index configurations
- Type distribution: {'mountain': 1, 'rope': 1, 'noose': 1, 'T1': 1, 'T2': 1, 'T3': 1}

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 99.1% | Core data for variance analysis |
| variance_ratio | 99.1% | Calculated from classifications |
| domain | 92.7% | Affects domain breakdown analysis |
