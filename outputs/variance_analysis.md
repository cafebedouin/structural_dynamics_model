# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 1036
- **Constraints with multiple index configs:** 1036 (100.0%)
- **High variance (>0.5):** 347 (33.5%)
- **Stable (ratio=1.0):** 1 (0.1%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |     1 |    0.1% | moltbook_agent_theater |
| 0.7-0.9         |    52 |    5.0% | absorbing_markov_chain_trap, ai_scholar_citatio... |
| 0.5-0.6         |   599 |   57.8% | abstraction_boundary_overrun, abstraction_leaka... |
| 0.3-0.4         |   261 |   25.2% | academic_fashion_modernism_2026, access_arbitra... |
| <0.3            |   123 |   11.9% | asean_ceasefire_2011, automatic_enrollment_defa... |
| null            |     0 |    0.0% | - |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| investigation        |   2 |         0.70 |          100.0% |
| psychology           |   1 |         0.67 |          100.0% |
| institutional        |   3 |         0.61 |          100.0% |
| bio_industrial       |   1 |         0.60 |          100.0% |
| military             |   2 |         0.58 |           50.0% |
| logistical           |   1 |         0.57 |          100.0% |
| logistics            |   1 |         0.57 |          100.0% |
| socio_political      |   1 |         0.57 |          100.0% |
| medical              |   4 |         0.56 |           75.0% |
| environmental        |   6 |         0.55 |           50.0% |
| social               | 133 |         0.54 |           40.6% |
| cognitive            |   4 |         0.54 |           50.0% |
| informational        |   2 |         0.54 |           50.0% |
| organizational       |   9 |         0.53 |           33.3% |
| religious            |   9 |         0.52 |           33.3% |
| economic             | 210 |         0.52 |           34.8% |
| philosophical        |  11 |         0.51 |           45.5% |
| biological           |  16 |         0.50 |           43.8% |
| corporate_governance |   1 |         0.50 |            0.0% |
| infrastructure       |   1 |         0.50 |            0.0% |
| political            | 154 |         0.50 |           28.6% |
| psychological        |   9 |         0.49 |           33.3% |
| technological        | 281 |         0.49 |           40.6% |
| legal                |  11 |         0.47 |           36.4% |
| health               |   5 |         0.47 |           20.0% |
| geopolitical         |  48 |         0.44 |           10.4% |
| linguistic           |   3 |         0.41 |           33.3% |
| scientific           |  19 |         0.39 |           15.8% |
| physics              |   3 |         0.39 |           33.3% |
| analytical           |   1 |         0.38 |            0.0% |
| systems_engineering  |   1 |         0.38 |            0.0% |
| physical             |   3 |         0.36 |            0.0% |
| ecological           |   2 |         0.36 |            0.0% |
| astrophysical        |   1 |         0.33 |            0.0% |
| atmospheric_science  |   1 |         0.33 |            0.0% |
| epistemological      |   2 |         0.33 |            0.0% |
| logical              |   1 |         0.33 |            0.0% |
| magical              |   1 |         0.33 |            0.0% |
| mathematical         |  61 |         0.32 |            8.2% |
| logic                |   1 |         0.29 |            0.0% |
| mathematics          |   2 |         0.29 |            0.0% |
| sociological         |   1 |         0.29 |            0.0% |
| statistical          |   1 |         0.29 |            0.0% |
| technology           |   1 |         0.17 |            0.0% |

## Key Findings

1. **Domain variance spread:** investigation shows highest variance (0.70), while technology shows lowest (0.17)

2. **High volatility:** 33.5% of constraints show high variance (>0.5)

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| moltbook_agent_theater         | 1.00 |       5 |     5 | technological | piton        |
| absorbing_markov_chain_trap    | 0.83 |       6 |     5 | technological | snare        |
| ai_scholar_citation_trap       | 0.83 |       6 |     5 | technological | tangled_rope |
| france_local_elections_march_2026 | 0.83 |       6 |     5 | political  | tangled_rope |
| genie_ip_constraint            | 0.83 |       6 |     5 | technological | tangled_rope |
| institutional_trust_decay      | 0.83 |       6 |     5 | social     | piton        |
| openai_api_access              | 0.83 |       6 |     5 | technological | tangled_rope |
| openai_codex_app_constraint    | 0.83 |       6 |     5 | technological | tangled_rope |
| openscholar_peer_review        | 0.83 |       6 |     5 | technological | tangled_rope |
| ulysses_scylla_1904            | 0.83 |       6 |     5 | social     | piton        |

### Detailed Examples

**1. moltbook_agent_theater**
- Domain: technological
- Variance: 1.00
- Produces 5 different types across 5 index configurations
- Type distribution: {'tangled_rope': 2, 'rope': 2, 'unknown': 1, 'snare': 1, 'piton': 1}

**2. absorbing_markov_chain_trap**
- Domain: technological
- Variance: 0.83
- Produces 5 different types across 6 index configurations
- Type distribution: {'unknown': 2, 'scaffold': 1, 'snare': 3, 'rope': 1, 'tangled_rope': 1}

**3. ai_scholar_citation_trap**
- Domain: technological
- Variance: 0.83
- Produces 5 different types across 6 index configurations
- Type distribution: {'unknown': 2, 'scaffold': 1, 'snare': 2, 'rope': 1, 'tangled_rope': 1}

## Suspicious Stability

Constraints with many index configs but low variance (possible modeling issues):

| Constraint ID | Configs | Types | Variance | Domain |
|---------------|---------|-------|----------|--------|
| banach_fixed_point_theorem     |       8 |     2 |     0.25 | technological   |
| cantor_set_topology            |       8 |     1 |     0.12 | mathematical    |
| constraint_twin_prime_conjecture |       8 |     2 |     0.25 | mathematical    |
| conways_game_of_life_dynamics  |       8 |     2 |     0.25 | mathematical    |
| cuban_missile_crisis_excomm_deliberation |       8 |     2 |     0.25 | political       |
| ehrenfest_barrier              |       8 |     2 |     0.25 | scientific      |
| gauss_bonnet_topology          |       8 |     2 |     0.25 | mathematical    |
| heine_borel                    |       8 |     2 |     0.25 | mathematical    |
| lcdm_small_scale_anomalies     |       8 |     2 |     0.25 | scientific      |
| local_vs_global_optima         |       8 |     2 |     0.25 | mathematical    |

**Note:** These constraints have many perspective configurations but produce the same type. This might indicate:
- The constraint is genuinely invariant (e.g., physical laws)
- Index dimensions are not affecting classification
- Potential data quality issue

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 100.0% | Core data for variance analysis |
| variance_ratio | 100.0% | Calculated from classifications |
| domain | 99.5% | Affects domain breakdown analysis |
