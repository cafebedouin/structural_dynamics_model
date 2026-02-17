# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 1034
- **Constraints with multiple index configs:** 1014 (98.1%)
- **High variance (>0.5):** 859 (83.1%)
- **Stable (ratio=1.0):** 567 (54.8%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |   567 |   54.8% | abstraction_boundary_overrun, abstraction_leaka... |
| 0.7-0.9         |   179 |   17.3% | absorbing_markov_chain_trap, ai_compute_capital... |
| 0.5-0.6         |   167 |   16.2% | ai_performance_watermark, alzheimers_levetirace... |
| 0.3-0.4         |    99 |    9.6% | asean_ceasefire_2011, automatic_enrollment_defa... |
| <0.3            |    22 |    2.1% | banach_fixed_point_theorem, cantor_set_topology... |
| null            |     0 |    0.0% | - |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| unknown_novel        |   2 |         1.00 |          100.0% |
| piton                |  91 |         0.98 |           97.8% |
| snare                | 640 |         0.89 |           96.2% |
| scaffold             |  21 |         0.82 |           85.7% |
| tangled_rope         |  92 |         0.78 |           82.6% |
| rope                 |  60 |         0.69 |           65.0% |
| mountain             | 128 |         0.40 |           14.8% |

## Key Findings

1. **Domain variance spread:** unknown_novel shows highest variance (1.00), while mountain shows lowest (0.40)

2. **High stability:** 54.8% of constraints are completely stable across index configs

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| abstraction_boundary_overrun   | 1.00 |       3 |     3 | snare      | tangled_rope |
| abstraction_leakage            | 1.00 |       3 |     3 | snare      | tangled_rope |
| academic_fashion_modernism_2026 | 1.00 |       3 |     3 | piton      | piton        |
| academic_peer_review_gatekeeping | 1.00 |       3 |     3 | snare      | tangled_rope |
| academic_tenure_system         | 1.00 |       3 |     3 | snare      | tangled_rope |
| access_arbitrage               | 1.00 |       2 |     2 | tangled_rope | tangled_rope |
| ad_fus_coordination            | 1.00 |       4 |     4 | snare      | tangled_rope |
| ad_synaptic_deficit            | 1.00 |       3 |     3 | snare      | tangled_rope |
| adaptive_lag_trap              | 1.00 |       4 |     4 | snare      | tangled_rope |
| adversarial_surface_inflation  | 1.00 |       4 |     4 | snare      | tangled_rope |

### Detailed Examples

**1. abstraction_boundary_overrun**
- Domain: snare
- Variance: 1.00
- Produces 3 different types across 3 index configurations
- Type distribution: {'snare': 1, 'rope': 1, 'tangled_rope': 1}

**2. abstraction_leakage**
- Domain: snare
- Variance: 1.00
- Produces 3 different types across 3 index configurations
- Type distribution: {'snare': 1, 'rope': 1, 'tangled_rope': 1}

**3. academic_fashion_modernism_2026**
- Domain: piton
- Variance: 1.00
- Produces 3 different types across 3 index configurations
- Type distribution: {'snare': 1, 'rope': 1, 'piton': 1}

## Suspicious Stability

Constraints with many index configs but low variance (possible modeling issues):

| Constraint ID | Configs | Types | Variance | Domain |
|---------------|---------|-------|----------|--------|
| treaty_land_entrenchment       |       5 |     1 |     0.20 | mountain        |

**Note:** These constraints have many perspective configurations but produce the same type. This might indicate:
- The constraint is genuinely invariant (e.g., physical laws)
- Index dimensions are not affecting classification
- Potential data quality issue

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 100.0% | Core data for variance analysis |
| variance_ratio | 100.0% | Calculated from classifications |
| domain | 100.0% | Affects domain breakdown analysis |
