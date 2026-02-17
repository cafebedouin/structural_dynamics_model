# Structural Pattern Mining

## Executive Summary

### Key Findings

1. **[HIGH]** Found 32 structural signatures shared by 5+ constraints
   - Action: Investigate if these represent distinct categories beyond current framework

2. **[HIGH]** Found 592 hybrid constraints (high extraction + high suppression)
   - Action: Strong evidence for 'Tangled Rope' category
   - Details: 592 constraints don't fit cleanly into mountain/rope/snare

3. **[MEDIUM]** Found 374 constraints matching 'tangled_rope' pattern
   - Action: Consider formalizing 'tangled_rope' as new category
   - Details: Type distribution: {'tangled_rope': 287, 'piton': 41, 'snare': 45, 'rope': 1}

4. **[MEDIUM]** Found 140 constraints matching 'scaffold' pattern
   - Action: Consider formalizing 'scaffold' as new category
   - Details: Type distribution: {'tangled_rope': 122, 'rope': 6, 'piton': 5, 'snare': 3, 'scaffold': 3, '[social_governance]': 1}

5. **[MEDIUM]** Found 135 constraints matching 'wings' pattern
   - Action: Consider formalizing 'wings' as new category
   - Details: Type distribution: {'mountain': 128, 'rope': 6, 'tangled_rope': 1}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 55

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.6, 0.7, False, True)        |    76 | piton, snare, tangled_rope | piton, snare    | ai_performance_watermark, ai_religion_regulation |
| (0.8, 0.7, False, True)        |    68 | piton, snare, tangled_rope | piton, snare    | abstraction_boundary_overrun, adaptive_lag_trap |
| (0.8, 0.8, False, True)        |    68 | piton, snare, tangled_rope | piton, snare    | ad_fus_coordination, ai_driven_surveillance_sensor_layer |
| (0.6, 0.8, False, True)        |    51 | piton, tangled_rope  | piton, snare    | ai_compute_capital_moat, airbnb_str_regulation |
| (0.9, 0.8, False, True)        |    50 | piton, snare, tangled_rope | piton, snare    | adversarial_truth_decay, agency_atrophy |
| (0.5, 0.7, False, True)        |    42 | piton, snare, tangled_rope | piton, snare    | ai_auditability_gap, ai_training_data_dependency |
| (0.7, 0.8, False, True)        |    42 | piton, snare, tangled_rope | piton, snare    | ai_banal_capture, ai_professional_displacement |
| (0.1, 0.1, True, False)        |    39 | rope, mountain       | rope, mountain  | banach_fixed_point_theorem, banach_tarski_paradox |
| (0.5, 0.8, False, True)        |    38 | scaffold, piton, tangled_rope | scaffold, piton, snare | ai_adoption_stigma, artificial_scarcity_scaffold |
| (0.8, 0.9, False, True)        |    35 | piton, snare, tangled_rope | piton, snare    | ad_synaptic_deficit, apartheid_nuclear_program |
| (0.8, 0.6, False, True)        |    24 | piton, rope, snare, tangled_rope | piton, rope, snare | academic_peer_review_gatekeeping, academic_tenure_system |
| (0.5, 0.6, False, True)        |    23 | piton, snare, tangled_rope | piton, snare    | ai_task_horizon_reliability, armra_colostrum_regulation |
| (0.4, 0.5, False, True)        |    19 | scaffold, rope, tangled_rope | scaffold, rope, tangled_rope | bgs_eigenvector_thermalization, constraint_yoneda |
| (0.7, 0.6, False, True)        |    16 | piton, snare, tangled_rope | piton, snare    | abstraction_leakage, carrying_capacity |
| (0.3, 0.5, False, True)        |    16 | scaffold, [social_governance], tangled_rope | scaffold, tangled_rope | alzheimers_levetiracetam, china_africa_zero_tariff_2026 |
| (0.2, 0.1, True, False)        |    16 | rope, mountain       | rope, mountain  | axiom_of_choice, base_pair_complementarity |
| (0.9, 0.7, False, True)        |    15 | piton, tangled_rope  | piton, snare    | bureaucratic_legibility_collapse, capital_misallocation_spiral |
| (0.7, 0.7, False, True)        |    14 | piton, tangled_rope  | piton, snare    | awareness_without_leverage, cb_far_beyond_human |
| (0.5, 0.5, False, True)        |    13 | rope, tangled_rope   | rope, snare, tangled_rope | access_arbitrage, aging_longevity_tests |
| (0.6, 0.6, False, True)        |    13 | piton, tangled_rope  | piton, snare    | boiled_pineapple_trend_2026, carbon_credit_markets_2026 |

## Candidate Category Analysis

### Tangled Rope

**Pattern:** High extraction + High suppression + Requires enforcement
**Interpretation:** Mix of snare and rope characteristics

**Constraints matching pattern:** 374

**Current type distribution:**
- tangled_rope: 287
- snare: 45
- piton: 41
- rope: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| abstraction_boundary_overrun   | tangled_rope    |           0.81 |        0.69 | snare      |
| academic_tenure_system         | tangled_rope    |           0.75 |        0.60 | snare      |
| ad_fus_coordination            | tangled_rope    |           0.75 |        0.80 | snare      |
| ad_synaptic_deficit            | tangled_rope    |           0.85 |        0.95 | snare      |
| adaptive_lag_trap              | tangled_rope    |           0.83 |        0.71 | snare      |
| adversarial_surface_inflation  | tangled_rope    |           0.84 |        0.72 | snare      |
| adversarial_truth_decay        | piton           |           0.89 |        0.78 | piton      |
| agency_atrophy                 | tangled_rope    |           0.88 |        0.79 | snare      |
| ai_banal_capture               | tangled_rope    |           0.68 |        0.75 | snare      |
| ai_compute_capital_moat        | tangled_rope    |           0.62 |        0.75 | snare      |

### Piton

**Pattern:** High suppression + Enforced + Claimed as mountain
**Interpretation:** False mountains that are obviously constructed

No constraints match this pattern.

### Scaffold

**Pattern:** Medium extractiveness + Medium suppression
**Interpretation:** Temporary transition mechanisms

**Constraints matching pattern:** 140

**Current type distribution:**
- tangled_rope: 122
- rope: 6
- piton: 5
- snare: 3
- scaffold: 3
- [social_governance]: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| access_arbitrage               | tangled_rope    |           0.45 |        0.50 | tangled_rope |
| advice_as_dangerous_gift       | tangled_rope    |           0.35 |        0.40 | tangled_rope |
| aging_longevity_tests          | tangled_rope    |           0.52 |        0.45 | snare      |
| ai_edu_decentralization        | rope            |           0.50 |        0.40 | rope       |
| ai_task_horizon_reliability    | tangled_rope    |           0.48 |        0.60 | snare      |
| alzheimers_levetiracetam       | tangled_rope    |           0.35 |        0.45 | tangled_rope |
| armra_colostrum_regulation     | tangled_rope    |           0.48 |        0.55 | snare      |
| arrows_impossibility_theorem   | tangled_rope    |           0.60 |        0.40 | snare      |
| artificial_snow_2026           | piton           |           0.48 |        0.30 | piton      |
| axiom_reasoner_2026            | tangled_rope    |           0.48 |        0.35 | snare      |

### Wings

**Pattern:** Low extraction + Low suppression + Emerges naturally
**Interpretation:** Enabling constraints, opposite of snare

**Constraints matching pattern:** 135

**Current type distribution:**
- mountain: 128
- rope: 6
- tangled_rope: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| axiom_of_choice                | mountain        |           0.25 |        0.05 | mountain   |
| banach_fixed_point             | mountain        |           0.01 |        0.01 | mountain   |
| banach_fixed_point_theorem     | mountain        |           0.10 |        0.05 | mountain   |
| banach_tarski_paradox          | mountain        |           0.05 |        0.05 | mountain   |
| base_pair_complementarity      | mountain        |           0.20 |        0.05 | mountain   |
| basel_problem_convergence      | mountain        |           0.02 |        0.01 | mountain   |
| bgs_spectral_universality      | mountain        |           0.08 |        0.03 | mountain   |
| bh_merger_gravitational_infall | mountain        |           0.05 |        0.02 | mountain   |
| biological_curiosity           | mountain        |           0.15 |        0.05 | mountain   |
| birthday_paradox_collision     | mountain        |           0.05 |        0.00 | mountain   |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 592

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| trojan_war_spoils              | snare           |       1.00 |        1.00 |  2.00 | snare      |
| gilgamesh_mortality_limit      | tangled_rope    |       1.00 |        0.90 |  1.90 | snare      |
| khantivadin_radical_patience   | tangled_rope    |       1.00 |        0.90 |  1.90 | snare      |
| north_korea_songun_mandatrophy | snare           |       0.95 |        0.95 |  1.90 | snare      |
| fiat_currency_lifecycle        | tangled_rope    |       0.95 |        0.90 |  1.85 | snare      |
| integrated_digital_governance_stack | tangled_rope    |       0.90 |        0.95 |  1.85 | snare      |
| taliban_slavery_law_2024       | snare           |       0.90 |        0.95 |  1.85 | snare      |
| technological_point_of_no_return | tangled_rope    |       0.86 |        0.98 |  1.84 | snare      |
| epstein_honeytrap              | piton           |       0.92 |        0.88 |  1.80 | piton      |
| ad_synaptic_deficit            | tangled_rope    |       0.85 |        0.95 |  1.80 | snare      |
| blackstone_tra                 | tangled_rope    |       0.85 |        0.95 |  1.80 | snare      |
| cuba_mandatrophic_collapse     | snare           |       0.95 |        0.85 |  1.80 | snare      |
| horizon_liability_contract     | tangled_rope    |       0.85 |        0.95 |  1.80 | snare      |
| iran_nin_repression            | snare           |       0.85 |        0.95 |  1.80 | snare      |
| nsl_hk                         | snare           |       0.85 |        0.95 |  1.80 | snare      |
| the_bacchae_madness_protocol   | tangled_rope    |       0.95 |        0.85 |  1.80 | snare      |
| us_venezuela_blockade          | snare           |       0.85 |        0.95 |  1.80 | snare      |
| interpretive_frame_fragmentation | tangled_rope    |       0.93 |        0.86 |  1.79 | snare      |
| delta_force_selection_2026     | tangled_rope    |       0.92 |        0.85 |  1.77 | snare      |
| intertemporal_responsibility_gap | tangled_rope    |       0.92 |        0.85 |  1.77 | snare      |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 319

| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |
|---------------|--------------|------------|-------------|------------|--------|
| absorbing_markov_chain_trap    | snare           |       0.55 |        0.65 | N/A        | snare      |
| access_arbitrage               | tangled_rope    |       0.45 |        0.50 | N/A        | tangled_rope |
| adverse_possession             | snare           |       0.65 |        0.45 | N/A        | snare      |
| advice_as_dangerous_gift       | tangled_rope    |       0.35 |        0.40 | N/A        | tangled_rope |
| agent_opt_2026                 | piton           |       0.70 |        0.65 | N/A        | unknown_novel |
| aging_longevity_tests          | tangled_rope    |       0.52 |        0.45 | N/A        | snare      |
| ai_auditability_gap            | tangled_rope    |       0.52 |        0.65 | N/A        | snare      |
| ai_edu_decentralization        | rope            |       0.50 |        0.40 | N/A        | rope       |
| ai_performance_watermark       | tangled_rope    |       0.55 |        0.65 | N/A        | snare      |
| ai_religion_regulation         | tangled_rope    |       0.60 |        0.70 | N/A        | snare      |
| ai_scholar_citation_trap       | tangled_rope    |       0.55 |        0.70 | N/A        | snare      |
| ai_superpowers_2026            | scaffold        |       0.64 |        0.70 | N/A        | scaffold   |
| ai_task_horizon_reliability    | tangled_rope    |       0.48 |        0.60 | N/A        | snare      |
| ai_training_data_dependency    | tangled_rope    |       0.48 |        0.65 | N/A        | snare      |
| altruistic_misery_paradox_2026 | piton           |       0.62 |        0.68 | N/A        | piton      |
| alzheimers_levetiracetam       | tangled_rope    |       0.35 |        0.45 | N/A        | tangled_rope |
| arctic_maritime_control        | tangled_rope    |       0.55 |        0.70 | N/A        | snare      |
| armra_colostrum_regulation     | tangled_rope    |       0.48 |        0.55 | N/A        | snare      |
| arrows_impossibility_theorem   | tangled_rope    |       0.60 |        0.40 | N/A        | snare      |
| artificial_snow_2026           | piton           |       0.48 |        0.30 | N/A        | piton      |

## Recommendations

### 1. Found 32 structural signatures shared by 5+ constraints

**Priority:** HIGH

**Recommended Action:** Investigate if these represent distinct categories beyond current framework

**Example signatures:**
- (0.6, 0.7, False, True)
- (0.8, 0.7, False, True)
- (0.8, 0.8, False, True)

### 2. Found 592 hybrid constraints (high extraction + high suppression)

**Priority:** HIGH

**Recommended Action:** Strong evidence for 'Tangled Rope' category

**Details:** 592 constraints don't fit cleanly into mountain/rope/snare

### 3. Found 374 constraints matching 'tangled_rope' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'tangled_rope' as new category

**Details:** Type distribution: {'tangled_rope': 287, 'piton': 41, 'snare': 45, 'rope': 1}

### 4. Found 140 constraints matching 'scaffold' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'scaffold' as new category

**Details:** Type distribution: {'tangled_rope': 122, 'rope': 6, 'piton': 5, 'snare': 3, 'scaffold': 3, '[social_governance]': 1}

### 5. Found 135 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'mountain': 128, 'rope': 6, 'tangled_rope': 1}

### 6. Found 319 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

