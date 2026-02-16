# Structural Pattern Mining

## Executive Summary

### Key Findings

1. **[HIGH]** Found 32 structural signatures shared by 5+ constraints
   - Action: Investigate if these represent distinct categories beyond current framework

2. **[HIGH]** Found 592 hybrid constraints (high extraction + high suppression)
   - Action: Strong evidence for 'Tangled Rope' category
   - Details: 592 constraints don't fit cleanly into mountain/rope/snare

3. **[MEDIUM]** Found 380 constraints matching 'tangled_rope' pattern
   - Action: Consider formalizing 'tangled_rope' as new category
   - Details: Type distribution: {'rope': 2, 'tangled_rope': 287, 'piton': 46, 'snare': 45}

4. **[MEDIUM]** Found 137 constraints matching 'scaffold' pattern
   - Action: Consider formalizing 'scaffold' as new category
   - Details: Type distribution: {'tangled_rope': 117, 'rope': 9, 'piton': 5, 'snare': 3, 'scaffold': 3}

5. **[MEDIUM]** Found 108 constraints matching 'wings' pattern
   - Action: Consider formalizing 'wings' as new category
   - Details: Type distribution: {'mountain': 102, 'rope': 6}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 55

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.6, 0.7, False, True)        |    81 | piton, tangled_rope, snare | social, bio_industrial, political | MOLTBOT_RELIGION, absorbing_markov_chains |
| (0.8, 0.8, False, True)        |    69 | piton, tangled_rope, rope, snare | Political, organizational, unknown | 26usc469_real_estate_exemption, ad_fus_coordination |
| (0.8, 0.7, False, True)        |    68 | piton, tangled_rope, snare | institutional, organizational, social | abstraction_boundary_overrun, adaptive_lag_trap |
| (0.6, 0.8, False, True)        |    51 | piton, tangled_rope  | social, legal, political | ai_compute_capital_moat, ape_cognition_framework |
| (0.9, 0.8, False, True)        |    50 | piton, tangled_rope, snare | logistical, organizational, religious | adversarial_truth_decay, agency_atrophy |
| (0.7, 0.8, False, True)        |    44 | piton, tangled_rope, snare | social, philosophical, political | CG_IsraelGaza_20231012, academic_fashion_modernism_2026 |
| (0.5, 0.7, False, True)        |    43 | piton, tangled_rope, snare | health, social, legal | ai_auditability_gap, ai_training_data_dependency |
| (0.5, 0.8, False, True)        |    38 | piton, tangled_rope, scaffold | social, philosophical, political | ai_adoption_stigma, artificial_scarcity_scaffold |
| (0.8, 0.9, False, True)        |    35 | piton, tangled_rope, snare | religious, social, legal | ad_synaptic_deficit, apartheid_nuclear_program |
| (0.1, 0.1, True, False)        |    29 | mountain, rope       | sociological, epistemological, mathematical | banach_fixed_point_theorem, banach_tarski_paradox |
| (0.8, 0.6, False, True)        |    24 | piton, tangled_rope, rope, snare | social, political, environmental | academic_peer_review_gatekeeping, academic_tenure_system |
| (0.5, 0.6, False, True)        |    24 | piton, tangled_rope, snare | religious, mathematical, social | ai_task_horizon_reliability, armra_colostrum_regulation |
| (0.4, 0.5, False, True)        |    19 | tangled_rope, rope, scaffold | mathematical, social, legal | bgs_eigenvector_thermalization, copyright_protection |
| (0.1, 0.1, False, True)        |    18 | mountain, tangled_rope, rope | mathematical, philosophical, economic | automatic_enrollment_defaults, biological_curiosity |
| (0.7, 0.6, False, True)        |    16 | piton, tangled_rope, snare | social, political, economic | abstraction_leakage, carrying_capacity |
| (0.7, 0.7, False, True)        |    16 | piton, tangled_rope  | social, political, economic | awareness_without_leverage, cb_far_beyond_human |
| (0.3, 0.5, False, True)        |    15 | tangled_rope, scaffold | mathematical, social, political | alzheimers_levetiracetam, fnl_shadow_probe |
| (0.9, 0.7, False, True)        |    15 | piton, tangled_rope  | organizational, social, informational | bureaucratic_legibility_collapse, capital_misallocation_spiral |
| (0.2, 0.1, True, False)        |    14 | mountain, rope       | mathematical, biological, scientific | axiom_of_choice_determinacy, base_pair_complementarity |
| (0.6, 0.6, False, True)        |    13 | piton, tangled_rope  | organizational, social, political | boiled_pineapple_trend_2026, carbon_credit_markets_2026 |

## Candidate Category Analysis

### Tangled Rope

**Pattern:** High extraction + High suppression + Requires enforcement
**Interpretation:** Mix of snare and rope characteristics

**Constraints matching pattern:** 380

**Current type distribution:**
- tangled_rope: 287
- piton: 46
- snare: 45
- rope: 2

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| 26usc469_real_estate_exemption | rope            |           0.75 |        0.80 | economic   |
| CG_IsraelGaza_20231012         | tangled_rope    |           0.65 |        0.80 | political  |
| MOLTBOT_RELIGION               | tangled_rope    |           0.60 |        0.70 | technological |
| abstraction_boundary_overrun   | tangled_rope    |           0.81 |        0.69 | technological |
| academic_fashion_modernism_2026 | piton           |           0.68 |        0.78 | social     |
| academic_tenure_system         | tangled_rope    |           0.75 |        0.60 | economic   |
| ad_fus_coordination            | tangled_rope    |           0.75 |        0.80 | medical    |
| ad_synaptic_deficit            | tangled_rope    |           0.85 |        0.95 | biological |
| adaptive_lag_trap              | tangled_rope    |           0.83 |        0.71 | economic   |
| adversarial_surface_inflation  | tangled_rope    |           0.84 |        0.72 | technological |

### Piton

**Pattern:** High suppression + Enforced + Claimed as mountain
**Interpretation:** False mountains that are obviously constructed

No constraints match this pattern.

### Scaffold

**Pattern:** Medium extractiveness + Medium suppression
**Interpretation:** Temporary transition mechanisms

**Constraints matching pattern:** 137

**Current type distribution:**
- tangled_rope: 117
- rope: 9
- piton: 5
- snare: 3
- scaffold: 3

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| advice_as_dangerous_gift       | tangled_rope    |           0.35 |        0.40 | social     |
| aging_well_assessment          | tangled_rope    |           0.52 |        0.45 | health     |
| ai_edu_decentralization        | rope            |           0.50 |        0.40 | technological |
| ai_task_horizon_reliability    | tangled_rope    |           0.48 |        0.60 | technological |
| alzheimers_levetiracetam       | tangled_rope    |           0.35 |        0.45 | social     |
| armra_colostrum_regulation     | tangled_rope    |           0.48 |        0.55 | economic   |
| arrows_impossibility_theorem   | tangled_rope    |           0.60 |        0.40 | political  |
| artificial_snow_2026           | piton           |           0.48 |        0.30 | environmental |
| axiom_reasoner_2026            | tangled_rope    |           0.48 |        0.35 | technological |
| bayes_theorem                  | tangled_rope    |           0.48 |        0.50 | social     |

### Wings

**Pattern:** Low extraction + Low suppression + Emerges naturally
**Interpretation:** Enabling constraints, opposite of snare

**Constraints matching pattern:** 108

**Current type distribution:**
- mountain: 102
- rope: 6

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| axiom_of_choice_determinacy    | mountain        |           0.25 |        0.05 | mathematical |
| banach_fixed_point             | mountain        |           0.01 |        0.01 | mathematical |
| banach_fixed_point_theorem     | mountain        |           0.10 |        0.05 | technological |
| banach_tarski_paradox          | mountain        |           0.05 |        0.05 | mathematical |
| base_pair_complementarity      | mountain        |           0.20 |        0.05 | biological |
| basel_problem_convergence      | mountain        |           0.02 |        0.01 | mathematical |
| bgs_spectral_universality      | mountain        |           0.08 |        0.03 | scientific |
| bh_merger_gravitational_infall | mountain        |           0.05 |        0.02 | physical   |
| birthday_paradox_collison      | mountain        |           0.05 |        0.00 | mathematical |
| brouwer_fixed_point            | mountain        |           0.05 |        0.01 | mathematics |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 592

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| trojan_war_spoils              | snare           |       1.00 |        1.00 |  2.00 | military   |
| gilgamesh_mortality_limit      | tangled_rope    |       1.00 |        0.90 |  1.90 | philosophical |
| khantivadin_radical_patience   | tangled_rope    |       1.00 |        0.90 |  1.90 | religious  |
| north_korea_songun_mandatrophy | snare           |       0.95 |        0.95 |  1.90 | political  |
| fiat_currency_lifecycle        | tangled_rope    |       0.95 |        0.90 |  1.85 | economic   |
| integrated_digital_governance_stack | tangled_rope    |       0.90 |        0.95 |  1.85 | technological |
| taliban_slavery_law_2024       | snare           |       0.90 |        0.95 |  1.85 | political  |
| technological_point_of_no_return | tangled_rope    |       0.86 |        0.98 |  1.84 | technological |
| epstein_kgb_honeytrap          | piton           |       0.92 |        0.88 |  1.80 | political  |
| ad_synaptic_deficit            | tangled_rope    |       0.85 |        0.95 |  1.80 | biological |
| blackstone_tax_receiveable_agreement | tangled_rope    |       0.85 |        0.95 |  1.80 | economic   |
| cuba_mandatrophic_collapse     | snare           |       0.95 |        0.85 |  1.80 | political  |
| horizon_liability_contract     | tangled_rope    |       0.85 |        0.95 |  1.80 | economic   |
| iran_nin_repression            | snare           |       0.85 |        0.95 |  1.80 | technological |
| nsl_hk                         | snare           |       0.85 |        0.95 |  1.80 | political  |
| the_bacchae_madness_protocol   | tangled_rope    |       0.95 |        0.85 |  1.80 | religious  |
| us_venezuela_blockade          | snare           |       0.85 |        0.95 |  1.80 | geopolitical |
| interpretive_frame_fragmentation | tangled_rope    |       0.93 |        0.86 |  1.79 | social     |
| delta_force_selection_2026     | tangled_rope    |       0.92 |        0.85 |  1.77 | military   |
| intertemporal_responsibility_gap | tangled_rope    |       0.92 |        0.85 |  1.77 | environmental |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 316

| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |
|---------------|--------------|------------|-------------|------------|--------|
| 8k_tv_limit_2026               | rope            |       0.70 |        0.50 | N/A        | technological |
| MOLTBOT_RELIGION               | tangled_rope    |       0.60 |        0.70 | N/A        | technological |
| absorbing_markov_chains        | snare           |       0.55 |        0.65 | N/A        | technological |
| adverse_possession             | snare           |       0.65 |        0.45 | N/A        | economic   |
| advice_as_dangerous_gift       | tangled_rope    |       0.35 |        0.40 | N/A        | social     |
| agentive_optimism_2026         | piton           |       0.70 |        0.65 | N/A        | political  |
| aging_well_assessment          | tangled_rope    |       0.52 |        0.45 | N/A        | health     |
| ai_auditability_gap            | tangled_rope    |       0.52 |        0.65 | N/A        | technological |
| ai_edu_decentralization        | rope            |       0.50 |        0.40 | N/A        | technological |
| ai_performance_watermark       | tangled_rope    |       0.55 |        0.65 | N/A        | technological |
| ai_religion_regulation         | tangled_rope    |       0.60 |        0.70 | N/A        | technological |
| ai_scholar_citation_trap       | tangled_rope    |       0.55 |        0.70 | N/A        | technological |
| ai_superpowers_race_2026       | scaffold        |       0.64 |        0.70 | N/A        | technological |
| ai_task_horizon_reliability    | tangled_rope    |       0.48 |        0.60 | N/A        | technological |
| ai_training_data_dependency    | tangled_rope    |       0.48 |        0.65 | N/A        | technological |
| altruistic_misery_paradox_2026 | piton           |       0.62 |        0.68 | N/A        | social     |
| alzheimers_levetiracetam       | tangled_rope    |       0.35 |        0.45 | N/A        | social     |
| arctic_maritime_control        | tangled_rope    |       0.55 |        0.70 | N/A        | geopolitical |
| armra_colostrum_regulation     | tangled_rope    |       0.48 |        0.55 | N/A        | economic   |
| arrows_impossibility_theorem   | tangled_rope    |       0.60 |        0.40 | N/A        | political  |

## Recommendations

### 1. Found 32 structural signatures shared by 5+ constraints

**Priority:** HIGH

**Recommended Action:** Investigate if these represent distinct categories beyond current framework

**Example signatures:**
- (0.6, 0.7, False, True)
- (0.8, 0.8, False, True)
- (0.8, 0.7, False, True)

### 2. Found 592 hybrid constraints (high extraction + high suppression)

**Priority:** HIGH

**Recommended Action:** Strong evidence for 'Tangled Rope' category

**Details:** 592 constraints don't fit cleanly into mountain/rope/snare

### 3. Found 380 constraints matching 'tangled_rope' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'tangled_rope' as new category

**Details:** Type distribution: {'rope': 2, 'tangled_rope': 287, 'piton': 46, 'snare': 45}

### 4. Found 137 constraints matching 'scaffold' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'scaffold' as new category

**Details:** Type distribution: {'tangled_rope': 117, 'rope': 9, 'piton': 5, 'snare': 3, 'scaffold': 3}

### 5. Found 108 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'mountain': 102, 'rope': 6}

### 6. Found 316 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

