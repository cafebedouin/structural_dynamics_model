# Structural Pattern Mining

## Executive Summary

### Key Findings

1. **[HIGH]** Found 33 structural signatures shared by 5+ constraints
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

5. **[MEDIUM]** Found 137 constraints matching 'wings' pattern
   - Action: Consider formalizing 'wings' as new category
   - Details: Type distribution: {'mountain': 128, 'rope': 8, 'tangled_rope': 1}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 55

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.6, 0.7, False, True)        |    76 | piton, snare, tangled_rope | bio_industrial, social/cognitive, economic/political | ai_performance_watermark, ai_religion_regulation |
| (0.8, 0.7, False, True)        |    68 | piton, snare, tangled_rope | political/social, technological/computational, biological/technological/social | abstraction_boundary_overrun, adaptive_lag_trap |
| (0.8, 0.8, False, True)        |    68 | piton, snare, tangled_rope | geopolitical/technological, political/social, economic/technological/infrastructural | ad_fus_coordination, ai_driven_surveillance_sensor_layer |
| (0.6, 0.8, False, True)        |    51 | piton, tangled_rope  | geopolitical/technological, social/cognitive, economic/political | ai_compute_capital_moat, airbnb_str_regulation |
| (0.9, 0.8, False, True)        |    50 | piton, snare, tangled_rope | technological/infrastructural/economic, social/informational/technological, economic/psychological/technological | adversarial_truth_decay, agency_atrophy |
| (0.5, 0.7, False, True)        |    42 | piton, snare, tangled_rope | geopolitical/technological, social/economic/linguistic, social/political/religious | ai_auditability_gap, ai_training_data_dependency |
| (0.7, 0.8, False, True)        |    42 | piton, snare, tangled_rope | political/social, social/philosophical, geopolitical/maritime | ai_banal_capture, ai_professional_displacement |
| (0.1, 0.1, True, False)        |    40 | mountain, rope       | biological/technological/social, mathematical / logical, technological/computational | banach_fixed_point_theorem, banach_tarski_paradox |
| (0.5, 0.8, False, True)        |    38 | piton, scaffold, tangled_rope | political/social, philosophical/social/technological, social/political/religious | ai_adoption_stigma, artificial_scarcity_scaffold |
| (0.8, 0.9, False, True)        |    35 | piton, snare, tangled_rope | political/social, economic/political, political/military/technological | ad_synaptic_deficit, apartheid_nuclear_program |
| (0.8, 0.6, False, True)        |    24 | piton, snare, tangled_rope, rope | social/economic/technological, technological/social, social/cultural | academic_peer_review_gatekeeping, academic_tenure_system |
| (0.5, 0.6, False, True)        |    23 | piton, snare, tangled_rope | religious/social, social/economic/religious, economic, technological, geopolitical | ai_task_horizon_reliability, armra_colostrum_regulation |
| (0.4, 0.5, False, True)        |    19 | rope, scaffold, tangled_rope | social/psychological, economic/legal/technological, political | bgs_eigenvector_thermalization, constraint_yoneda |
| (0.7, 0.6, False, True)        |    16 | piton, snare, tangled_rope | political, psychological/social, technological/social | abstraction_leakage, carrying_capacity |
| (0.3, 0.5, False, True)        |    16 | [social_governance], scaffold, tangled_rope | social/psychological, political, economic | alzheimers_levetiracetam, china_africa_zero_tariff_2026 |
| (0.2, 0.1, True, False)        |    16 | mountain, rope       | mathematical/logical, mathematical, mathematical/technological | axiom_of_choice, base_pair_complementarity |
| (0.9, 0.7, False, True)        |    15 | piton, tangled_rope  | social/psychological, technological/cybernetic/organizational, political/organizational/informational | bureaucratic_legibility_collapse, capital_misallocation_spiral |
| (0.7, 0.7, False, True)        |    14 | piton, tangled_rope  | political/technological, technological/social, psychological/economic | awareness_without_leverage, cb_far_beyond_human |
| (0.5, 0.5, False, True)        |    13 | rope, tangled_rope   | health/economic, political/technological, mathematical/technological | access_arbitrage, aging_longevity_tests |
| (0.6, 0.6, False, True)        |    13 | piton, tangled_rope  | social/wellness/technological, political/social, political | boiled_pineapple_trend_2026, carbon_credit_markets_2026 |

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
| abstraction_boundary_overrun   | tangled_rope    |           0.81 |        0.69 | technological/computational |
| academic_tenure_system         | tangled_rope    |           0.75 |        0.60 | economic/social |
| ad_fus_coordination            | tangled_rope    |           0.75 |        0.80 | medical/neurological |
| ad_synaptic_deficit            | tangled_rope    |           0.85 |        0.95 | biological/technological |
| adaptive_lag_trap              | tangled_rope    |           0.83 |        0.71 | economic/technological/regulatory |
| adversarial_surface_inflation  | tangled_rope    |           0.84 |        0.72 | technological/cybernetic/security |
| adversarial_truth_decay        | piton           |           0.89 |        0.78 | social/technological/political |
| agency_atrophy                 | tangled_rope    |           0.88 |        0.79 | technological/cognitive |
| ai_banal_capture               | tangled_rope    |           0.68 |        0.75 | technological/social |
| ai_compute_capital_moat        | tangled_rope    |           0.62 |        0.75 | technological/economic |

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
| advice_as_dangerous_gift       | tangled_rope    |           0.35 |        0.40 | social/philosophical |
| aging_longevity_tests          | tangled_rope    |           0.52 |        0.45 | health/economic |
| ai_edu_decentralization        | rope            |           0.50 |        0.40 | technological/educational/economic |
| ai_task_horizon_reliability    | tangled_rope    |           0.48 |        0.60 | technological/economic |
| alzheimers_levetiracetam       | tangled_rope    |           0.35 |        0.45 | social     |
| armra_colostrum_regulation     | tangled_rope    |           0.48 |        0.55 | economic   |
| arrows_impossibility_theorem   | tangled_rope    |           0.60 |        0.40 | political/economic |
| artificial_snow_2026           | piton           |           0.48 |        0.30 | environmental/cultural |
| axiom_reasoner_2026            | tangled_rope    |           0.48 |        0.35 | technological/scientific |

### Wings

**Pattern:** Low extraction + Low suppression + Emerges naturally
**Interpretation:** Enabling constraints, opposite of snare

**Constraints matching pattern:** 137

**Current type distribution:**
- mountain: 128
- rope: 8
- tangled_rope: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| axiom_of_choice                | mountain        |           0.25 |        0.05 | mathematical/logical |
| banach_fixed_point             | mountain        |           0.01 |        0.01 | mathematical/logical |
| banach_fixed_point_theorem     | mountain        |           0.10 |        0.05 | technological |
| banach_tarski_paradox          | mountain        |           0.05 |        0.05 | mathematical/logical |
| base_pair_complementarity      | mountain        |           0.20 |        0.05 | biological/chemical |
| basel_problem_convergence      | mountain        |           0.02 |        0.01 | mathematical |
| bgs_spectral_universality      | mountain        |           0.08 |        0.03 | scientific (mathematical physics / quantum chaos) |
| bh_merger_gravitational_infall | mountain        |           0.05 |        0.02 | physical   |
| biological_curiosity           | mountain        |           0.15 |        0.05 | biological/technological/social |
| birthday_paradox_collision     | mountain        |           0.05 |        0.00 | mathematical/technological |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 592

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| trojan_war_spoils              | snare           |       1.00 |        1.00 |  2.00 | military/social |
| gilgamesh_mortality_limit      | tangled_rope    |       1.00 |        0.90 |  1.90 | philosophical/religious |
| khantivadin_radical_patience   | tangled_rope    |       1.00 |        0.90 |  1.90 | religious/ethical |
| north_korea_songun_mandatrophy | snare           |       0.95 |        0.95 |  1.90 | political/economic |
| fiat_currency_lifecycle        | tangled_rope    |       0.95 |        0.90 |  1.85 | economic/political |
| integrated_digital_governance_stack | tangled_rope    |       0.90 |        0.95 |  1.85 | technological/political |
| taliban_slavery_law_2024       | snare           |       0.90 |        0.95 |  1.85 | political/legal |
| technological_point_of_no_return | tangled_rope    |       0.86 |        0.98 |  1.84 | technological/social |
| epstein_honeytrap              | piton           |       0.92 |        0.88 |  1.80 | political/intelligence |
| ad_synaptic_deficit            | tangled_rope    |       0.85 |        0.95 |  1.80 | biological/technological |
| blackstone_tra                 | tangled_rope    |       0.85 |        0.95 |  1.80 | economic   |
| cuba_mandatrophic_collapse     | snare           |       0.95 |        0.85 |  1.80 | political/economic/technological |
| horizon_liability_contract     | tangled_rope    |       0.85 |        0.95 |  1.80 | economic/technological/legal |
| iran_nin_repression            | snare           |       0.85 |        0.95 |  1.80 | technological/political |
| nsl_hk                         | snare           |       0.85 |        0.95 |  1.80 | political/legal |
| the_bacchae_madness_protocol   | tangled_rope    |       0.95 |        0.85 |  1.80 | religious/political/social |
| us_venezuela_blockade          | snare           |       0.85 |        0.95 |  1.80 | geopolitical/economic |
| interpretive_frame_fragmentation | tangled_rope    |       0.93 |        0.86 |  1.79 | social/informational/technological |
| delta_force_selection_2026     | tangled_rope    |       0.92 |        0.85 |  1.77 | military/special_operations |
| intertemporal_responsibility_gap | tangled_rope    |       0.92 |        0.85 |  1.77 | environmental/economic/technological |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 319

| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |
|---------------|--------------|------------|-------------|------------|--------|
| absorbing_markov_chain_trap    | snare           |       0.55 |        0.65 | N/A        | technological |
| access_arbitrage               | tangled_rope    |       0.45 |        0.50 | N/A        | tangled_rope |
| adverse_possession             | snare           |       0.65 |        0.45 | N/A        | economic/political/social |
| advice_as_dangerous_gift       | tangled_rope    |       0.35 |        0.40 | N/A        | social/philosophical |
| agent_opt_2026                 | piton           |       0.70 |        0.65 | N/A        | political/social |
| aging_longevity_tests          | tangled_rope    |       0.52 |        0.45 | N/A        | health/economic |
| ai_auditability_gap            | tangled_rope    |       0.52 |        0.65 | N/A        | technological |
| ai_edu_decentralization        | rope            |       0.50 |        0.40 | N/A        | technological/educational/economic |
| ai_performance_watermark       | tangled_rope    |       0.55 |        0.65 | N/A        | technological/economic |
| ai_religion_regulation         | tangled_rope    |       0.60 |        0.70 | N/A        | technological |
| ai_scholar_citation_trap       | tangled_rope    |       0.55 |        0.70 | N/A        | technological |
| ai_superpowers_2026            | scaffold        |       0.64 |        0.70 | N/A        | technological/geopolitical |
| ai_task_horizon_reliability    | tangled_rope    |       0.48 |        0.60 | N/A        | technological/economic |
| ai_training_data_dependency    | tangled_rope    |       0.48 |        0.65 | N/A        | technological |
| altruistic_misery_paradox_2026 | piton           |       0.62 |        0.68 | N/A        | social/psychological |
| alzheimers_levetiracetam       | tangled_rope    |       0.35 |        0.45 | N/A        | social     |
| arctic_maritime_control        | tangled_rope    |       0.55 |        0.70 | N/A        | geopolitical |
| armra_colostrum_regulation     | tangled_rope    |       0.48 |        0.55 | N/A        | economic   |
| arrows_impossibility_theorem   | tangled_rope    |       0.60 |        0.40 | N/A        | political/economic |
| artificial_snow_2026           | piton           |       0.48 |        0.30 | N/A        | environmental/cultural |

## Recommendations

### 1. Found 33 structural signatures shared by 5+ constraints

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

### 5. Found 137 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'mountain': 128, 'rope': 8, 'tangled_rope': 1}

### 6. Found 319 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

