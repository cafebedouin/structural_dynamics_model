# Structural Pattern Mining

## Executive Summary

### Key Findings

1. **[HIGH]** Found 18 structural signatures shared by 5+ constraints
   - Action: Investigate if these represent distinct categories beyond current framework

2. **[HIGH]** Found 66 hybrid constraints (high extraction + high suppression)
   - Action: Strong evidence for 'Tangled Rope' category
   - Details: 66 constraints don't fit cleanly into mountain/rope/snare

3. **[MEDIUM]** Found 33 constraints matching 'tangled_rope' pattern
   - Action: Consider formalizing 'tangled_rope' as new category
   - Details: Type distribution: {'tangled_rope': 3, 'snare': 23, 'mountain': 5, 'piton': 2}

4. **[MEDIUM]** Found 50 constraints matching 'scaffold' pattern
   - Action: Consider formalizing 'scaffold' as new category
   - Details: Type distribution: {'rope': 32, 'mountain': 6, 'snare': 2, 'tangled_rope': 6, 'scaffold': 1, 'piton': 1}

5. **[MEDIUM]** Found 64 constraints matching 'wings' pattern
   - Action: Consider formalizing 'wings' as new category
   - Details: Type distribution: {'rope': 48, 'snare': 1, 'mountain': 13, 'tangled_rope': 2}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 36

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.1, 0.2, True, False)        |    18 | snare, mountain, rope | mathematics, technological, philosophical | biological_curiosity, church_turing_thesis |
| (0.2, 0.3, True, False)        |    13 | tangled_rope, rope   | technological, legal, psychological | burali_forte_paradox, central_limit_theorem_convergence |
| (0.1, 0.1, True, False)        |    11 | tangled_rope, rope, mountain | social, technological, economic | automatic_enrollment_defaults, ergo_lets_protocol |
| (0.8, 0.8, False, True)        |    10 | snare, tangled_rope, mountain | technological, unknown, legal | 26usc469_real_estate_exemption, ad_fus_coordination |
| (0.2, 0.1, True, False)        |    10 | mountain, rope       | mathematics, technological, physics | basel_problem_convergence, brouwer_fixed_point |
| (0.2, 0.4, True, False)        |     8 | snare, tangled_rope, rope | social, technological, health | advice_as_dangerous_gift, axiom_of_choice_determinacy |
| (0.5, 0.7, False, True)        |     7 | snare, piton         | social, technological, economic | ai_auditability_gap, ulysses_chp01 |
| (0.5, 0.8, False, True)        |     7 | snare, piton         | social, technological, philosophical | atrophied_optimization_piton, ulysses_chp03 |
| (0.8, 0.6, False, True)        |     6 | snare, tangled_rope  | social, technological, economic | academic_peer_review_gatekeeping, academic_tenure_system |
| (0.8, 0.7, False, True)        |     6 | snare, mountain      | social, technological, organizational | algorithmic_bias, algorithmic_epistemic_capture |
| (0.4, 0.7, True, False)        |     6 | mountain, tangled_rope, rope | social, technological, biological | availability_heuristic, cap_theorem |
| (0.1, 0.2, False, True)        |     6 | mountain, rope       | political, technological, economic | berkshire_compounding_culture, burden_of_proof_legal_criminal |
| (0.4, 0.3, True, False)        |     6 | mountain, rope       | social, technological, economic | busy_beaver_noncomputability, cow_field_poop |
| (0.3, 0.2, True, False)        |     6 | mountain, rope       | technological, economic, mathematical | cantor_set_topology, feigenbaum_universality |
| (0.4, 0.5, False, True)        |     6 | tangled_rope, rope   | social, technological, psychological | copyright_protection, fmeca_procedures_1980 |
| (0.4, 0.2, True, False)        |     5 | mountain, rope       | technological, economic, mathematical | birthday_paradox_collison, ergo_sig_usd_protocol |
| (0.2, 0.2, True, False)        |     5 | mountain, rope       | economic, mathematical | countable_infinity_cardinality, ergo_dexy_gold_protocol |
| (0.4, 0.7, False, True)        |     5 | tangled_rope, rope, mountain | legal, political, social | france_cordon_sanitaire_2026, kjv_textual_authority |
| (0.8, 0.9, False, True)        |     4 | snare, mountain, piton | biological, political, social | ad_synaptic_deficit, apartheid_nuclear_program |
| (0.2, 0.5, True, False)        |     4 | mountain, rope       | social, analytical, technological | currys_paradox, kjv_puritan_new_world_exit |

## Candidate Category Analysis

### Tangled Rope

**Pattern:** High extraction + High suppression + Requires enforcement
**Interpretation:** Mix of snare and rope characteristics

**Constraints matching pattern:** 33

**Current type distribution:**
- snare: 23
- mountain: 5
- tangled_rope: 3
- piton: 2

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| 26usc469_real_estate_exemption | tangled_rope    |           0.75 |        0.80 | legal      |
| academic_tenure_system         | snare           |           0.75 |        0.60 | economic   |
| ad_fus_coordination            | snare           |           0.75 |        0.80 | medical    |
| ad_synaptic_deficit            | mountain        |           0.85 |        0.95 | biological |
| ai_driven_surveillance_sensor_layer | tangled_rope    |           0.75 |        0.80 | technological |
| ai_evaluators_matching         | tangled_rope    |           0.75 |        0.80 | unknown    |
| ai_professional_displacement   | mountain        |           0.71 |        0.80 | economic   |
| algorithmic_bias               | snare           |           0.75 |        0.70 | technological |
| algorithmic_epistemic_capture  | snare           |           0.85 |        0.74 | technological |
| alignment_tax_tradeoff         | snare           |           0.84 |        0.62 | technological |

### Piton

**Pattern:** High suppression + Enforced + Claimed as mountain
**Interpretation:** False mountains that are obviously constructed

**Constraints matching pattern:** 8

**Current type distribution:**
- mountain: 8

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| ad_synaptic_deficit            | mountain        |           0.85 |        0.95 | biological |
| ai_professional_displacement   | mountain        |           0.71 |        0.80 | economic   |
| constitutional_supremacy       | mountain        |           0.30 |        0.95 | legal      |
| kjv_textual_authority          | mountain        |           0.40 |        0.70 | religious  |
| sts86_ascent_checklist         | mountain        |           0.05 |        0.95 | technological |
| viral_transmission_rates       | mountain        |           0.70 |        0.80 | technological |
| wikipedia_notability_requirement_2026 | mountain        |           0.40 |        0.70 | social     |
| xi_mao_ideological_centralization | mountain        |           0.75 |        0.85 | political  |

### Scaffold

**Pattern:** Medium extractiveness + Medium suppression
**Interpretation:** Temporary transition mechanisms

**Constraints matching pattern:** 50

**Current type distribution:**
- rope: 32
- mountain: 6
- tangled_rope: 6
- snare: 2
- scaffold: 1
- piton: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| ai_edu_decentralization        | rope            |           0.50 |        0.40 | technological |
| arrows_impossibility_theorem   | mountain        |           0.60 |        0.40 | political  |
| axiom_reasoner_2026            | snare           |           0.48 |        0.35 | technological |
| boiled_pineapple_trend_2026    | snare           |           0.55 |        0.60 | social     |
| burden_of_proof_scientific_empirical | rope            |           0.30 |        0.60 | technological |
| busy_beaver_noncomputability   | rope            |           0.40 |        0.30 | technological |
| climate_target_one_point_five  | tangled_rope    |           0.30 |        0.60 | political  |
| columbia_2026_elections        | rope            |           0.45 |        0.35 | political  |
| conways_game_of_life_dynamics  | rope            |           0.30 |        0.40 | technological |
| copyright_protection           | tangled_rope    |           0.40 |        0.50 | social     |

### Wings

**Pattern:** Low extraction + Low suppression + Emerges naturally
**Interpretation:** Enabling constraints, opposite of snare

**Constraints matching pattern:** 64

**Current type distribution:**
- rope: 48
- mountain: 13
- tangled_rope: 2
- snare: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| automatic_enrollment_defaults  | rope            |           0.05 |        0.10 | economic   |
| basel_problem_convergence      | rope            |           0.20 |        0.10 | mathematical |
| biological_curiosity           | snare           |           0.15 |        0.20 | biological |
| brouwer_fixed_point            | rope            |           0.20 |        0.10 | mathematics |
| buffons_needle_pi_estimation   | rope            |           0.20 |        0.10 | mathematical |
| burali_forte_paradox           | rope            |           0.20 |        0.30 | technological |
| cantor_set_topology            | mountain        |           0.30 |        0.20 | mathematical |
| chaitins_omega_undecidability  | rope            |           0.20 |        0.30 | technological |
| choice_architecture_design     | rope            |           0.20 |        0.30 | psychological |
| church_turing_thesis           | rope            |           0.10 |        0.20 | technological |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 66

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| epstein_kgb_honeytrap          | snare           |       0.92 |        0.88 |  1.80 | political  |
| ad_synaptic_deficit            | mountain        |       0.85 |        0.95 |  1.80 | biological |
| apartheid_nuclear_program      | snare           |       0.85 |        0.90 |  1.75 | political  |
| authoritarian_power_paradox    | snare           |       0.80 |        0.90 |  1.70 | political  |
| bay_of_pigs_operational_silo   | snare           |       0.90 |        0.80 |  1.70 | political  |
| riot_incentive_loop_2026       | snare           |       0.82 |        0.85 |  1.67 | political  |
| juvenile_underclass_2026       | piton           |       0.75 |        0.90 |  1.65 | social     |
| australia_social_ban_2026      | snare           |       0.72 |        0.88 |  1.60 | political  |
| minnesota_sovereignty_2026     | piton           |       0.68 |        0.92 |  1.60 | political  |
| xi_mao_ideological_centralization | mountain        |       0.75 |        0.85 |  1.60 | political  |
| epstein_espionage_crisis_2026  | piton           |       0.68 |        0.91 |  1.59 | political  |
| algorithmic_epistemic_capture  | snare           |       0.85 |        0.74 |  1.59 | technological |
| art_market_decoupling          | snare           |       0.84 |        0.75 |  1.59 | economic   |
| ancestral_pueblo_hydrology     | snare           |       0.82 |        0.75 |  1.57 | environmental |
| cultural_refragmentation_2026  | snare           |       0.72 |        0.85 |  1.57 | social     |
| 26usc469_real_estate_exemption | tangled_rope    |       0.75 |        0.80 |  1.55 | legal      |
| ad_fus_coordination            | snare           |       0.75 |        0.80 |  1.55 | medical    |
| ai_driven_surveillance_sensor_layer | tangled_rope    |       0.75 |        0.80 |  1.55 | technological |
| ai_evaluators_matching         | tangled_rope    |       0.75 |        0.80 |  1.55 | unknown    |
| amish_technological_renunciation | snare           |       0.80 |        0.75 |  1.55 | social     |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 76

| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |
|---------------|--------------|------------|-------------|------------|--------|
| adverse_possession             | snare           |       0.65 |        0.45 | N/A        | economic   |
| agentive_optimism_2026         | piton           |       0.70 |        0.65 | N/A        | political  |
| ai_auditability_gap            | snare           |       0.52 |        0.65 | N/A        | technological |
| ai_edu_decentralization        | rope            |       0.50 |        0.40 | N/A        | technological |
| ai_superpowers_race_2026       | snare           |       0.64 |        0.70 | N/A        | technological |
| altruistic_misery_paradox_2026 | snare           |       0.62 |        0.68 | N/A        | social     |
| arrows_impossibility_theorem   | mountain        |       0.60 |        0.40 | N/A        | political  |
| availability_heuristic         | mountain        |       0.40 |        0.70 | N/A        | social     |
| axiom_reasoner_2026            | snare           |       0.48 |        0.35 | N/A        | technological |
| blackstone_carried_interest_taxation | rope            |       0.30 |        0.70 | N/A        | economic   |
| boiled_pineapple_trend_2026    | snare           |       0.55 |        0.60 | N/A        | social     |
| burden_of_proof_scientific_empirical | rope            |       0.30 |        0.60 | N/A        | technological |
| busy_beaver_noncomputability   | rope            |       0.40 |        0.30 | N/A        | technological |
| cap_theorem                    | rope            |       0.40 |        0.70 | N/A        | technological |
| climate_target_one_point_five  | tangled_rope    |       0.30 |        0.60 | N/A        | political  |
| columbia_2026_elections        | rope            |       0.45 |        0.35 | N/A        | political  |
| conways_game_of_life_dynamics  | rope            |       0.30 |        0.40 | N/A        | technological |
| copyright_protection           | tangled_rope    |       0.40 |        0.50 | N/A        | social     |
| cow_field_poop                 | mountain        |       0.40 |        0.30 | N/A        | social     |
| dunbars_number                 | rope            |       0.40 |        0.50 | N/A        | social     |

## Recommendations

### 1. Found 18 structural signatures shared by 5+ constraints

**Priority:** HIGH

**Recommended Action:** Investigate if these represent distinct categories beyond current framework

**Example signatures:**
- (0.1, 0.2, True, False)
- (0.2, 0.3, True, False)
- (0.1, 0.1, True, False)

### 2. Found 66 hybrid constraints (high extraction + high suppression)

**Priority:** HIGH

**Recommended Action:** Strong evidence for 'Tangled Rope' category

**Details:** 66 constraints don't fit cleanly into mountain/rope/snare

### 3. Found 33 constraints matching 'tangled_rope' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'tangled_rope' as new category

**Details:** Type distribution: {'tangled_rope': 3, 'snare': 23, 'mountain': 5, 'piton': 2}

### 4. Found 50 constraints matching 'scaffold' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'scaffold' as new category

**Details:** Type distribution: {'rope': 32, 'mountain': 6, 'snare': 2, 'tangled_rope': 6, 'scaffold': 1, 'piton': 1}

### 5. Found 64 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'rope': 48, 'snare': 1, 'mountain': 13, 'tangled_rope': 2}

### 6. Found 76 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

