# Structural Pattern Mining

## Executive Summary

### Key Findings

1. **[HIGH]** Found 23 structural signatures shared by 5+ constraints
   - Action: Investigate if these represent distinct categories beyond current framework

2. **[HIGH]** Found 168 hybrid constraints (high extraction + high suppression)
   - Action: Strong evidence for 'Tangled Rope' category
   - Details: 168 constraints don't fit cleanly into mountain/rope/noose

3. **[MEDIUM]** Found 99 constraints matching 'tangled_rope' pattern
   - Action: Consider formalizing 'tangled_rope' as new category
   - Details: Type distribution: {'rope': 11, 'noose': 47, 'mountain': 13}

4. **[MEDIUM]** Found 18 constraints matching 'zombie' pattern
   - Action: Consider formalizing 'zombie' as new category
   - Details: Type distribution: {'mountain': 18}

5. **[MEDIUM]** Found 93 constraints matching 'scaffold' pattern
   - Action: Consider formalizing 'scaffold' as new category
   - Details: Type distribution: {'rope': 20, 'mountain': 39, 'noose': 22}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 44

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.8, 0.7, False, True)        |    19 | noose, mountain, rope | biological, political, economic | cbdc_implementation, mandatrophic_margin_collapse |
| (None, None, None, None)       |    17 | noose, rope          | narrative, technical, unknown | rope_design, colombia_2026_presidential_election |
| (0.8, 0.8, False, True)        |    16 | noose, mountain, rope | environmental, political, economic | ai_driven_surveillance_sensor_layer, arctic_geopolitical_flashpoint |
| (0.1, 0.2, True, False)        |    16 | noose, mountain, rope | biological, mathematics, technological | biological_curiosity, church_turing_thesis |
| (0.2, 0.3, True, False)        |    13 | noose, mountain, rope | psychological, scientific, legal | central_limit_theorem_convergence, chaitins_omega_undecidability |
| (0.8, 0.9, False, True)        |    11 | noose, mountain      | legal, political, economic | apartheid_nuclear_program, authoritarian_power_paradox |
| (0.1, 0.1, True, False)        |    11 | noose, mountain, rope | technological, mathematical, economic | automatic_enrollment_defaults, ergo_lets_protocol |
| (0.2, 0.1, True, False)        |    10 | noose, mountain, rope | technological, mathematics, mathematical | basel_problem_convergence, brouwer_fixed_point |
| (0.2, 0.4, True, False)        |     9 | noose, mountain      | health, technological, mathematical | axiom_of_choice_determinacy, banach_tarski_paradox |
| (0.7, 0.8, False, True)        |     8 | noose, mountain, rope | political, economic, technological | ai_professional_displacement, cia_fbi_legal_wall |
| (0.3, 0.4, True, False)        |     8 | noose, mountain      | biological, economic, technological | conways_game_of_life_dynamics, endowment_effect |
| (0.4, 0.5, False, True)        |     8 | noose, rope          | psychological, religious, economic | copyright_protection, fmeca_procedures_1980 |
| (0.4, 0.5, True, False)        |     8 | noose, mountain, rope | biological, organizational, technological | dunbars_number, graph_coloring_complexity |
| (0.5, 0.7, True, False)        |     7 | noose, mountain      | technological, biological, economic | goodharts_law, innovators_dilemma |
| (0.4, 0.7, True, False)        |     6 | noose, mountain      | biological, technological, social | availability_heuristic, cap_theorem |
| (0.2, 0.2, True, False)        |     6 | mountain, rope       | economic, mathematics, mathematical | banach_fixed_point, countable_infinity_cardinality |
| (0.1, 0.2, False, True)        |     6 | noose, rope          | economic, technological, political | berkshire_compounding_culture, burden_of_proof_legal_criminal |
| (0.3, 0.2, True, False)        |     6 | noose, mountain, rope | economic, technological, mathematical | cantor_set_topology, feigenbaum_universality |
| (0.7, 0.6, False, True)        |     6 | noose, mountain      | economic, medical, political | carrying_capacity, colorado_sbe_decentralization_friction |
| (0.5, 0.6, True, False)        |     5 | noose, mountain      | economic, technological | ai_task_horizon_reliability, network_effects |

## Candidate Category Analysis

### Tangled Rope

**Pattern:** High extraction + High suppression + Requires enforcement
**Interpretation:** Mix of noose and rope - extractive but coordinated

**Constraints matching pattern:** 99

**Current type distribution:**
- noose: 47
- mountain: 13
- rope: 11

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| academic_tenure_system         | rope            |           0.75 |        0.60 | social     |
| ai_driven_surveillance_sensor_layer | rope            |           0.75 |        0.80 | technological |
| ai_professional_displacement   | noose           |           0.70 |        0.80 | economic   |
| algorithmic_bias               | noose           |           0.60 |        0.70 | technological |
| apartheid_nuclear_program      | noose           |           0.85 |        0.90 | political  |
| arctic_geopolitical_flashpoint | noose           |           0.75 |        0.80 | geopolitical |
| authoritarian_power_paradox    | noose           |           0.80 |        0.90 | political  |
| blackstone_conflicts_of_interest | noose           |           0.75 |        0.85 | corporate_governance |
| blackstone_smd_control         | noose           |           0.80 |        0.90 | corporate_governance |
| burden_of_proof_engineering_safety | noose           |           0.90 |        0.80 | technological |

### Zombie

**Pattern:** High suppression + Enforced + Claimed as mountain
**Interpretation:** False mountains that are obviously constructed

**Constraints matching pattern:** 18

**Current type distribution:**
- mountain: 18

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| china_taiwan_reunification_mandate | mountain        |           0.80 |        0.90 | political  |
| cia_fbi_legal_wall             | mountain        |           0.70 |        0.85 | political  |
| coinbase_regulatory_uncertainty | mountain        |           0.70 |        0.90 | political  |
| constitutional_supremacy       | mountain        |           0.30 |        0.95 | legal      |
| couples_residency_match        | mountain        |           0.50 |        0.95 | technological |
| ergo_autolykos_asic_resistance | mountain        |           0.20 |        0.85 | technological |
| greshams_law                   | mountain        |           0.60 |        0.70 | economic   |
| kidney_exchange_market         | mountain        |           0.10 |        0.90 | social     |
| metamorphosis_samsa            | mountain        |           0.80 |        0.70 | biological |
| overton_window                 | mountain        |           0.40 |        0.80 | political  |

### Scaffold

**Pattern:** Medium extractiveness + Medium suppression
**Interpretation:** Temporary transition mechanisms

**Constraints matching pattern:** 93

**Current type distribution:**
- mountain: 39
- noose: 22
- rope: 20

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| ai_edu_decentralization        | rope            |           0.50 |        0.40 | technological |
| ai_task_horizon_reliability    | mountain        |           0.45 |        0.60 | technological |
| antifragility                  | mountain        |           0.40 |        0.60 | technological |
| arrows_impossibility_theorem   | mountain        |           0.60 |        0.40 | political  |
| burden_of_proof_scientific_empirical | noose           |           0.30 |        0.60 | technological |
| busy_beaver_noncomputability   | mountain        |           0.40 |        0.30 | technological |
| carbon_credit_markets_2026     | rope            |           0.55 |        0.60 | economic   |
| cobra_effect                   | noose           |           0.60 |        0.40 | economic   |
| compounding_logic              | rope            |           0.50 |        0.40 | economic   |
| conways_game_of_life_dynamics  | mountain        |           0.30 |        0.40 | technological |

### Wings

**Pattern:** Low extraction + Low suppression + Emerges naturally
**Interpretation:** Enabling constraints, opposite of noose

**Constraints matching pattern:** 63

**Current type distribution:**
- mountain: 34
- rope: 13
- noose: 11

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| automatic_enrollment_defaults  | rope            |           0.05 |        0.10 | economic   |
| banach_fixed_point             | mountain        |           0.20 |        0.20 | mathematics |
| basel_problem_convergence      | noose           |           0.20 |        0.10 | mathematical |
| biological_curiosity           | noose           |           0.15 |        0.20 | biological |
| brouwer_fixed_point            | mountain        |           0.20 |        0.10 | mathematics |
| buffons_needle_pi_estimation   | mountain        |           0.20 |        0.10 | mathematical |
| cantor_set_topology            | mountain        |           0.30 |        0.20 | mathematical |
| chaitins_omega_undecidability  | mountain        |           0.20 |        0.30 | technological |
| church_turing_thesis           | mountain        |           0.10 |        0.20 | technological |
| collatz_conjecture_determinism | mountain        |           0.15 |        0.20 | mathematical |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 168

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| trojan_war_spoils              | noose           |       1.00 |        1.00 |  2.00 | military   |
| gilgamesh_mortality_limit      | mountain        |       1.00 |        0.90 |  1.90 | philosophical |
| khantivadin_radical_patience   | N/A             |       1.00 |        0.90 |  1.90 | religious  |
| integrated_digital_governance_stack | rope            |       0.90 |        0.95 |  1.85 | systemic   |
| north_korea_songun_mandatrophy | noose           |       0.95 |        0.90 |  1.85 | political  |
| cuba_mandatrophic_collapse     | noose           |       0.95 |        0.85 |  1.80 | political  |
| the_bacchae_madness_protocol   | noose           |       0.95 |        0.85 |  1.80 | religious  |
| blackstone_tax_receiveable_agreement | noose           |       0.85 |        0.95 |  1.80 | economic   |
| apartheid_nuclear_program      | noose           |       0.85 |        0.90 |  1.75 | political  |
| dark_patterns_manipulation     | noose           |       0.85 |        0.90 |  1.75 | technological |
| frankenstein_creation_hubris   | noose           |       0.90 |        0.85 |  1.75 | technological |
| iran_mandatrophic_collapse     | noose           |       0.90 |        0.85 |  1.75 | political  |
| lehman_repo_105                | N/A             |       0.90 |        0.85 |  1.75 | economic   |
| trumps_second_term_authoritarianism_2026 | noose           |       0.85 |        0.90 |  1.75 | political  |
| authoritarian_power_paradox    | noose           |       0.80 |        0.90 |  1.70 | political  |
| blackstone_smd_control         | noose           |       0.80 |        0.90 |  1.70 | corporate_governance |
| burden_of_proof_engineering_safety | noose           |       0.90 |        0.80 |  1.70 | technological |
| china_taiwan_reunification_mandate | mountain        |       0.80 |        0.90 |  1.70 | political  |
| dionysiac_frenzy               | noose           |       0.80 |        0.90 |  1.70 | unknown    |
| faint_blue_neural_bifurcation  | noose           |       0.90 |        0.80 |  1.70 | technological |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 143

| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |
|---------------|--------------|------------|-------------|------------|--------|
| adverse_possession             | noose           |       0.70 |        0.40 | N/A        | economic   |
| ai_edu_decentralization        | rope            |       0.50 |        0.40 | N/A        | technological |
| ai_task_horizon_reliability    | mountain        |       0.45 |        0.60 | N/A        | technological |
| algorithmic_bias               | noose           |       0.60 |        0.70 | N/A        | technological |
| antifragility                  | mountain        |       0.40 |        0.60 | N/A        | technological |
| arrows_impossibility_theorem   | mountain        |       0.60 |        0.40 | N/A        | political  |
| availability_heuristic         | noose           |       0.40 |        0.70 | N/A        | social     |
| blackstone_carried_interest_taxation | rope            |       0.30 |        0.70 | N/A        | economic   |
| burden_of_proof_scientific_empirical | noose           |       0.30 |        0.60 | N/A        | technological |
| busy_beaver_noncomputability   | mountain        |       0.40 |        0.30 | N/A        | technological |
| cap_theorem                    | mountain        |       0.40 |        0.70 | N/A        | technological |
| carbon_credit_markets_2026     | rope            |       0.55 |        0.60 | N/A        | economic   |
| carrying_capacity              | noose           |       0.70 |        0.60 | N/A        | economic   |
| cobra_effect                   | noose           |       0.60 |        0.40 | N/A        | economic   |
| college_admissions_market      | rope            |       0.70 |        0.50 | N/A        | social     |
| colorado_sbe_decentralization_friction | mountain        |       0.70 |        0.60 | N/A        | political  |
| compounding_logic              | rope            |       0.50 |        0.40 | N/A        | economic   |
| conways_game_of_life_dynamics  | mountain        |       0.30 |        0.40 | N/A        | technological |
| copyright_protection           | rope            |       0.40 |        0.50 | N/A        | social     |
| cow_field_poop                 | mountain        |       0.40 |        0.30 | N/A        | social     |

## Recommendations

### 1. Found 23 structural signatures shared by 5+ constraints

**Priority:** HIGH

**Recommended Action:** Investigate if these represent distinct categories beyond current framework

**Example signatures:**
- (0.8, 0.7, False, True)
- (None, None, None, None)
- (0.8, 0.8, False, True)

### 2. Found 168 hybrid constraints (high extraction + high suppression)

**Priority:** HIGH

**Recommended Action:** Strong evidence for 'Tangled Rope' category

**Details:** 168 constraints don't fit cleanly into mountain/rope/noose

### 3. Found 99 constraints matching 'tangled_rope' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'tangled_rope' as new category

**Details:** Type distribution: {'rope': 11, 'noose': 47, 'mountain': 13}

### 4. Found 18 constraints matching 'zombie' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'zombie' as new category

**Details:** Type distribution: {'mountain': 18}

### 5. Found 93 constraints matching 'scaffold' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'scaffold' as new category

**Details:** Type distribution: {'rope': 20, 'mountain': 39, 'noose': 22}

### 6. Found 63 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'rope': 13, 'mountain': 34, 'noose': 11}

### 7. Found 143 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

