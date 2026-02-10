# Structural Pattern Mining

## Executive Summary

### Key Findings

1. **[HIGH]** Found 38 structural signatures shared by 5+ constraints
   - Action: Investigate if these represent distinct categories beyond current framework

2. **[HIGH]** Found 413 hybrid constraints (high extraction + high suppression)
   - Action: Strong evidence for 'Tangled Rope' category
   - Details: 413 constraints don't fit cleanly into mountain/rope/snare

3. **[MEDIUM]** Found 302 constraints matching 'tangled_rope' pattern
   - Action: Consider formalizing 'tangled_rope' as new category
   - Details: Type distribution: {'tangled_rope': 204, 'piton': 46, 'mountain': 23, 'snare': 28, 'rope': 1}

4. **[MEDIUM]** Found 26 constraints matching 'piton' pattern
   - Action: Consider formalizing 'piton' as new category
   - Details: Type distribution: {'mountain': 26}

5. **[MEDIUM]** Found 99 constraints matching 'scaffold' pattern
   - Action: Consider formalizing 'scaffold' as new category
   - Details: Type distribution: {'tangled_rope': 47, 'rope': 11, 'mountain': 30, 'piton': 4, 'snare': 4, 'scaffold': 3}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 70

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.8, 0.7, False, True)        |    64 | snare, piton, tangled_rope, mountain | social, psychological, systems_engineering | abstraction_boundary_overrun, adaptive_lag_trap |
| (0.8, 0.8, False, True)        |    47 | snare, piton, tangled_rope, mountain | social, political, organizational | 26usc469_real_estate_exemption, ad_fus_coordination |
| (0.9, 0.8, False, True)        |    47 | snare, piton, tangled_rope, mountain | social, logistical, political | adversarial_truth_decay, agency_atrophy |
| (0.6, 0.7, False, True)        |    36 | snare, piton, tangled_rope | social, bio_industrial, political | MOLTBOT_RELIGION, ai_religion_regulation |
| (0.7, 0.8, False, True)        |    25 | snare, piton, tangled_rope, mountain | social, political, geopolitical | CG_IsraelGaza_20231012, academic_fashion_modernism_2026 |
| (0.8, 0.6, False, True)        |    24 | snare, piton, tangled_rope, rope, mountain | social, psychological, political | academic_peer_review_gatekeeping, academic_tenure_system |
| (0.8, 0.9, False, True)        |    21 | snare, piton, tangled_rope, mountain | social, political, legal | ad_synaptic_deficit, apartheid_nuclear_program |
| (0.1, 0.2, True, False)        |    18 | snare, rope, mountain | analytical, mathematical, mathematics | biological_curiosity, church_turing_thesis |
| (0.7, 0.6, False, True)        |    16 | snare, piton, tangled_rope, mountain | social, psychological, political | abstraction_leakage, carrying_capacity |
| (0.9, 0.7, False, True)        |    15 | piton, tangled_rope  | social, political, organizational | bureaucratic_legibility_collapse, capital_misallocation_spiral |
| (0.7, 0.7, False, True)        |    14 | piton, tangled_rope  | social, psychological, political | awareness_without_leverage, cb_far_beyond_human |
| (0.5, 0.7, False, True)        |    13 | piton, tangled_rope, mountain | social, political, economic | ai_auditability_gap, eurozone_fragmentation_2026 |
| (0.5, 0.8, False, True)        |    13 | piton, tangled_rope, scaffold | social, philosophical, political | artificial_scarcity_scaffold, atrophied_optimization_piton |
| (0.6, 0.8, False, True)        |    13 | piton, tangled_rope  | social, political, institutional | bangladesh_july_national_charter, discover_core_2026 |
| (0.2, 0.3, True, False)        |    12 | rope, mountain       | scientific, mathematical, psychological | burali_forte_paradox, central_limit_theorem_convergence |
| (0.7, 0.5, False, True)        |    11 | snare, piton, tangled_rope | social, economic, political | adverse_possession, ai_cognitive_diversity_arbitrage |
| (0.6, 0.6, False, True)        |    11 | piton, tangled_rope  | social, economic, organizational | boiled_pineapple_trend_2026, carbon_credit_markets_2026 |
| (0.1, 0.1, True, False)        |    10 | rope, mountain       | mathematical, economic, technological | automatic_enrollment_defaults, ergo_lets_protocol |
| (0.2, 0.1, True, False)        |    10 | rope, mountain       | mathematical, mathematics, physics | basel_problem_convergence, brouwer_fixed_point |
| (0.6, 0.9, False, True)        |    10 | piton, tangled_rope, mountain | social, political, geopolitical | constitutional_consecration, digital_identity_tether |

## Candidate Category Analysis

### Tangled Rope

**Pattern:** High extraction + High suppression + Requires enforcement
**Interpretation:** Mix of snare and rope characteristics

**Constraints matching pattern:** 302

**Current type distribution:**
- tangled_rope: 204
- piton: 46
- snare: 28
- mountain: 23
- rope: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| 26usc469_real_estate_exemption | tangled_rope    |           0.75 |        0.80 | legal      |
| CG_IsraelGaza_20231012         | tangled_rope    |           0.65 |        0.80 | political  |
| MOLTBOT_RELIGION               | tangled_rope    |           0.60 |        0.70 | technological |
| abstraction_boundary_overrun   | tangled_rope    |           0.81 |        0.69 | technological |
| academic_fashion_modernism_2026 | piton           |           0.68 |        0.78 | social     |
| academic_tenure_system         | tangled_rope    |           0.75 |        0.60 | economic   |
| ad_fus_coordination            | tangled_rope    |           0.75 |        0.80 | medical    |
| ad_synaptic_deficit            | mountain        |           0.85 |        0.95 | biological |
| adaptive_lag_trap              | tangled_rope    |           0.83 |        0.71 | economic   |
| adversarial_truth_decay        | piton           |           0.89 |        0.78 | social     |

### Piton

**Pattern:** High suppression + Enforced + Claimed as mountain
**Interpretation:** False mountains that are obviously constructed

**Constraints matching pattern:** 26

**Current type distribution:**
- mountain: 26

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| ad_synaptic_deficit            | mountain        |           0.85 |        0.95 | biological |
| ai_professional_displacement   | mountain        |           0.71 |        0.80 | economic   |
| ancestral_pueblo_hydrology     | mountain        |           0.82 |        0.75 | environmental |
| asce_7_22_seismic_design       | mountain        |           0.20 |        0.70 | technological |
| bushman_money_magic            | mountain        |           0.90 |        0.80 | economic   |
| challenger_o_ring_integrity    | mountain        |           0.80 |        0.70 | technological |
| constitutional_supremacy       | mountain        |           0.30 |        0.95 | legal      |
| dark_patterns_manipulation     | mountain        |           0.85 |        0.90 | technological |
| family_succession_and_decadence | mountain        |           0.80 |        0.90 | social     |
| goodharts_law                  | mountain        |           0.50 |        0.70 | economic   |

### Scaffold

**Pattern:** Medium extractiveness + Medium suppression
**Interpretation:** Temporary transition mechanisms

**Constraints matching pattern:** 99

**Current type distribution:**
- tangled_rope: 47
- mountain: 30
- rope: 11
- piton: 4
- snare: 4
- scaffold: 3

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| aging_well_assessment          | tangled_rope    |           0.52 |        0.45 | health     |
| ai_edu_decentralization        | rope            |           0.50 |        0.40 | technological |
| ai_task_horizon_reliability    | tangled_rope    |           0.48 |        0.60 | technological |
| arrows_impossibility_theorem   | mountain        |           0.60 |        0.40 | political  |
| artificial_snow_2026           | piton           |           0.48 |        0.30 | environmental |
| axiom_reasoner_2026            | mountain        |           0.48 |        0.35 | technological |
| boiled_pineapple_trend_2026    | piton           |           0.55 |        0.60 | social     |
| brazil_2026_general_elections  | tangled_rope    |           0.52 |        0.42 | political  |
| burden_of_proof_scientific_empirical | mountain        |           0.30 |        0.60 | technological |
| busy_beaver_noncomputability   | mountain        |           0.40 |        0.30 | technological |

### Wings

**Pattern:** Low extraction + Low suppression + Emerges naturally
**Interpretation:** Enabling constraints, opposite of snare

**Constraints matching pattern:** 62

**Current type distribution:**
- mountain: 46
- rope: 15
- snare: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| automatic_enrollment_defaults  | rope            |           0.05 |        0.10 | economic   |
| basel_problem_convergence      | mountain        |           0.20 |        0.10 | mathematical |
| biological_curiosity           | snare           |           0.15 |        0.20 | biological |
| brouwer_fixed_point            | mountain        |           0.20 |        0.10 | mathematics |
| buffons_needle_pi_estimation   | mountain        |           0.20 |        0.10 | mathematical |
| burali_forte_paradox           | mountain        |           0.20 |        0.30 | technological |
| cantor_set_topology            | mountain        |           0.30 |        0.20 | mathematical |
| chaitins_omega_undecidability  | mountain        |           0.20 |        0.30 | technological |
| choice_architecture_design     | rope            |           0.20 |        0.30 | psychological |
| church_turing_thesis           | mountain        |           0.10 |        0.20 | technological |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 413

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| trojan_war_spoils              | snare           |       1.00 |        1.00 |  2.00 | military   |
| gilgamesh_mortality_limit      | tangled_rope    |       1.00 |        0.90 |  1.90 | philosophical |
| khantivadin_radical_patience   | mountain        |       1.00 |        0.90 |  1.90 | religious  |
| north_korea_songun_mandatrophy | snare           |       0.95 |        0.95 |  1.90 | political  |
| integrated_digital_governance_stack | tangled_rope    |       0.90 |        0.95 |  1.85 | technological |
| technological_point_of_no_return | tangled_rope    |       0.86 |        0.98 |  1.84 | technological |
| epstein_kgb_honeytrap          | piton           |       0.92 |        0.88 |  1.80 | political  |
| ad_synaptic_deficit            | mountain        |       0.85 |        0.95 |  1.80 | biological |
| blackstone_tax_receiveable_agreement | tangled_rope    |       0.85 |        0.95 |  1.80 | economic   |
| cuba_mandatrophic_collapse     | snare           |       0.95 |        0.85 |  1.80 | political  |
| the_bacchae_madness_protocol   | mountain        |       0.95 |        0.85 |  1.80 | religious  |
| interpretive_frame_fragmentation | tangled_rope    |       0.93 |        0.86 |  1.79 | social     |
| delta_force_selection_2026     | tangled_rope    |       0.92 |        0.85 |  1.77 | military   |
| intertemporal_responsibility_gap | tangled_rope    |       0.92 |        0.85 |  1.77 | environmental |
| power_without_responsibility   | tangled_rope    |       0.92 |        0.85 |  1.77 | political  |
| emergency_mode_lock_in         | tangled_rope    |       0.91 |        0.85 |  1.76 | political  |
| apartheid_nuclear_program      | tangled_rope    |       0.85 |        0.90 |  1.75 | political  |
| attention_market_cannibalization | tangled_rope    |       0.91 |        0.84 |  1.75 | economic   |
| dark_patterns_manipulation     | mountain        |       0.85 |        0.90 |  1.75 | technological |
| frankenstein_creation_hubris   | tangled_rope    |       0.90 |        0.85 |  1.75 | technological |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 197

| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |
|---------------|--------------|------------|-------------|------------|--------|
| 8k_tv_limit_2026               | rope            |       0.70 |        0.50 | N/A        | technological |
| MOLTBOT_RELIGION               | tangled_rope    |       0.60 |        0.70 | N/A        | technological |
| adverse_possession             | snare           |       0.65 |        0.45 | N/A        | economic   |
| agentive_optimism_2026         | piton           |       0.70 |        0.65 | N/A        | political  |
| aging_well_assessment          | tangled_rope    |       0.52 |        0.45 | N/A        | health     |
| ai_auditability_gap            | tangled_rope    |       0.52 |        0.65 | N/A        | technological |
| ai_edu_decentralization        | rope            |       0.50 |        0.40 | N/A        | technological |
| ai_religion_regulation         | tangled_rope    |       0.60 |        0.70 | N/A        | technological |
| ai_scholar_citation_trap       | tangled_rope    |       0.55 |        0.70 | N/A        | technological |
| ai_superpowers_race_2026       | scaffold        |       0.64 |        0.70 | N/A        | technological |
| ai_task_horizon_reliability    | tangled_rope    |       0.48 |        0.60 | N/A        | technological |
| altruistic_misery_paradox_2026 | piton           |       0.62 |        0.68 | N/A        | social     |
| arrows_impossibility_theorem   | mountain        |       0.60 |        0.40 | N/A        | political  |
| artificial_snow_2026           | piton           |       0.48 |        0.30 | N/A        | environmental |
| availability_heuristic         | mountain        |       0.40 |        0.70 | N/A        | social     |
| axiom_reasoner_2026            | mountain        |       0.48 |        0.35 | N/A        | technological |
| blackstone_carried_interest_taxation | rope            |       0.30 |        0.70 | N/A        | economic   |
| boiled_pineapple_trend_2026    | piton           |       0.55 |        0.60 | N/A        | social     |
| brazil_2026_general_elections  | tangled_rope    |       0.52 |        0.42 | N/A        | political  |
| burden_of_proof_scientific_empirical | mountain        |       0.30 |        0.60 | N/A        | technological |

## Recommendations

### 1. Found 38 structural signatures shared by 5+ constraints

**Priority:** HIGH

**Recommended Action:** Investigate if these represent distinct categories beyond current framework

**Example signatures:**
- (0.8, 0.7, False, True)
- (0.8, 0.8, False, True)
- (0.9, 0.8, False, True)

### 2. Found 413 hybrid constraints (high extraction + high suppression)

**Priority:** HIGH

**Recommended Action:** Strong evidence for 'Tangled Rope' category

**Details:** 413 constraints don't fit cleanly into mountain/rope/snare

### 3. Found 302 constraints matching 'tangled_rope' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'tangled_rope' as new category

**Details:** Type distribution: {'tangled_rope': 204, 'piton': 46, 'mountain': 23, 'snare': 28, 'rope': 1}

### 4. Found 26 constraints matching 'piton' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'piton' as new category

**Details:** Type distribution: {'mountain': 26}

### 5. Found 99 constraints matching 'scaffold' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'scaffold' as new category

**Details:** Type distribution: {'tangled_rope': 47, 'rope': 11, 'mountain': 30, 'piton': 4, 'snare': 4, 'scaffold': 3}

### 6. Found 62 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'rope': 15, 'mountain': 46, 'snare': 1}

### 7. Found 197 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

