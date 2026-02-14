# Structural Pattern Mining

## Executive Summary

### Key Findings

1. **[HIGH]** Found 31 structural signatures shared by 5+ constraints
   - Action: Investigate if these represent distinct categories beyond current framework

2. **[HIGH]** Found 430 hybrid constraints (high extraction + high suppression)
   - Action: Strong evidence for 'Tangled Rope' category
   - Details: 430 constraints don't fit cleanly into mountain/rope/snare

3. **[MEDIUM]** Found 314 constraints matching 'tangled_rope' pattern
   - Action: Consider formalizing 'tangled_rope' as new category
   - Details: Type distribution: {'tangled_rope': 238, 'piton': 46, 'snare': 29, 'rope': 1}

4. **[MEDIUM]** Found 96 constraints matching 'scaffold' pattern
   - Action: Consider formalizing 'scaffold' as new category
   - Details: Type distribution: {'tangled_rope': 73, 'rope': 11, 'piton': 4, 'snare': 5, 'scaffold': 3}

5. **[MEDIUM]** Found 65 constraints matching 'wings' pattern
   - Action: Consider formalizing 'wings' as new category
   - Details: Type distribution: {'rope': 15, 'mountain': 40, 'snare': 1, 'scaffold': 9}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 60

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.8, 0.7, False, True)        |    68 | piton, tangled_rope, snare | organizational, cognitive, technological | abstraction_boundary_overrun, adaptive_lag_trap |
| (0.8, 0.8, False, True)        |    50 | piton, tangled_rope, snare | organizational, technological, legal | 26usc469_real_estate_exemption, ad_fus_coordination |
| (0.9, 0.8, False, True)        |    50 | piton, tangled_rope, snare | organizational, cognitive, technological | adversarial_truth_decay, agency_atrophy |
| (0.6, 0.7, False, True)        |    39 | piton, tangled_rope, snare | technological, biological, political | MOLTBOT_RELIGION, ai_religion_regulation |
| (0.7, 0.8, False, True)        |    24 | piton, tangled_rope, snare | technological, philosophical, political | CG_IsraelGaza_20231012, academic_fashion_modernism_2026 |
| (0.8, 0.6, False, True)        |    24 | piton, tangled_rope, rope, snare | technological, environmental, scientific | academic_peer_review_gatekeeping, academic_tenure_system |
| (0.8, 0.9, False, True)        |    21 | piton, tangled_rope, snare | technological, legal, biological | ad_synaptic_deficit, apartheid_nuclear_program |
| (0.7, 0.6, False, True)        |    16 | piton, tangled_rope, snare | technological, psychological, political | abstraction_leakage, carrying_capacity |
| (0.5, 0.7, False, True)        |    16 | piton, tangled_rope, snare | technological, biological, political | ai_auditability_gap, brain_network_paradigm_2026 |
| (0.5, 0.8, False, True)        |    15 | piton, tangled_rope, scaffold | political, philosophical, technological | artificial_scarcity_scaffold, atrophied_optimization_piton |
| (0.9, 0.7, False, True)        |    15 | piton, tangled_rope  | organizational, cognitive, technological | bureaucratic_legibility_collapse, capital_misallocation_spiral |
| (0.7, 0.7, False, True)        |    14 | piton, tangled_rope  | technological, psychological, political | awareness_without_leverage, cb_far_beyond_human |
| (0.2, 0.3, False, True)        |    13 | tangled_rope, scaffold, rope | mathematical, technological, legal | alternative_sovereignty_scaffold, burali_forte_paradox |
| (0.1, 0.1, True, False)        |    13 | mountain, scaffold, rope | mathematical, technological, economic | automatic_enrollment_defaults, busy_beaver_noncomputability |
| (0.6, 0.8, False, True)        |    13 | piton, tangled_rope  | technological, political, economic | bangladesh_july_national_charter, discover_core_2026 |
| (0.4, 0.5, False, True)        |    13 | tangled_rope, scaffold, rope | mathematical, technological, scientific | bgs_eigenvector_thermalization, copyright_protection |
| (0.7, 0.5, False, True)        |    12 | piton, tangled_rope, snare | political, technological, economic | adverse_possession, ai_cognitive_diversity_arbitrage |
| (0.6, 0.6, False, True)        |    12 | piton, tangled_rope  | organizational, technological, political | boiled_pineapple_trend_2026, carbon_credit_markets_2026 |
| (0.2, 0.1, False, True)        |    11 | tangled_rope, rope   | mathematics, mathematical, technological | basel_problem_convergence, brouwer_fixed_point |
| (0.1, 0.2, False, True)        |    11 | tangled_rope, rope   | mathematical, technological, philosophical | berkshire_compounding_culture, burden_of_proof_legal_criminal |

## Candidate Category Analysis

### Tangled Rope

**Pattern:** High extraction + High suppression + Requires enforcement
**Interpretation:** Mix of snare and rope characteristics

**Constraints matching pattern:** 314

**Current type distribution:**
- tangled_rope: 238
- piton: 46
- snare: 29
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

**Constraints matching pattern:** 96

**Current type distribution:**
- tangled_rope: 73
- rope: 11
- snare: 5
- piton: 4
- scaffold: 3

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| aging_well_assessment          | tangled_rope    |           0.52 |        0.45 | health     |
| ai_edu_decentralization        | rope            |           0.50 |        0.40 | technological |
| ai_task_horizon_reliability    | tangled_rope    |           0.48 |        0.60 | technological |
| arrows_impossibility_theorem   | tangled_rope    |           0.60 |        0.40 | political  |
| artificial_snow_2026           | piton           |           0.48 |        0.30 | environmental |
| axiom_reasoner_2026            | tangled_rope    |           0.48 |        0.35 | technological |
| bgs_eigenvector_thermalization | tangled_rope    |           0.42 |        0.45 | scientific |
| boiled_pineapple_trend_2026    | piton           |           0.55 |        0.60 | social     |
| brazil_2026_general_elections  | tangled_rope    |           0.52 |        0.42 | political  |
| carbon_credit_markets_2026     | tangled_rope    |           0.55 |        0.60 | economic   |

### Wings

**Pattern:** Low extraction + Low suppression + Emerges naturally
**Interpretation:** Enabling constraints, opposite of snare

**Constraints matching pattern:** 65

**Current type distribution:**
- mountain: 40
- rope: 15
- scaffold: 9
- snare: 1

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| automatic_enrollment_defaults  | rope            |           0.05 |        0.10 | economic   |
| axiom_of_choice_determinacy    | mountain        |           0.25 |        0.05 | mathematical |
| banach_fixed_point             | mountain        |           0.01 |        0.01 | mathematical |
| bgs_spectral_universality      | mountain        |           0.08 |        0.03 | scientific |
| biological_curiosity           | snare           |           0.15 |        0.20 | biological |
| birthday_paradox_collison      | mountain        |           0.05 |        0.00 | mathematical |
| busy_beaver_noncomputability   | mountain        |           0.05 |        0.05 | technological |
| c_physical_blue_wavelength     | mountain        |           0.02 |        0.01 | scientific |
| cantor_set_topology            | mountain        |           0.05 |        0.01 | mathematical |
| cap_theorem                    | mountain        |           0.05 |        0.01 | technological |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 430

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| trojan_war_spoils              | snare           |       1.00 |        1.00 |  2.00 | military   |
| gilgamesh_mortality_limit      | tangled_rope    |       1.00 |        0.90 |  1.90 | philosophical |
| khantivadin_radical_patience   | tangled_rope    |       1.00 |        0.90 |  1.90 | religious  |
| north_korea_songun_mandatrophy | snare           |       0.95 |        0.95 |  1.90 | political  |
| integrated_digital_governance_stack | tangled_rope    |       0.90 |        0.95 |  1.85 | technological |
| technological_point_of_no_return | tangled_rope    |       0.86 |        0.98 |  1.84 | technological |
| epstein_kgb_honeytrap          | piton           |       0.92 |        0.88 |  1.80 | political  |
| ad_synaptic_deficit            | tangled_rope    |       0.85 |        0.95 |  1.80 | biological |
| blackstone_tax_receiveable_agreement | tangled_rope    |       0.85 |        0.95 |  1.80 | economic   |
| cuba_mandatrophic_collapse     | snare           |       0.95 |        0.85 |  1.80 | political  |
| the_bacchae_madness_protocol   | tangled_rope    |       0.95 |        0.85 |  1.80 | religious  |
| interpretive_frame_fragmentation | tangled_rope    |       0.93 |        0.86 |  1.79 | social     |
| delta_force_selection_2026     | tangled_rope    |       0.92 |        0.85 |  1.77 | military   |
| intertemporal_responsibility_gap | tangled_rope    |       0.92 |        0.85 |  1.77 | environmental |
| power_without_responsibility   | tangled_rope    |       0.92 |        0.85 |  1.77 | political  |
| emergency_mode_lock_in         | tangled_rope    |       0.91 |        0.85 |  1.76 | political  |
| apartheid_nuclear_program      | tangled_rope    |       0.85 |        0.90 |  1.75 | political  |
| attention_market_cannibalization | tangled_rope    |       0.91 |        0.84 |  1.75 | economic   |
| dark_patterns_manipulation     | tangled_rope    |       0.85 |        0.90 |  1.75 | technological |
| frankenstein_creation_hubris   | tangled_rope    |       0.90 |        0.85 |  1.75 | technological |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 199

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
| arrows_impossibility_theorem   | tangled_rope    |       0.60 |        0.40 | N/A        | political  |
| artificial_snow_2026           | piton           |       0.48 |        0.30 | N/A        | environmental |
| asce_7_22_seismic_design       | tangled_rope    |       0.35 |        0.70 | N/A        | technological |
| availability_heuristic         | tangled_rope    |       0.40 |        0.70 | N/A        | social     |
| axiom_reasoner_2026            | tangled_rope    |       0.48 |        0.35 | N/A        | technological |
| bgs_eigenvector_thermalization | tangled_rope    |       0.42 |        0.45 | N/A        | scientific |
| blackstone_carried_interest_taxation | rope            |       0.30 |        0.70 | N/A        | economic   |
| boiled_pineapple_trend_2026    | piton           |       0.55 |        0.60 | N/A        | social     |

## Recommendations

### 1. Found 31 structural signatures shared by 5+ constraints

**Priority:** HIGH

**Recommended Action:** Investigate if these represent distinct categories beyond current framework

**Example signatures:**
- (0.8, 0.7, False, True)
- (0.8, 0.8, False, True)
- (0.9, 0.8, False, True)

### 2. Found 430 hybrid constraints (high extraction + high suppression)

**Priority:** HIGH

**Recommended Action:** Strong evidence for 'Tangled Rope' category

**Details:** 430 constraints don't fit cleanly into mountain/rope/snare

### 3. Found 314 constraints matching 'tangled_rope' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'tangled_rope' as new category

**Details:** Type distribution: {'tangled_rope': 238, 'piton': 46, 'snare': 29, 'rope': 1}

### 4. Found 96 constraints matching 'scaffold' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'scaffold' as new category

**Details:** Type distribution: {'tangled_rope': 73, 'rope': 11, 'piton': 4, 'snare': 5, 'scaffold': 3}

### 5. Found 65 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'rope': 15, 'mountain': 40, 'snare': 1, 'scaffold': 9}

### 6. Found 199 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

