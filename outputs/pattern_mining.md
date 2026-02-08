# Structural Pattern Mining

## Executive Summary

### Key Findings

1. **[HIGH]** Found 38 structural signatures shared by 5+ constraints
   - Action: Investigate if these represent distinct categories beyond current framework

2. **[HIGH]** Found 411 hybrid constraints (high extraction + high suppression)
   - Action: Strong evidence for 'Tangled Rope' category
   - Details: 411 constraints don't fit cleanly into mountain/rope/snare

3. **[MEDIUM]** Found 301 constraints matching 'tangled_rope' pattern
   - Action: Consider formalizing 'tangled_rope' as new category
   - Details: Type distribution: {'tangled_rope': 204, 'piton': 46, 'mountain': 23, 'snare': 27, 'rope': 1}

4. **[MEDIUM]** Found 26 constraints matching 'piton' pattern
   - Action: Consider formalizing 'piton' as new category
   - Details: Type distribution: {'mountain': 26}

5. **[MEDIUM]** Found 97 constraints matching 'scaffold' pattern
   - Action: Consider formalizing 'scaffold' as new category
   - Details: Type distribution: {'mountain': 30, 'tangled_rope': 45, 'snare': 4, 'rope': 11, 'scaffold': 3, 'piton': 4}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 70

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.8, 0.7, False, True)        |    64 | tangled_rope, snare, piton, mountain | political, economic, social | abstraction_boundary_overrun, ergot_grain_poisoning |
| (0.8, 0.8, False, True)        |    47 | tangled_rope, snare, piton, mountain | political, economic, social | 26usc469_real_estate_exemption, ad_fus_coordination |
| (0.9, 0.8, False, True)        |    47 | tangled_rope, snare, piton, mountain | religious, political, economic | consensus_without_truth, lehman_repo_105 |
| (0.6, 0.7, False, True)        |    34 | tangled_rope, piton  | political, economic, social | MOLTBOT_RELIGION, lung_transplant_protocol |
| (0.7, 0.8, False, True)        |    25 | tangled_rope, snare, piton, mountain | political, economic, social | CG_IsraelGaza_20231012, academic_fashion_modernism_2026 |
| (0.8, 0.6, False, True)        |    24 | rope, snare, tangled_rope, piton, mountain | scientific, political, economic | academic_peer_review_gatekeeping, academic_tenure_system |
| (0.8, 0.9, False, True)        |    21 | tangled_rope, snare, piton, mountain | religious, political, economic | ad_synaptic_deficit, china_taiwan_reunification_mandate |
| (0.1, 0.2, True, False)        |    18 | snare, rope, mountain | analytical, biological, philosophical | biological_curiosity, liar_paradox |
| (0.7, 0.6, False, True)        |    16 | tangled_rope, snare, piton, mountain | political, economic, social | abstraction_leakage, sludge_bureaucratic_friction |
| (0.9, 0.7, False, True)        |    15 | tangled_rope, piton  | political, economic, social | tail_risk_compression, capital_misallocation_spiral |
| (0.7, 0.7, False, True)        |    14 | tangled_rope, piton  | political, economic, social | cholesterol_pill_cost, awareness_without_leverage |
| (0.5, 0.7, False, True)        |    13 | tangled_rope, piton, mountain | political, economic, social | visibility_bias_governance, ai_auditability_gap |
| (0.5, 0.8, False, True)        |    13 | tangled_rope, scaffold, piton | social, philosophical, political | ulysses_chp10, germline_regulation_threshold_2026 |
| (0.6, 0.8, False, True)        |    13 | tangled_rope, piton  | political, economic, social | meta_nda, technocratic_overreach |
| (0.2, 0.3, True, False)        |    12 | rope, mountain       | psychological, technological, scientific | nonstandard_arithmetic_models, tarski_undefinability |
| (0.7, 0.5, False, True)        |    11 | tangled_rope, snare, piton | social, economic, political | shobies_existential_commitment, ai_cognitive_diversity_arbitrage |
| (0.6, 0.6, False, True)        |    11 | tangled_rope, piton  | social, organizational, political | hu_2026_electoral_parity, portugal_polarization_threshold_2026 |
| (0.2, 0.1, True, False)        |    10 | rope, mountain       | physics, mathematical, mathematics | quine_self_replication, information_foraging_theory |
| (0.1, 0.1, True, False)        |    10 | rope, mountain       | economic, technological, mathematical | goldbach_conjecture, noethers_theorem_symmetry |
| (0.6, 0.9, False, True)        |    10 | tangled_rope, piton, mountain | political, economic, biological | semantic_overload_friction, ulysses_chp14 |

## Candidate Category Analysis

### Tangled Rope

**Pattern:** High extraction + High suppression + Requires enforcement
**Interpretation:** Mix of snare and rope characteristics

**Constraints matching pattern:** 301

**Current type distribution:**
- tangled_rope: 204
- piton: 46
- snare: 27
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
| emergency_powers_ratchet       | tangled_rope    |           0.84 |        0.78 | political  |
| ergot_grain_poisoning          | tangled_rope    |           0.80 |        0.70 | social     |

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
| ancestral_pueblo_hydrology     | mountain        |           0.82 |        0.75 | environmental |
| bushman_money_magic            | mountain        |           0.90 |        0.80 | economic   |
| asce_7_22_seismic_design       | mountain        |           0.20 |        0.70 | technological |
| wikipedia_notability_requirement_2026 | mountain        |           0.40 |        0.70 | social     |
| iran_mandatrophic_collapse     | mountain        |           0.90 |        0.85 | political  |
| dark_patterns_manipulation     | mountain        |           0.85 |        0.90 | technological |
| family_succession_and_decadence | mountain        |           0.80 |        0.90 | social     |
| viral_transmission_rates       | mountain        |           0.70 |        0.80 | technological |
| xi_mao_ideological_centralization | mountain        |           0.75 |        0.85 | political  |

### Scaffold

**Pattern:** Medium extractiveness + Medium suppression
**Interpretation:** Temporary transition mechanisms

**Constraints matching pattern:** 97

**Current type distribution:**
- tangled_rope: 45
- mountain: 30
- rope: 11
- snare: 4
- piton: 4
- scaffold: 3

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| local_vs_global_optima         | mountain        |           0.30 |        0.50 | mathematical |
| social_narrative_casting       | mountain        |           0.30 |        0.60 | social     |
| cmr_001                        | tangled_rope    |           0.55 |        0.40 | economic   |
| keltner_relationship_evaluation | mountain        |           0.35 |        0.45 | social     |
| stable_marriage_coordination   | mountain        |           0.40 |        0.30 | economic   |
| endowment_effect               | snare           |           0.30 |        0.40 | economic   |
| empty_tomb_transformation      | mountain        |           0.40 |        0.40 | religious  |
| sat_csp_complexity             | mountain        |           0.40 |        0.50 | technological |
| jevons_paradox                 | tangled_rope    |           0.60 |        0.50 | economic   |
| emotional_cycles_of_change     | tangled_rope    |           0.55 |        0.45 | psychology |

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
| nonstandard_arithmetic_models  | mountain        |           0.25 |        0.30 | mathematical |
| quine_self_replication         | mountain        |           0.20 |        0.10 | technological |
| information_foraging_theory    | rope            |           0.20 |        0.10 | technological |
| biological_curiosity           | snare           |           0.15 |        0.20 | biological |
| goldbach_conjecture            | mountain        |           0.15 |        0.10 | mathematical |
| four_color_theorem_topological_bound | mountain        |           0.25 |        0.20 | mathematical |
| tarski_undefinability          | mountain        |           0.20 |        0.30 | technological |
| liar_paradox                   | mountain        |           0.10 |        0.20 | analytical |
| fundamental_theorem_of_algebra | mountain        |           0.10 |        0.20 | mathematical |
| chaitins_omega_undecidability  | mountain        |           0.20 |        0.30 | technological |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 411

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| trojan_war_spoils              | snare           |       1.00 |        1.00 |  2.00 | military   |
| north_korea_songun_mandatrophy | snare           |       0.95 |        0.95 |  1.90 | political  |
| gilgamesh_mortality_limit      | tangled_rope    |       1.00 |        0.90 |  1.90 | philosophical |
| khantivadin_radical_patience   | mountain        |       1.00 |        0.90 |  1.90 | religious  |
| integrated_digital_governance_stack | tangled_rope    |       0.90 |        0.95 |  1.85 | technological |
| technological_point_of_no_return | tangled_rope    |       0.86 |        0.98 |  1.84 | technological |
| epstein_kgb_honeytrap          | piton           |       0.92 |        0.88 |  1.80 | political  |
| ad_synaptic_deficit            | mountain        |       0.85 |        0.95 |  1.80 | biological |
| the_bacchae_madness_protocol   | mountain        |       0.95 |        0.85 |  1.80 | religious  |
| cuba_mandatrophic_collapse     | snare           |       0.95 |        0.85 |  1.80 | political  |
| blackstone_tax_receiveable_agreement | tangled_rope    |       0.85 |        0.95 |  1.80 | economic   |
| interpretive_frame_fragmentation | tangled_rope    |       0.93 |        0.86 |  1.79 | social     |
| power_without_responsibility   | tangled_rope    |       0.92 |        0.85 |  1.77 | political  |
| intertemporal_responsibility_gap | tangled_rope    |       0.92 |        0.85 |  1.77 | environmental |
| delta_force_selection_2026     | tangled_rope    |       0.92 |        0.85 |  1.77 | military   |
| emergency_mode_lock_in         | tangled_rope    |       0.91 |        0.85 |  1.76 | political  |
| lehman_repo_105                | tangled_rope    |       0.90 |        0.85 |  1.75 | economic   |
| scam_compound_grey_zone_2026   | snare           |       0.85 |        0.90 |  1.75 | social     |
| guthrie_kidnapping_2026        | snare           |       0.85 |        0.90 |  1.75 | social     |
| iran_mandatrophic_collapse     | mountain        |       0.90 |        0.85 |  1.75 | political  |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 193

| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |
|---------------|--------------|------------|-------------|------------|--------|
| 8k_tv_limit_2026               | rope            |       0.70 |        0.50 | N/A        | technological |
| MOLTBOT_RELIGION               | tangled_rope    |       0.60 |        0.70 | N/A        | technological |
| local_vs_global_optima         | mountain        |       0.30 |        0.50 | N/A        | mathematical |
| shobies_existential_commitment | tangled_rope    |       0.65 |        0.50 | N/A        | social     |
| social_narrative_casting       | mountain        |       0.30 |        0.60 | N/A        | social     |
| cmr_001                        | tangled_rope    |       0.55 |        0.40 | N/A        | economic   |
| keltner_relationship_evaluation | mountain        |       0.35 |        0.45 | N/A        | social     |
| stable_marriage_coordination   | mountain        |       0.40 |        0.30 | N/A        | economic   |
| endowment_effect               | snare           |       0.30 |        0.40 | N/A        | economic   |
| visibility_bias_governance     | tangled_rope    |       0.54 |        0.68 | N/A        | political  |
| cholesterol_pill_cost          | tangled_rope    |       0.70 |        0.70 | N/A        | economic   |
| lung_transplant_protocol       | tangled_rope    |       0.55 |        0.70 | N/A        | social     |
| tragedy_of_the_commons         | tangled_rope    |       0.70 |        0.40 | N/A        | economic   |
| empty_tomb_transformation      | mountain        |       0.40 |        0.40 | N/A        | religious  |
| new_start_expiration           | tangled_rope    |       0.60 |        0.70 | N/A        | political  |
| sat_csp_complexity             | mountain        |       0.40 |        0.50 | N/A        | technological |
| jevons_paradox                 | tangled_rope    |       0.60 |        0.50 | N/A        | economic   |
| emotional_cycles_of_change     | tangled_rope    |       0.55 |        0.45 | N/A        | psychology |
| hu_2026_electoral_parity       | tangled_rope    |       0.58 |        0.62 | N/A        | political  |
| global_stimulus_spree          | tangled_rope    |       0.62 |        0.45 | N/A        | economic   |

## Recommendations

### 1. Found 38 structural signatures shared by 5+ constraints

**Priority:** HIGH

**Recommended Action:** Investigate if these represent distinct categories beyond current framework

**Example signatures:**
- (0.8, 0.7, False, True)
- (0.8, 0.8, False, True)
- (0.9, 0.8, False, True)

### 2. Found 411 hybrid constraints (high extraction + high suppression)

**Priority:** HIGH

**Recommended Action:** Strong evidence for 'Tangled Rope' category

**Details:** 411 constraints don't fit cleanly into mountain/rope/snare

### 3. Found 301 constraints matching 'tangled_rope' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'tangled_rope' as new category

**Details:** Type distribution: {'tangled_rope': 204, 'piton': 46, 'mountain': 23, 'snare': 27, 'rope': 1}

### 4. Found 26 constraints matching 'piton' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'piton' as new category

**Details:** Type distribution: {'mountain': 26}

### 5. Found 97 constraints matching 'scaffold' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'scaffold' as new category

**Details:** Type distribution: {'mountain': 30, 'tangled_rope': 45, 'snare': 4, 'rope': 11, 'scaffold': 3, 'piton': 4}

### 6. Found 62 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'mountain': 46, 'rope': 15, 'snare': 1}

### 7. Found 193 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

