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
   - Details: Type distribution: {'rope': 2, 'tangled_rope': 287, 'snare': 45, 'piton': 46}

4. **[MEDIUM]** Found 137 constraints matching 'scaffold' pattern
   - Action: Consider formalizing 'scaffold' as new category
   - Details: Type distribution: {'tangled_rope': 117, 'rope': 9, 'scaffold': 3, 'snare': 3, 'piton': 5}

5. **[MEDIUM]** Found 108 constraints matching 'wings' pattern
   - Action: Consider formalizing 'wings' as new category
   - Details: Type distribution: {'mountain': 102, 'rope': 6}

## Structural Twins

Constraints with identical structural signatures but different claimed types.

**Total twin groups found:** 55

| Signature | Count | Types Present | Domains | Examples |
|-----------|-------|---------------|---------|----------|
| (0.6, 0.7, False, True)        |    81 | snare, piton, tangled_rope | political, geopolitical, technological | shadow_fleet_sanctions_evasion, israeli_settlement_policy_authority_restriction |
| (0.8, 0.8, False, True)        |    69 | snare, piton, tangled_rope, rope | corporate_governance, Political, medical | 26usc469_real_estate_exemption, emergency_powers_ratchet |
| (0.8, 0.7, False, True)        |    68 | snare, piton, tangled_rope | psychological, infrastructure, institutional | ergot_grain_poisoning, abstraction_boundary_overrun |
| (0.6, 0.8, False, True)        |    51 | piton, tangled_rope  | institutional, political, geopolitical | openai_prism_development, eu_irgc_terrorist_designation |
| (0.9, 0.8, False, True)        |    50 | snare, piton, tangled_rope | logistical, institutional, informational | consensus_without_truth, lehman_repo_105 |
| (0.7, 0.8, False, True)        |    44 | snare, piton, tangled_rope | philosophical, political, geopolitical | CG_IsraelGaza_20231012, coe_ukraine_reparations_register |
| (0.5, 0.7, False, True)        |    43 | snare, piton, tangled_rope | health, political, geopolitical | dutch_minority_govt_2026, hershey_salt_strategy |
| (0.5, 0.8, False, True)        |    38 | piton, scaffold, tangled_rope | philosophical, political, geopolitical | scientific_paradigm_lifecycle, ai_adoption_stigma |
| (0.8, 0.9, False, True)        |    35 | snare, piton, tangled_rope | political, geopolitical, technological | dwp_carers_allowance_cliff, china_taiwan_reunification_mandate |
| (0.1, 0.1, True, False)        |    29 | mountain, rope       | sociological, logical, epistemological | local_vs_global_optima, endowment_effect |
| (0.5, 0.6, False, True)        |    24 | snare, piton, tangled_rope | psychological, mathematical, technological | cognac_geopolitical_risk, armra_colostrum_regulation |
| (0.8, 0.6, False, True)        |    24 | piton, tangled_rope, snare, rope | psychological, political, environmental | asshole_filter_2015, academic_peer_review_gatekeeping |
| (0.4, 0.5, False, True)        |    19 | scaffold, tangled_rope, rope | psychological, political, mathematical | stable_marriage_coordination, yoneda_lemma |
| (0.1, 0.1, False, True)        |    18 | mountain, tangled_rope, rope | philosophical, mathematical, physical | biological_curiosity, hiv_prep_prevention_2026 |
| (0.7, 0.7, False, True)        |    16 | piton, tangled_rope  | psychological, political, technological | cholesterol_pill_cost, awareness_without_leverage |
| (0.7, 0.6, False, True)        |    16 | snare, piton, tangled_rope | psychological, medical, political | sludge_bureaucratic_friction, utopia_apocalypse_fragility |
| (0.3, 0.5, False, True)        |    15 | scaffold, tangled_rope | investigation, political, mathematical | keltner_relationship_evaluation, publishing_embargo |
| (0.9, 0.7, False, True)        |    15 | piton, tangled_rope  | informational, political, technological | tail_risk_compression, capital_misallocation_spiral |
| (0.2, 0.1, True, False)        |    14 | mountain, rope       | analytical, mathematical, technological | nonstandard_arithmetic_models, information_foraging_theory |
| (0.6, 0.6, False, True)        |    13 | piton, tangled_rope  | political, technological, organizational | hu_2026_electoral_parity, portugal_polarization_threshold_2026 |

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
| emergency_powers_ratchet       | tangled_rope    |           0.84 |        0.78 | political  |
| ergot_grain_poisoning          | tangled_rope    |           0.80 |        0.70 | social     |
| cz_plea_agreement_2026         | tangled_rope    |           0.85 |        0.78 | economic   |
| consensus_without_truth        | tangled_rope    |           0.88 |        0.82 | social     |
| gale_shapley                   | tangled_rope    |           0.80 |        0.80 | economic   |
| abstraction_boundary_overrun   | tangled_rope    |           0.81 |        0.69 | technological |
| eu_irgc_terrorist_designation  | tangled_rope    |           0.60 |        0.85 | geopolitical |
| ai_evaluators_matching         | tangled_rope    |           0.75 |        0.80 | unknown    |

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
- scaffold: 3
- snare: 3

**Examples:**

| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |
|---------------|--------------|----------------|-------------|--------|
| noethers_isomorphism_theorems  | tangled_rope    |           0.48 |        0.42 | technological |
| cognac_geopolitical_risk       | tangled_rope    |           0.50 |        0.60 | economic   |
| social_narrative_casting       | tangled_rope    |           0.30 |        0.60 | social     |
| cmr_001                        | tangled_rope    |           0.55 |        0.40 | economic   |
| keltner_relationship_evaluation | tangled_rope    |           0.35 |        0.45 | social     |
| publishing_embargo             | tangled_rope    |           0.35 |        0.50 | social     |
| stable_marriage_coordination   | tangled_rope    |           0.40 |        0.45 | economic   |
| armra_colostrum_regulation     | tangled_rope    |           0.48 |        0.55 | economic   |
| yoneda_lemma                   | tangled_rope    |           0.40 |        0.45 | technological |
| kjv_great_awakening            | tangled_rope    |           0.50 |        0.60 | religious  |

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
| local_vs_global_optima         | mountain        |           0.05 |        0.05 | mathematical |
| nonstandard_arithmetic_models  | mountain        |           0.25 |        0.05 | mathematical |
| endowment_effect               | mountain        |           0.15 |        0.05 | economic   |
| english_chinese_tense_structure | mountain        |           0.05 |        0.02 | linguistic |
| information_foraging_theory    | rope            |           0.20 |        0.05 | technological |
| goldbach_conjecture            | mountain        |           0.08 |        0.05 | mathematical |
| four_color_theorem_topological_bound | mountain        |           0.02 |        0.05 | mathematical |
| kolmogorov_complexity          | mountain        |           0.15 |        0.05 | technological |
| c_physical_blue_wavelength     | mountain        |           0.02 |        0.01 | scientific |
| lowenheim_skolem_theorem       | mountain        |           0.05 |        0.00 | technological |

## Hybrid Patterns

Constraints with both high extraction and high suppression.

**Total hybrids found:** 592

| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |
|---------------|--------------|------------|-------------|-------|--------|
| trojan_war_spoils              | snare           |       1.00 |        1.00 |  2.00 | military   |
| north_korea_songun_mandatrophy | snare           |       0.95 |        0.95 |  1.90 | political  |
| gilgamesh_mortality_limit      | tangled_rope    |       1.00 |        0.90 |  1.90 | philosophical |
| khantivadin_radical_patience   | tangled_rope    |       1.00 |        0.90 |  1.90 | religious  |
| taliban_slavery_law_2024       | snare           |       0.90 |        0.95 |  1.85 | political  |
| integrated_digital_governance_stack | tangled_rope    |       0.90 |        0.95 |  1.85 | technological |
| fiat_currency_lifecycle        | tangled_rope    |       0.95 |        0.90 |  1.85 | economic   |
| technological_point_of_no_return | tangled_rope    |       0.86 |        0.98 |  1.84 | technological |
| epstein_kgb_honeytrap          | piton           |       0.92 |        0.88 |  1.80 | political  |
| iran_nin_repression            | snare           |       0.85 |        0.95 |  1.80 | technological |
| the_bacchae_madness_protocol   | tangled_rope    |       0.95 |        0.85 |  1.80 | religious  |
| horizon_liability_contract     | tangled_rope    |       0.85 |        0.95 |  1.80 | economic   |
| cuba_mandatrophic_collapse     | snare           |       0.95 |        0.85 |  1.80 | political  |
| blackstone_tax_receiveable_agreement | tangled_rope    |       0.85 |        0.95 |  1.80 | economic   |
| nsl_hk                         | snare           |       0.85 |        0.95 |  1.80 | political  |
| us_venezuela_blockade          | snare           |       0.85 |        0.95 |  1.80 | geopolitical |
| ad_synaptic_deficit            | tangled_rope    |       0.85 |        0.95 |  1.80 | biological |
| interpretive_frame_fragmentation | tangled_rope    |       0.93 |        0.86 |  1.79 | social     |
| power_without_responsibility   | tangled_rope    |       0.92 |        0.85 |  1.77 | political  |
| intertemporal_responsibility_gap | tangled_rope    |       0.92 |        0.85 |  1.77 | environmental |

**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.

## Transition Markers

Constraints with mid-range metrics suggesting transitional states.

**Total transition markers found:** 316

| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |
|---------------|--------------|------------|-------------|------------|--------|
| 8k_tv_limit_2026               | rope            |       0.70 |        0.50 | N/A        | technological |
| noethers_isomorphism_theorems  | tangled_rope    |       0.48 |        0.42 | N/A        | technological |
| cognac_geopolitical_risk       | tangled_rope    |       0.50 |        0.60 | N/A        | economic   |
| dutch_minority_govt_2026       | tangled_rope    |       0.48 |        0.65 | N/A        | political  |
| shadow_fleet_sanctions_evasion | tangled_rope    |       0.55 |        0.70 | N/A        | geopolitical |
| shobies_existential_commitment | tangled_rope    |       0.65 |        0.50 | N/A        | social     |
| social_narrative_casting       | tangled_rope    |       0.30 |        0.60 | N/A        | social     |
| cmr_001                        | tangled_rope    |       0.55 |        0.40 | N/A        | economic   |
| keltner_relationship_evaluation | tangled_rope    |       0.35 |        0.45 | N/A        | social     |
| publishing_embargo             | tangled_rope    |       0.35 |        0.50 | N/A        | social     |
| hershey_salt_strategy          | tangled_rope    |       0.48 |        0.65 | N/A        | economic   |
| stable_marriage_coordination   | tangled_rope    |       0.40 |        0.45 | N/A        | economic   |
| israeli_settlement_policy_authority_restriction | tangled_rope    |       0.55 |        0.70 | N/A        | political  |
| debt_trap_microfinance         | snare           |       0.55 |        0.70 | N/A        | economic   |
| coalition_disinfo_framework_2026 | tangled_rope    |       0.48 |        0.65 | N/A        | technological |
| visibility_bias_governance     | tangled_rope    |       0.54 |        0.68 | N/A        | political  |
| cholesterol_pill_cost          | tangled_rope    |       0.70 |        0.70 | N/A        | economic   |
| med_diet_consensus_2026        | tangled_rope    |       0.48 |        0.65 | N/A        | health     |
| armra_colostrum_regulation     | tangled_rope    |       0.48 |        0.55 | N/A        | economic   |
| lung_transplant_protocol       | tangled_rope    |       0.55 |        0.70 | N/A        | social     |

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

**Details:** Type distribution: {'rope': 2, 'tangled_rope': 287, 'snare': 45, 'piton': 46}

### 4. Found 137 constraints matching 'scaffold' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'scaffold' as new category

**Details:** Type distribution: {'tangled_rope': 117, 'rope': 9, 'scaffold': 3, 'snare': 3, 'piton': 5}

### 5. Found 108 constraints matching 'wings' pattern

**Priority:** MEDIUM

**Recommended Action:** Consider formalizing 'wings' as new category

**Details:** Type distribution: {'mountain': 102, 'rope': 6}

### 6. Found 316 constraints with mid-range metrics

**Priority:** MEDIUM

**Recommended Action:** Consider 'Scaffold' category for temporary/transitional constraints

**Details:** These constraints show characteristics of multiple types

