:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/ability_ceiling_reading.pl', 'ability_ceiling_reading', 'ABILITY_CEILING_READING', 1).
test_case('testsets/access_barrier_reading.pl', 'access_barrier_reading', 'ACCESS_BARRIER_READING', 2).
test_case('testsets/actinide_replenishment_mechanism_contradictions.pl', 'unknown_interval', 'ACTINIDE_REPLENISHMENT_MECHANISM_CONTRADICTIONS', 3).
test_case('testsets/actinide_replenishment_mechanism_flat_control.pl', 'actinide_replenishment_mechanism_flat_control', 'ACTINIDE_REPLENISHMENT_MECHANISM_FLAT_CONTROL', 4).
test_case('testsets/adjacency_reading.pl', 'adjacency_reading', 'ADJACENCY_READING', 5).
test_case('testsets/adjunctification_of_university_teaching_c0.pl', 'adjunctification_of_university_teaching_c0', 'ADJUNCTIFICATION_OF_UNIVERSITY_TEACHING_C0', 6).
test_case('testsets/adverse_effect_guarantee_kernel_contradictions.pl', 'unknown_interval', 'ADVERSE_EFFECT_GUARANTEE_KERNEL_CONTRADICTIONS', 7).
test_case('testsets/adverse_effect_guarantee_kernel_flat_control.pl', 'adverse_effect_guarantee_kernel_flat_control', 'ADVERSE_EFFECT_GUARANTEE_KERNEL_FLAT_CONTROL', 8).
test_case('testsets/adverse_effect_measurability_contradictions.pl', 'unknown_interval', 'ADVERSE_EFFECT_MEASURABILITY_CONTRADICTIONS', 9).
test_case('testsets/adverse_effect_measurability_flat_control.pl', 'adverse_effect_measurability_flat_control', 'ADVERSE_EFFECT_MEASURABILITY_FLAT_CONTROL', 10).
test_case('testsets/ai_governance_legitimacy__democratic_pluralist_reading.pl', 'ai_governance_legitimacy__democratic_pluralist_reading', 'AI_GOVERNANCE_LEGITIMACY__DEMOCRATIC_PLURALIST_READING', 11).
test_case('testsets/algorithmic_attribution_contradictions.pl', 'unknown_interval', 'ALGORITHMIC_ATTRIBUTION_CONTRADICTIONS', 12).
test_case('testsets/algorithmic_attribution_flat_control.pl', 'algorithmic_attribution_flat_control', 'ALGORITHMIC_ATTRIBUTION_FLAT_CONTROL', 13).
test_case('testsets/alignment_constraint_narrowing.pl', 'alignment_constraint_narrowing', 'ALIGNMENT_CONSTRAINT_NARROWING', 14).
test_case('testsets/alpha_m_supercriticality_kernel_flat_control.pl', 'alpha_m_supercriticality_kernel_flat_control', 'ALPHA_M_SUPERCRITICALITY_KERNEL_FLAT_CONTROL', 15).
test_case('testsets/animal_status_kernel__property_reading.pl', 'animal_status_kernel__property_reading', 'ANIMAL_STATUS_KERNEL__PROPERTY_READING', 16).
test_case('testsets/apoe4_mitochondrial_vulnerability.pl', 'apoe4_mitochondrial_vulnerability', 'APOE4_MITOCHONDRIAL_VULNERABILITY', 17).
test_case('testsets/arbitrary_selection_under_competence_signaling.pl', 'arbitrary_selection_under_competence_signaling', 'ARBITRARY_SELECTION_UNDER_COMPETENCE_SIGNALING', 18).
test_case('testsets/architectural_pattern_validity.pl', 'architectural_pattern_validity', 'ARCHITECTURAL_PATTERN_VALIDITY', 19).
test_case('testsets/audit_reading.pl', 'audit_reading', 'AUDIT_READING', 20).
test_case('testsets/authentic_preference_boundary_contradictions.pl', 'unknown_interval', 'AUTHENTIC_PREFERENCE_BOUNDARY_CONTRADICTIONS', 21).
test_case('testsets/authorial_legitimacy_kernel_flat_control.pl', 'authorial_legitimacy_kernel_flat_control', 'AUTHORIAL_LEGITIMACY_KERNEL_FLAT_CONTROL', 22).
test_case('testsets/authorial_primacy_reading.pl', 'authorial_primacy_reading', 'AUTHORIAL_PRIMACY_READING', 23).
test_case('testsets/authoritative_specification_reading.pl', 'authoritative_specification_reading', 'AUTHORITATIVE_SPECIFICATION_READING', 24).
test_case('testsets/authority_vacuum_incommensurability.pl', 'authority_vacuum_incommensurability', 'AUTHORITY_VACUUM_INCOMMENSURABILITY', 25).
test_case('testsets/autonomy_reading.pl', 'autonomy_reading', 'AUTONOMY_READING', 26).
test_case('testsets/basic_law_interpretive_authority__parliamentary_sovereignty_reading.pl', 'basic_law_interpretive_authority__parliamentary_sovereignty_reading', 'BASIC_LAW_INTERPRETIVE_AUTHORITY__PARLIAMENTARY_SOVEREIGNTY_READING', 27).
test_case('testsets/basic_law_interpretive_boundary__parliamentary_sovereignty_reading.pl', 'basic_law_interpretive_boundary__parliamentary_sovereignty_reading', 'BASIC_LAW_INTERPRETIVE_BOUNDARY__PARLIAMENTARY_SOVEREIGNTY_READING', 28).
test_case('testsets/beatability_of_the_take_contradictions.pl', 'unknown_interval', 'BEATABILITY_OF_THE_TAKE_CONTRADICTIONS', 29).
test_case('testsets/beatability_of_the_take_flat_control.pl', 'beatability_of_the_take_flat_control', 'BEATABILITY_OF_THE_TAKE_FLAT_CONTROL', 30).
test_case('testsets/behavioral_adoption_friction.pl', 'behavioral_adoption_friction', 'BEHAVIORAL_ADOPTION_FRICTION', 31).
test_case('testsets/behavioral_mechanism_reading.pl', 'behavioral_mechanism_reading', 'BEHAVIORAL_MECHANISM_READING', 32).
test_case('testsets/behaviorist_counterfactual_reading.pl', 'behaviorist_counterfactual_reading', 'BEHAVIORIST_COUNTERFACTUAL_READING', 33).
test_case('testsets/benchmark_saturation_interpretation.pl', 'benchmark_saturation_interpretation', 'BENCHMARK_SATURATION_INTERPRETATION', 34).
test_case('testsets/benign_dictator_reading.pl', 'benign_dictator_reading', 'BENIGN_DICTATOR_READING', 35).
test_case('testsets/bitcoin_whitepaper_purpose__nakamoto_oracle_opacity.pl', 'bitcoin_whitepaper_purpose__nakamoto_oracle_opacity', 'BITCOIN_WHITEPAPER_PURPOSE__NAKAMOTO_ORACLE_OPACITY', 36).
test_case('testsets/blindness_decomposition_kernel_contradictions.pl', 'unknown_interval', 'BLINDNESS_DECOMPOSITION_KERNEL_CONTRADICTIONS', 37).
test_case('testsets/blindness_decomposition_kernel_flat_control.pl', 'blindness_decomposition_kernel_flat_control', 'BLINDNESS_DECOMPOSITION_KERNEL_FLAT_CONTROL', 38).
test_case('testsets/border_control_legitimacy__freedom_of_movement_primary.pl', 'border_control_legitimacy__freedom_of_movement_primary', 'BORDER_CONTROL_LEGITIMACY__FREEDOM_OF_MOVEMENT_PRIMARY', 39).
test_case('testsets/bounded_empathy_as_stabilizing_mechanism.pl', 'bounded_empathy_as_stabilizing_mechanism', 'BOUNDED_EMPATHY_AS_STABILIZING_MECHANISM', 40).
test_case('testsets/bounded_ownership_vs_capital_scale.pl', 'bounded_ownership_vs_capital_scale', 'BOUNDED_OWNERSHIP_VS_CAPITAL_SCALE', 41).
test_case('testsets/bully_gang_classification_law.pl', 'bully_gang_classification_law', 'BULLY_GANG_CLASSIFICATION_LAW', 42).
test_case('testsets/bureaucratic_drift_reading.pl', 'bureaucratic_drift_reading', 'BUREAUCRATIC_DRIFT_READING', 43).
test_case('testsets/capability_endogeneity.pl', 'capability_endogeneity', 'CAPABILITY_ENDOGENEITY', 44).
test_case('testsets/catastrophe_memory_kernel__boundary_maintenance_reading.pl', 'catastrophe_memory_kernel__boundary_maintenance_reading', 'CATASTROPHE_MEMORY_KERNEL__BOUNDARY_MAINTENANCE_READING', 45).
test_case('testsets/categorical_nonexistence_as_soft_denial.pl', 'categorical_nonexistence_as_soft_denial', 'CATEGORICAL_NONEXISTENCE_AS_SOFT_DENIAL', 46).
test_case('testsets/channel_conversion_reading.pl', 'channel_conversion_reading', 'CHANNEL_CONVERSION_READING', 47).
test_case('testsets/citation_purity_reading.pl', 'citation_purity_reading', 'CITATION_PURITY_READING', 48).
test_case('testsets/clock_incompatibility_reading.pl', 'clock_incompatibility_reading', 'CLOCK_INCOMPATIBILITY_READING', 49).
test_case('testsets/cold_reader_reading.pl', 'cold_reader_reading', 'COLD_READER_READING', 50).
test_case('testsets/collapse_inevitability_reading.pl', 'collapse_inevitability_reading', 'COLLAPSE_INEVITABILITY_READING', 51).
test_case('testsets/collective_refusal_as_sole_leverage.pl', 'collective_refusal_as_sole_leverage', 'COLLECTIVE_REFUSAL_AS_SOLE_LEVERAGE', 52).
test_case('testsets/commitment_reading.pl', 'commitment_reading', 'COMMITMENT_READING', 53).
test_case('testsets/conceptual_framework_reading.pl', 'conceptual_framework_reading', 'CONCEPTUAL_FRAMEWORK_READING', 54).
test_case('testsets/conduct_regulation_reading.pl', 'conduct_regulation_reading', 'CONDUCT_REGULATION_READING', 55).
test_case('testsets/constitutional_text_authority__living_constitutionalist_reading.pl', 'constitutional_text_authority__living_constitutionalist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__LIVING_CONSTITUTIONALIST_READING', 56).
test_case('testsets/constitutional_text_authority__originalist_reading.pl', 'constitutional_text_authority__originalist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__ORIGINALIST_READING', 57).
test_case('testsets/constitutional_text_authority__positivist_reading.pl', 'constitutional_text_authority__positivist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__POSITIVIST_READING', 58).
test_case('testsets/conversational_ai_consent_migration.pl', 'conversational_ai_consent_migration', 'CONVERSATIONAL_AI_CONSENT_MIGRATION', 59).
test_case('testsets/cooperative_artifact_legitimacy_contradictions.pl', 'unknown_interval', 'COOPERATIVE_ARTIFACT_LEGITIMACY_CONTRADICTIONS', 60).
test_case('testsets/cooperative_artifact_legitimacy_flat_control.pl', 'cooperative_artifact_legitimacy_flat_control', 'COOPERATIVE_ARTIFACT_LEGITIMACY_FLAT_CONTROL', 61).
test_case('testsets/coverage_neutral_reading.pl', 'coverage_neutral_reading', 'COVERAGE_NEUTRAL_READING', 62).
test_case('testsets/credible_cooperator_kernel_contradictions.pl', 'unknown_interval', 'CREDIBLE_COOPERATOR_KERNEL_CONTRADICTIONS', 63).
test_case('testsets/credible_cooperator_kernel_flat_control.pl', 'credible_cooperator_kernel_flat_control', 'CREDIBLE_COOPERATOR_KERNEL_FLAT_CONTROL', 64).
test_case('testsets/crypto_permissionless_reading.pl', 'crypto_permissionless_reading', 'CRYPTO_PERMISSIONLESS_READING', 65).
test_case('testsets/dataset_recycling_amplification.pl', 'dataset_recycling_amplification', 'DATASET_RECYCLING_AMPLIFICATION', 66).
test_case('testsets/deferred_consent_reading.pl', 'deferred_consent_reading', 'DEFERRED_CONSENT_READING', 67).
test_case('testsets/deflationary_reading.pl', 'deflationary_reading', 'DEFLATIONARY_READING', 68).
test_case('testsets/democratic_legitimacy_reading.pl', 'democratic_legitimacy_reading', 'DEMOCRATIC_LEGITIMACY_READING', 69).
test_case('testsets/demographic_resource_allocation.pl', 'demographic_resource_allocation', 'DEMOGRAPHIC_RESOURCE_ALLOCATION', 70).
test_case('testsets/demographic_skill_mismatch_c0.pl', 'demographic_skill_mismatch_c0', 'DEMOGRAPHIC_SKILL_MISMATCH_C0', 71).
test_case('testsets/developmental_harm_reading.pl', 'developmental_harm_reading', 'DEVELOPMENTAL_HARM_READING', 72).
test_case('testsets/diagnostic_taxonomy_blind_spot.pl', 'diagnostic_taxonomy_blind_spot', 'DIAGNOSTIC_TAXONOMY_BLIND_SPOT', 73).
test_case('testsets/digital_money_legitimacy_contradictions.pl', 'unknown_interval', 'DIGITAL_MONEY_LEGITIMACY_CONTRADICTIONS', 74).
test_case('testsets/digital_money_legitimacy_flat_control.pl', 'digital_money_legitimacy_flat_control', 'DIGITAL_MONEY_LEGITIMACY_FLAT_CONTROL', 75).
test_case('testsets/disciplined_self_distrust_as_inherited_suppression.pl', 'disciplined_self_distrust_as_inherited_suppression', 'DISCIPLINED_SELF_DISTRUST_AS_INHERITED_SUPPRESSION', 76).
test_case('testsets/dispositional_reading.pl', 'dispositional_reading', 'DISPOSITIONAL_READING', 77).
test_case('testsets/distributed_verification.pl', 'distributed_verification', 'DISTRIBUTED_VERIFICATION', 78).
test_case('testsets/divine_legitimacy_substrate__folk_syncretistic_reading.pl', 'divine_legitimacy_substrate__folk_syncretistic_reading', 'DIVINE_LEGITIMACY_SUBSTRATE__FOLK_SYNCRETISTIC_READING', 79).
test_case('testsets/domestic_currency_vs_external_settlement_seam.pl', 'domestic_currency_vs_external_settlement_seam', 'DOMESTIC_CURRENCY_VS_EXTERNAL_SETTLEMENT_SEAM', 80).
test_case('testsets/doomsday_clock_metric__hybrid_legitimacy_reading.pl', 'doomsday_clock_metric__hybrid_legitimacy_reading', 'DOOMSDAY_CLOCK_METRIC__HYBRID_LEGITIMACY_READING', 81).
test_case('testsets/drift_reading.pl', 'drift_reading', 'DRIFT_READING', 82).
test_case('testsets/dueling_disappearance_mechanism__overdetermined_composite_reading.pl', 'dueling_disappearance_mechanism__overdetermined_composite_reading', 'DUELING_DISAPPEARANCE_MECHANISM__OVERDETERMINED_COMPOSITE_READING', 83).
test_case('testsets/emotivism_as_diagnosis_vs_practice.pl', 'emotivism_as_diagnosis_vs_practice', 'EMOTIVISM_AS_DIAGNOSIS_VS_PRACTICE', 84).
test_case('testsets/empathy_simulation_reading.pl', 'empathy_simulation_reading', 'EMPATHY_SIMULATION_READING', 85).
test_case('testsets/empirical_precedent_reading.pl', 'empirical_precedent_reading', 'EMPIRICAL_PRECEDENT_READING', 86).
test_case('testsets/empty_shell_tolerance.pl', 'empty_shell_tolerance', 'EMPTY_SHELL_TOLERANCE', 87).
test_case('testsets/epistemic_collapse.pl', 'epistemic_collapse', 'EPISTEMIC_COLLAPSE', 88).
test_case('testsets/epistemic_inadmissibility_of_tacit_expertise.pl', 'epistemic_inadmissibility_of_tacit_expertise', 'EPISTEMIC_INADMISSIBILITY_OF_TACIT_EXPERTISE', 89).
test_case('testsets/epsilon_substrate_dependency.pl', 'epsilon_substrate_dependency', 'EPSILON_SUBSTRATE_DEPENDENCY', 90).
test_case('testsets/equal_protection_kernel__colorblind_reading.pl', 'equal_protection_kernel__colorblind_reading', 'EQUAL_PROTECTION_KERNEL__COLORBLIND_READING', 91).
test_case('testsets/evaluator_incentive_asymmetry.pl', 'evaluator_incentive_asymmetry', 'EVALUATOR_INCENTIVE_ASYMMETRY', 92).
test_case('testsets/exile_reading.pl', 'exile_reading', 'EXILE_READING', 93).
test_case('testsets/exit_option_reading.pl', 'exit_option_reading', 'EXIT_OPTION_READING', 94).
test_case('testsets/exit_suppression_mechanism.pl', 'exit_suppression_mechanism', 'EXIT_SUPPRESSION_MECHANISM', 95).
test_case('testsets/expressive_attribution_reading.pl', 'expressive_attribution_reading', 'EXPRESSIVE_ATTRIBUTION_READING', 96).
test_case('testsets/family_attention_reallocation.pl', 'family_attention_reallocation', 'FAMILY_ATTENTION_REALLOCATION', 97).
test_case('testsets/federation_membership__integration_reading.pl', 'federation_membership__integration_reading', 'FEDERATION_MEMBERSHIP__INTEGRATION_READING', 98).
test_case('testsets/federation_membership__sovereignty_reading.pl', 'federation_membership__sovereignty_reading', 'FEDERATION_MEMBERSHIP__SOVEREIGNTY_READING', 99).
test_case('testsets/fetterley_transfer_kernel_contradictions.pl', 'unknown_interval', 'FETTERLEY_TRANSFER_KERNEL_CONTRADICTIONS', 100).
test_case('testsets/fetterley_transfer_kernel_flat_control.pl', 'fetterley_transfer_kernel_flat_control', 'FETTERLEY_TRANSFER_KERNEL_FLAT_CONTROL', 101).
test_case('testsets/fiat_efficacy_kernel_contradictions.pl', 'unknown_interval', 'FIAT_EFFICACY_KERNEL_CONTRADICTIONS', 102).
test_case('testsets/fiat_efficacy_kernel_flat_control.pl', 'fiat_efficacy_kernel_flat_control', 'FIAT_EFFICACY_KERNEL_FLAT_CONTROL', 103).
test_case('testsets/fictional_construct_reading.pl', 'fictional_construct_reading', 'FICTIONAL_CONSTRUCT_READING', 104).
test_case('testsets/filter_reading.pl', 'filter_reading', 'FILTER_READING', 105).
test_case('testsets/flow_extraction_reading.pl', 'flow_extraction_reading', 'FLOW_EXTRACTION_READING', 106).
test_case('testsets/folk_mountain_reading.pl', 'folk_mountain_reading', 'FOLK_MOUNTAIN_READING', 107).
test_case('testsets/fourteenth_amendment_equal_protection__formal_equality_reading.pl', 'fourteenth_amendment_equal_protection__formal_equality_reading', 'FOURTEENTH_AMENDMENT_EQUAL_PROTECTION__FORMAL_EQUALITY_READING', 108).
test_case('testsets/frame_independence_reading.pl', 'frame_independence_reading', 'FRAME_INDEPENDENCE_READING', 109).
test_case('testsets/fusion_reading.pl', 'fusion_reading', 'FUSION_READING', 110).
test_case('testsets/future_claims_present_resources_contradictions.pl', 'unknown_interval', 'FUTURE_CLAIMS_PRESENT_RESOURCES_CONTRADICTIONS', 111).
test_case('testsets/future_claims_present_resources_flat_control.pl', 'future_claims_present_resources_flat_control', 'FUTURE_CLAIMS_PRESENT_RESOURCES_FLAT_CONTROL', 112).
test_case('testsets/gendered_outcome_asymmetry.pl', 'gendered_outcome_asymmetry', 'GENDERED_OUTCOME_ASYMMETRY', 113).
test_case('testsets/genealogical_origin_reading.pl', 'genealogical_origin_reading', 'GENEALOGICAL_ORIGIN_READING', 114).
test_case('testsets/generality_standard_contradictions.pl', 'unknown_interval', 'GENERALITY_STANDARD_CONTRADICTIONS', 115).
test_case('testsets/generality_standard_flat_control.pl', 'generality_standard_flat_control', 'GENERALITY_STANDARD_FLAT_CONTROL', 116).
test_case('testsets/generation_gate_reading.pl', 'generation_gate_reading', 'GENERATION_GATE_READING', 117).
test_case('testsets/genuine_relational_understanding_contradictions.pl', 'unknown_interval', 'GENUINE_RELATIONAL_UNDERSTANDING_CONTRADICTIONS', 118).
test_case('testsets/geopolitical_settlement_competition.pl', 'geopolitical_settlement_competition', 'GEOPOLITICAL_SETTLEMENT_COMPETITION', 119).
test_case('testsets/gita_kurukshetra_discourse__gandhian_allegorical_reading.pl', 'gita_kurukshetra_discourse__gandhian_allegorical_reading', 'GITA_KURUKSHETRA_DISCOURSE__GANDHIAN_ALLEGORICAL_READING', 120).
test_case('testsets/gpl_derivative_work_trigger__interface_boundary_reading.pl', 'gpl_derivative_work_trigger__interface_boundary_reading', 'GPL_DERIVATIVE_WORK_TRIGGER__INTERFACE_BOUNDARY_READING', 121).
test_case('testsets/h2a_exit_mobility_axis.pl', 'h2a_exit_mobility_axis', 'H2A_EXIT_MOBILITY_AXIS', 122).
test_case('testsets/historical_treaty_substrate__stewardship_reading.pl', 'historical_treaty_substrate__stewardship_reading', 'HISTORICAL_TREATY_SUBSTRATE__STEWARDSHIP_READING', 123).
test_case('testsets/impression_management_reading.pl', 'impression_management_reading', 'IMPRESSION_MANAGEMENT_READING', 124).
test_case('testsets/indexical_realism.pl', 'indexical_realism', 'INDEXICAL_REALISM', 125).
test_case('testsets/information_control_regime.pl', 'information_control_regime', 'INFORMATION_CONTROL_REGIME', 126).
test_case('testsets/installed_authorship_reading.pl', 'installed_authorship_reading', 'INSTALLED_AUTHORSHIP_READING', 127).
test_case('testsets/institutional_barrier_structure.pl', 'institutional_barrier_structure', 'INSTITUTIONAL_BARRIER_STRUCTURE', 128).
test_case('testsets/institutional_trust_erosion_c0.pl', 'institutional_trust_erosion_c0', 'INSTITUTIONAL_TRUST_EROSION_C0', 129).
test_case('testsets/institutional_validation_reading.pl', 'institutional_validation_reading', 'INSTITUTIONAL_VALIDATION_READING', 130).
test_case('testsets/institutional_verification_collapse.pl', 'institutional_verification_collapse', 'INSTITUTIONAL_VERIFICATION_COLLAPSE', 131).
test_case('testsets/instrument_capture_reading.pl', 'instrument_capture_reading', 'INSTRUMENT_CAPTURE_READING', 132).
test_case('testsets/instrument_dependent_reading.pl', 'instrument_dependent_reading', 'INSTRUMENT_DEPENDENT_READING', 133).
test_case('testsets/instrumentalist_reading.pl', 'instrumentalist_reading', 'INSTRUMENTALIST_READING', 134).
test_case('testsets/interactionist_reading.pl', 'interactionist_reading', 'INTERACTIONIST_READING', 135).
test_case('testsets/intervention_target_selection.pl', 'intervention_target_selection', 'INTERVENTION_TARGET_SELECTION', 136).
test_case('testsets/issuance_as_deliberative_judgment.pl', 'issuance_as_deliberative_judgment', 'ISSUANCE_AS_DELIBERATIVE_JUDGMENT', 137).
test_case('testsets/issuance_as_market_discovered_confidence.pl', 'issuance_as_market_discovered_confidence', 'ISSUANCE_AS_MARKET_DISCOVERED_CONFIDENCE', 138).
test_case('testsets/issuance_as_physical_backing.pl', 'issuance_as_physical_backing', 'ISSUANCE_AS_PHYSICAL_BACKING', 139).
test_case('testsets/jewish_self_determination__indigenous_return_reading.pl', 'jewish_self_determination__indigenous_return_reading', 'JEWISH_SELF_DETERMINATION__INDIGENOUS_RETURN_READING', 140).
test_case('testsets/jewish_sovereignty_palestine__cultural_zionist_reading.pl', 'jewish_sovereignty_palestine__cultural_zionist_reading', 'JEWISH_SOVEREIGNTY_PALESTINE__CULTURAL_ZIONIST_READING', 141).
test_case('testsets/jewish_sovereignty_palestine__settler_colonial_reading.pl', 'jewish_sovereignty_palestine__settler_colonial_reading', 'JEWISH_SOVEREIGNTY_PALESTINE__SETTLER_COLONIAL_READING', 142).
test_case('testsets/jewish_territorial_claim__labor_zionism_reading.pl', 'jewish_territorial_claim__labor_zionism_reading', 'JEWISH_TERRITORIAL_CLAIM__LABOR_ZIONISM_READING', 143).
test_case('testsets/john_1_1_logos__non_incarnational_monotheist.pl', 'john_1_1_logos__non_incarnational_monotheist', 'JOHN_1_1_LOGOS__NON_INCARNATIONAL_MONOTHEIST', 144).
test_case('testsets/knowledge_legitimacy_biomedicine_contradictions.pl', 'unknown_interval', 'KNOWLEDGE_LEGITIMACY_BIOMEDICINE_CONTRADICTIONS', 145).
test_case('testsets/knowledge_legitimacy_biomedicine_flat_control.pl', 'knowledge_legitimacy_biomedicine_flat_control', 'KNOWLEDGE_LEGITIMACY_BIOMEDICINE_FLAT_CONTROL', 146).
test_case('testsets/lausanne_minority_protections__guarantor_reading.pl', 'lausanne_minority_protections__guarantor_reading', 'LAUSANNE_MINORITY_PROTECTIONS__GUARANTOR_READING', 147).
test_case('testsets/learning_difficulty_substrate_contradictions.pl', 'unknown_interval', 'LEARNING_DIFFICULTY_SUBSTRATE_CONTRADICTIONS', 148).
test_case('testsets/learning_difficulty_substrate_flat_control.pl', 'learning_difficulty_substrate_flat_control', 'LEARNING_DIFFICULTY_SUBSTRATE_FLAT_CONTROL', 149).
test_case('testsets/legibility_primacy_reading.pl', 'legibility_primacy_reading', 'LEGIBILITY_PRIMACY_READING', 150).
test_case('testsets/liability_termination_visibility.pl', 'liability_termination_visibility', 'LIABILITY_TERMINATION_VISIBILITY', 151).
test_case('testsets/livelihood_security_reading.pl', 'livelihood_security_reading', 'LIVELIHOOD_SECURITY_READING', 152).
test_case('testsets/llm_synthesis_capacity.pl', 'llm_synthesis_capacity', 'LLM_SYNTHESIS_CAPACITY', 153).
test_case('testsets/longevity_mismatch.pl', 'longevity_mismatch', 'LONGEVITY_MISMATCH', 154).
test_case('testsets/lycurgan_laws__demographic_trap_reading.pl', 'lycurgan_laws__demographic_trap_reading', 'LYCURGAN_LAWS__DEMOGRAPHIC_TRAP_READING', 155).
test_case('testsets/maat_order_principle__reciprocity_reading.pl', 'maat_order_principle__reciprocity_reading', 'MAAT_ORDER_PRINCIPLE__RECIPROCITY_READING', 156).
test_case('testsets/marriage_authority__judicial_harmonization_reading.pl', 'marriage_authority__judicial_harmonization_reading', 'MARRIAGE_AUTHORITY__JUDICIAL_HARMONIZATION_READING', 157).
test_case('testsets/measurement_architecture_reading.pl', 'measurement_architecture_reading', 'MEASUREMENT_ARCHITECTURE_READING', 158).
test_case('testsets/measurement_authority_decoupling.pl', 'measurement_authority_decoupling', 'MEASUREMENT_AUTHORITY_DECOUPLING', 159).
test_case('testsets/mechanism_defensibility_burden.pl', 'mechanism_defensibility_burden', 'MECHANISM_DEFENSIBILITY_BURDEN', 160).
test_case('testsets/menu_curation_capture.pl', 'menu_curation_capture', 'MENU_CURATION_CAPTURE', 161).
test_case('testsets/meta_prediction_reading.pl', 'meta_prediction_reading', 'META_PREDICTION_READING', 162).
test_case('testsets/mitochondrial_demand_signal_deficiency.pl', 'mitochondrial_demand_signal_deficiency', 'MITOCHONDRIAL_DEMAND_SIGNAL_DEFICIENCY', 163).
test_case('testsets/mixed_sector_survival_bind.pl', 'mixed_sector_survival_bind', 'MIXED_SECTOR_SURVIVAL_BIND', 164).
test_case('testsets/model_collapse_feedback.pl', 'model_collapse_feedback', 'MODEL_COLLAPSE_FEEDBACK', 165).
test_case('testsets/money_governance_coupling_contradictions.pl', 'unknown_interval', 'MONEY_GOVERNANCE_COUPLING_CONTRADICTIONS', 166).
test_case('testsets/money_governance_coupling_flat_control.pl', 'money_governance_coupling_flat_control', 'MONEY_GOVERNANCE_COUPLING_FLAT_CONTROL', 167).
test_case('testsets/moral_causation_locus_contradictions.pl', 'unknown_interval', 'MORAL_CAUSATION_LOCUS_CONTRADICTIONS', 168).
test_case('testsets/moral_causation_locus_flat_control.pl', 'moral_causation_locus_flat_control', 'MORAL_CAUSATION_LOCUS_FLAT_CONTROL', 169).
test_case('testsets/nad_precursor_bioavailability.pl', 'nad_precursor_bioavailability', 'NAD_PRECURSOR_BIOAVAILABILITY', 170).
test_case('testsets/negative_vs_positive_decision_structure.pl', 'negative_vs_positive_decision_structure', 'NEGATIVE_VS_POSITIVE_DECISION_STRUCTURE', 171).
test_case('testsets/negotiated_agency_reading.pl', 'negotiated_agency_reading', 'NEGOTIATED_AGENCY_READING', 172).
test_case('testsets/neutron_star_bombardment_reading.pl', 'neutron_star_bombardment_reading', 'NEUTRON_STAR_BOMBARDMENT_READING', 173).
test_case('testsets/nicene_creed_authority__liturgical_habituation_reading.pl', 'nicene_creed_authority__liturgical_habituation_reading', 'NICENE_CREED_AUTHORITY__LITURGICAL_HABITUATION_READING', 174).
test_case('testsets/nonperturbative_matter_sector_reading.pl', 'nonperturbative_matter_sector_reading', 'NONPERTURBATIVE_MATTER_SECTOR_READING', 175).
test_case('testsets/notability_guidelines__deliberative_reading.pl', 'notability_guidelines__deliberative_reading', 'NOTABILITY_GUIDELINES__DELIBERATIVE_READING', 176).
test_case('testsets/omega_production_cost_asymmetry.pl', 'omega_production_cost_asymmetry', 'OMEGA_PRODUCTION_COST_ASYMMETRY', 177).
test_case('testsets/ontological_commitment_reading.pl', 'ontological_commitment_reading', 'ONTOLOGICAL_COMMITMENT_READING', 178).
test_case('testsets/operational_security_reading.pl', 'operational_security_reading', 'OPERATIONAL_SECURITY_READING', 179).
test_case('testsets/organization_floor_c0.pl', 'organization_floor_c0', 'ORGANIZATION_FLOOR_C0', 180).
test_case('testsets/paper_ready_boundary_flat_control.pl', 'paper_ready_boundary_flat_control', 'PAPER_READY_BOUNDARY_FLAT_CONTROL', 181).
test_case('testsets/partition_choice_reading.pl', 'partition_choice_reading', 'PARTITION_CHOICE_READING', 182).
test_case('testsets/performance_legitimacy_contradictions.pl', 'unknown_interval', 'PERFORMANCE_LEGITIMACY_CONTRADICTIONS', 183).
test_case('testsets/performance_legitimacy_flat_control.pl', 'performance_legitimacy_flat_control', 'PERFORMANCE_LEGITIMACY_FLAT_CONTROL', 184).
test_case('testsets/persona_as_valid_proxy_contradictions.pl', 'unknown_interval', 'PERSONA_AS_VALID_PROXY_CONTRADICTIONS', 185).
test_case('testsets/persona_as_valid_proxy_flat_control.pl', 'persona_as_valid_proxy_flat_control', 'PERSONA_AS_VALID_PROXY_FLAT_CONTROL', 186).
test_case('testsets/personhood_boundary_kernel_contradictions.pl', 'unknown_interval', 'PERSONHOOD_BOUNDARY_KERNEL_CONTRADICTIONS', 187).
test_case('testsets/personhood_boundary_kernel_flat_control.pl', 'personhood_boundary_kernel_flat_control', 'PERSONHOOD_BOUNDARY_KERNEL_FLAT_CONTROL', 188).
test_case('testsets/personhood_continuity_reading.pl', 'personhood_continuity_reading', 'PERSONHOOD_CONTINUITY_READING', 189).
test_case('testsets/phenomenological_endorsement_reading.pl', 'phenomenological_endorsement_reading', 'PHENOMENOLOGICAL_ENDORSEMENT_READING', 190).
test_case('testsets/phenomenological_program_reading.pl', 'phenomenological_program_reading', 'PHENOMENOLOGICAL_PROGRAM_READING', 191).
test_case('testsets/polaris_document_status_contradictions.pl', 'unknown_interval', 'POLARIS_DOCUMENT_STATUS_CONTRADICTIONS', 192).
test_case('testsets/polaris_document_status_flat_control.pl', 'polaris_document_status_flat_control', 'POLARIS_DOCUMENT_STATUS_FLAT_CONTROL', 193).
test_case('testsets/positional_disagreement_as_evidence_contradictions.pl', 'unknown_interval', 'POSITIONAL_DISAGREEMENT_AS_EVIDENCE_CONTRADICTIONS', 194).
test_case('testsets/positional_disagreement_as_evidence_flat_control.pl', 'positional_disagreement_as_evidence_flat_control', 'POSITIONAL_DISAGREEMENT_AS_EVIDENCE_FLAT_CONTROL', 195).
test_case('testsets/post_evidentiary.pl', 'post_evidentiary', 'POST_EVIDENTIARY', 196).
test_case('testsets/pragmatic_action_reading.pl', 'pragmatic_action_reading', 'PRAGMATIC_ACTION_READING', 197).
test_case('testsets/pragmatist_reading.pl', 'pragmatist_reading', 'PRAGMATIST_READING', 198).
test_case('testsets/pre_public_initiative_reading.pl', 'pre_public_initiative_reading', 'PRE_PUBLIC_INITIATIVE_READING', 199).
test_case('testsets/predictive_synthesis_reading.pl', 'predictive_synthesis_reading', 'PREDICTIVE_SYNTHESIS_READING', 200).
test_case('testsets/prerequisite_debt_reading.pl', 'prerequisite_debt_reading', 'PREREQUISITE_DEBT_READING', 201).
test_case('testsets/presentation_audit_reading.pl', 'presentation_audit_reading', 'PRESENTATION_AUDIT_READING', 202).
test_case('testsets/press_reformation_causality__strategic_deployment.pl', 'press_reformation_causality__strategic_deployment', 'PRESS_REFORMATION_CAUSALITY__STRATEGIC_DEPLOYMENT', 203).
test_case('testsets/press_reformation_causation__mutual_shaping.pl', 'press_reformation_causation__mutual_shaping', 'PRESS_REFORMATION_CAUSATION__MUTUAL_SHAPING', 204).
test_case('testsets/press_reformation_causation__strategic_deployment.pl', 'press_reformation_causation__strategic_deployment', 'PRESS_REFORMATION_CAUSATION__STRATEGIC_DEPLOYMENT', 205).
test_case('testsets/procedural_fairness_as_severity_laundering.pl', 'procedural_fairness_as_severity_laundering', 'PROCEDURAL_FAIRNESS_AS_SEVERITY_LAUNDERING', 206).
test_case('testsets/proceduralist_reading.pl', 'proceduralist_reading', 'PROCEDURALIST_READING', 207).
test_case('testsets/process_transparency_reading.pl', 'process_transparency_reading', 'PROCESS_TRANSPARENCY_READING', 208).
test_case('testsets/procurement_inertia.pl', 'procurement_inertia', 'PROCUREMENT_INERTIA', 209).
test_case('testsets/products_liability_reading.pl', 'products_liability_reading', 'PRODUCTS_LIABILITY_READING', 210).
test_case('testsets/propagation_speed_asymmetry.pl', 'propagation_speed_asymmetry', 'PROPAGATION_SPEED_ASYMMETRY', 211).
test_case('testsets/property_sector_overhang.pl', 'property_sector_overhang', 'PROPERTY_SECTOR_OVERHANG', 212).
test_case('testsets/protective_fiction_as_distributed_liability.pl', 'protective_fiction_as_distributed_liability', 'PROTECTIVE_FICTION_AS_DISTRIBUTED_LIABILITY', 213).
test_case('testsets/protein_anabolic_resistance.pl', 'protein_anabolic_resistance', 'PROTEIN_ANABOLIC_RESISTANCE', 214).
test_case('testsets/public_risk_reading.pl', 'public_risk_reading', 'PUBLIC_RISK_READING', 215).
test_case('testsets/qualified_immunity_doctrine__protective_scaffold_reading.pl', 'qualified_immunity_doctrine__protective_scaffold_reading', 'QUALIFIED_IMMUNITY_DOCTRINE__PROTECTIVE_SCAFFOLD_READING', 216).
test_case('testsets/qualitative_development_reading.pl', 'qualitative_development_reading', 'QUALITATIVE_DEVELOPMENT_READING', 217).
test_case('testsets/quantitative_growth_reading.pl', 'quantitative_growth_reading', 'QUANTITATIVE_GROWTH_READING', 218).
test_case('testsets/radiative_levitation_stratification.pl', 'radiative_levitation_stratification', 'RADIATIVE_LEVITATION_STRATIFICATION', 219).
test_case('testsets/reading_acquisition_legitimacy__whole_language_meaning_primacy.pl', 'reading_acquisition_legitimacy__whole_language_meaning_primacy', 'READING_ACQUISITION_LEGITIMACY__WHOLE_LANGUAGE_MEANING_PRIMACY', 220).
test_case('testsets/redistributive_stabilization_reading.pl', 'redistributive_stabilization_reading', 'REDISTRIBUTIVE_STABILIZATION_READING', 221).
test_case('testsets/refugee_convention_text__expansive_humanitarian_reading.pl', 'refugee_convention_text__expansive_humanitarian_reading', 'REFUGEE_CONVENTION_TEXT__EXPANSIVE_HUMANITARIAN_READING', 222).
test_case('testsets/regulated_stablecoin_reading.pl', 'regulated_stablecoin_reading', 'REGULATED_STABLECOIN_READING', 223).
test_case('testsets/regulatory_lag_extraction.pl', 'regulatory_lag_extraction', 'REGULATORY_LAG_EXTRACTION', 224).
test_case('testsets/relational_obligation_reading.pl', 'relational_obligation_reading', 'RELATIONAL_OBLIGATION_READING', 225).
test_case('testsets/representational_correspondence_reading.pl', 'representational_correspondence_reading', 'REPRESENTATIONAL_CORRESPONDENCE_READING', 226).
test_case('testsets/responsibility_preservation_mechanism.pl', 'responsibility_preservation_mechanism', 'RESPONSIBILITY_PRESERVATION_MECHANISM', 227).
test_case('testsets/ritual_transmission_as_double_edged_inheritance.pl', 'ritual_transmission_as_double_edged_inheritance', 'RITUAL_TRANSMISSION_AS_DOUBLE_EDGED_INHERITANCE', 228).
test_case('testsets/scale_ceiling_c0.pl', 'scale_ceiling_c0', 'SCALE_CEILING_C0', 229).
test_case('testsets/scholarship_reading.pl', 'scholarship_reading', 'SCHOLARSHIP_READING', 230).
test_case('testsets/seat_gauge_orientation_kernel_flat_control.pl', 'seat_gauge_orientation_kernel_flat_control', 'SEAT_GAUGE_ORIENTATION_KERNEL_FLAT_CONTROL', 231).
test_case('testsets/secession_legitimacy_boundary__constitutional_impossibility_reading.pl', 'secession_legitimacy_boundary__constitutional_impossibility_reading', 'SECESSION_LEGITIMACY_BOUNDARY__CONSTITUTIONAL_IMPOSSIBILITY_READING', 232).
test_case('testsets/sex_gender_category__identity_reading.pl', 'sex_gender_category__identity_reading', 'SEX_GENDER_CATEGORY__IDENTITY_READING', 233).
test_case('testsets/shared_backbone_self_preference_confound.pl', 'shared_backbone_self_preference_confound', 'SHARED_BACKBONE_SELF_PREFERENCE_CONFOUND', 234).
test_case('testsets/shinbutsu_ontological_commitment__incoherence_reading.pl', 'shinbutsu_ontological_commitment__incoherence_reading', 'SHINBUTSU_ONTOLOGICAL_COMMITMENT__INCOHERENCE_READING', 235).
test_case('testsets/signaling_market_reading.pl', 'signaling_market_reading', 'SIGNALING_MARKET_READING', 236).
test_case('testsets/silence_dependent_ritual_integrity.pl', 'silence_dependent_ritual_integrity', 'SILENCE_DEPENDENT_RITUAL_INTEGRITY', 237).
test_case('testsets/simulation_reading.pl', 'simulation_reading', 'SIMULATION_READING', 238).
test_case('testsets/situational_reading.pl', 'situational_reading', 'SITUATIONAL_READING', 239).
test_case('testsets/skills_mismatch_reading.pl', 'skills_mismatch_reading', 'SKILLS_MISMATCH_READING', 240).
test_case('testsets/sociotechnical_risk_reading.pl', 'sociotechnical_risk_reading', 'SOCIOTECHNICAL_RISK_READING', 241).
test_case('testsets/sovereign_cbdc_reading.pl', 'sovereign_cbdc_reading', 'SOVEREIGN_CBDC_READING', 242).
test_case('testsets/specification_binding_authority.pl', 'specification_binding_authority', 'SPECIFICATION_BINDING_AUTHORITY', 243).
test_case('testsets/speech_protection_kernel__absolutist_reading.pl', 'speech_protection_kernel__absolutist_reading', 'SPEECH_PROTECTION_KERNEL__ABSOLUTIST_READING', 244).
test_case('testsets/stability_legitimacy_kernel_contradictions.pl', 'unknown_interval', 'STABILITY_LEGITIMACY_KERNEL_CONTRADICTIONS', 245).
test_case('testsets/stability_legitimacy_kernel_flat_control.pl', 'stability_legitimacy_kernel_flat_control', 'STABILITY_LEGITIMACY_KERNEL_FLAT_CONTROL', 246).
test_case('testsets/stance_reading.pl', 'stance_reading', 'STANCE_READING', 247).
test_case('testsets/standpoint_reading.pl', 'standpoint_reading', 'STANDPOINT_READING', 248).
test_case('testsets/state_killing_legitimacy__abolition_reading.pl', 'state_killing_legitimacy__abolition_reading', 'STATE_KILLING_LEGITIMACY__ABOLITION_READING', 249).
test_case('testsets/statute_of_anne_ip_foundation__entangled_event_reading.pl', 'statute_of_anne_ip_foundation__entangled_event_reading', 'STATUTE_OF_ANNE_IP_FOUNDATION__ENTANGLED_EVENT_READING', 250).
test_case('testsets/statutory_debt_ceiling__coordination_scaffold_reading.pl', 'statutory_debt_ceiling__coordination_scaffold_reading', 'STATUTORY_DEBT_CEILING__COORDINATION_SCAFFOLD_READING', 251).
test_case('testsets/sufficiency_reading.pl', 'sufficiency_reading', 'SUFFICIENCY_READING', 252).
test_case('testsets/superheavy_decay_reading.pl', 'superheavy_decay_reading', 'SUPERHEAVY_DECAY_READING', 253).
test_case('testsets/surveillance_productivity_dividend.pl', 'surveillance_productivity_dividend', 'SURVEILLANCE_PRODUCTIVITY_DIVIDEND', 254).
test_case('testsets/synthesis_infrastructure_gap.pl', 'synthesis_infrastructure_gap', 'SYNTHESIS_INFRASTRUCTURE_GAP', 255).
test_case('testsets/technician_intent_reading.pl', 'technician_intent_reading', 'TECHNICIAN_INTENT_READING', 256).
test_case('testsets/techno_nationalist_reading.pl', 'techno_nationalist_reading', 'TECHNO_NATIONALIST_READING', 257).
test_case('testsets/technological_displacement_axiom_contradictions.pl', 'unknown_interval', 'TECHNOLOGICAL_DISPLACEMENT_AXIOM_CONTRADICTIONS', 258).
test_case('testsets/technological_displacement_axiom_flat_control.pl', 'technological_displacement_axiom_flat_control', 'TECHNOLOGICAL_DISPLACEMENT_AXIOM_FLAT_CONTROL', 259).
test_case('testsets/technology_diffusion_asymmetry.pl', 'technology_diffusion_asymmetry', 'TECHNOLOGY_DIFFUSION_ASYMMETRY', 260).
test_case('testsets/tempo_margin_regime.pl', 'tempo_margin_regime', 'TEMPO_MARGIN_REGIME', 261).
test_case('testsets/temporal_equivalence_reading.pl', 'temporal_equivalence_reading', 'TEMPORAL_EQUIVALENCE_READING', 262).
test_case('testsets/termination_and_falsifiability_of_review_loops.pl', 'termination_and_falsifiability_of_review_loops', 'TERMINATION_AND_FALSIFIABILITY_OF_REVIEW_LOOPS', 263).
test_case('testsets/textualist_severability_reading.pl', 'textualist_severability_reading', 'TEXTUALIST_SEVERABILITY_READING', 264).
test_case('testsets/third_act_arbitrage.pl', 'third_act_arbitrage', 'THIRD_ACT_ARBITRAGE', 265).
test_case('testsets/tool_reading.pl', 'tool_reading', 'TOOL_READING', 266).
test_case('testsets/trajectory_extrapolation_reading.pl', 'trajectory_extrapolation_reading', 'TRAJECTORY_EXTRAPOLATION_READING', 267).
test_case('testsets/truth_procedure_reading.pl', 'truth_procedure_reading', 'TRUTH_PROCEDURE_READING', 268).
test_case('testsets/udhr_article_3__negative_liberty_reading.pl', 'udhr_article_3__negative_liberty_reading', 'UDHR_ARTICLE_3__NEGATIVE_LIBERTY_READING', 269).
test_case('testsets/unaudited_contentment_as_entitlement.pl', 'unaudited_contentment_as_entitlement', 'UNAUDITED_CONTENTMENT_AS_ENTITLEMENT', 270).
test_case('testsets/unaudited_reserve_asymmetry.pl', 'unaudited_reserve_asymmetry', 'UNAUDITED_RESERVE_ASYMMETRY', 271).
test_case('testsets/unranked_substrate_as_negative_commons.pl', 'unranked_substrate_as_negative_commons', 'UNRANKED_SUBSTRATE_AS_NEGATIVE_COMMONS', 272).
test_case('testsets/unregulated_psychological_experimentation.pl', 'unregulated_psychological_experimentation', 'UNREGULATED_PSYCHOLOGICAL_EXPERIMENTATION', 273).
test_case('testsets/unsettled_claim_ontology_contradictions.pl', 'unknown_interval', 'UNSETTLED_CLAIM_ONTOLOGY_CONTRADICTIONS', 274).
test_case('testsets/unsettled_claim_ontology_flat_control.pl', 'unsettled_claim_ontology_flat_control', 'UNSETTLED_CLAIM_ONTOLOGY_FLAT_CONTROL', 275).
test_case('testsets/utopian_fiction_reading.pl', 'utopian_fiction_reading', 'UTOPIAN_FICTION_READING', 276).
test_case('testsets/validation_judgment_separation.pl', 'validation_judgment_separation', 'VALIDATION_JUDGMENT_SEPARATION', 277).
test_case('testsets/vedic_corpus_social_prescription__colonial_orientalist_reading.pl', 'vedic_corpus_social_prescription__colonial_orientalist_reading', 'VEDIC_CORPUS_SOCIAL_PRESCRIPTION__COLONIAL_ORIENTALIST_READING', 278).
test_case('testsets/verification_cost_as_constitutional_precondition.pl', 'verification_cost_as_constitutional_precondition', 'VERIFICATION_COST_AS_CONSTITUTIONAL_PRECONDITION', 279).
test_case('testsets/verification_prohibition_as_self_defeating_trial.pl', 'verification_prohibition_as_self_defeating_trial', 'VERIFICATION_PROHIBITION_AS_SELF_DEFEATING_TRIAL', 280).
test_case('testsets/victim_self_attribution_foreclosure.pl', 'victim_self_attribution_foreclosure', 'VICTIM_SELF_ATTRIBUTION_FORECLOSURE', 281).
test_case('testsets/virtue_performance_as_exculpation.pl', 'virtue_performance_as_exculpation', 'VIRTUE_PERFORMANCE_AS_EXCULPATION', 282).
test_case('testsets/visual_evidentiary_authority_contradictions.pl', 'unknown_interval', 'VISUAL_EVIDENTIARY_AUTHORITY_CONTRADICTIONS', 283).
test_case('testsets/visual_evidentiary_authority_flat_control.pl', 'visual_evidentiary_authority_flat_control', 'VISUAL_EVIDENTIARY_AUTHORITY_FLAT_CONTROL', 284).
test_case('testsets/vocabulary_collision_reading.pl', 'vocabulary_collision_reading', 'VOCABULARY_COLLISION_READING', 285).
test_case('testsets/voice_without_leverage.pl', 'voice_without_leverage', 'VOICE_WITHOUT_LEVERAGE', 286).
test_case('testsets/weaponization_accessibility.pl', 'weaponization_accessibility', 'WEAPONIZATION_ACCESSIBILITY', 287).
test_case('testsets/womens_financial_autonomy.pl', 'womens_financial_autonomy', 'WOMENS_FINANCIAL_AUTONOMY', 288).
test_case('testsets/zero_mathematical_status__parmenidean_rejection.pl', 'zero_mathematical_status__parmenidean_rejection', 'ZERO_MATHEMATICAL_STATUS__PARMENIDEAN_REJECTION', 289).
test_case('testsets/zero_mathematical_status__placeholder_reading.pl', 'zero_mathematical_status__placeholder_reading', 'ZERO_MATHEMATICAL_STATUS__PLACEHOLDER_READING', 290).
test_case('testsets/zionist_legitimacy_basis__national_liberation_reading.pl', 'zionist_legitimacy_basis__national_liberation_reading', 'ZIONIST_LEGITIMACY_BASIS__NATIONAL_LIBERATION_READING', 291).

% --- Test Suite Runner ---
run_dynamic_suite :-
    retractall(test_passed(_)),
    retractall(test_failed(_, _, _)),
    writeln('--- STARTING DYNAMIC VALIDATION ---'),
    forall(test_case(Path, ID, Label, N), run_single_test(Path, ID, Label, N)),
    count_and_report,
    % Call validate_all directly from data_validation module
    data_validation:validate_all.

% --- Single Test Executor ---
%  Per-test timeout guard (60s) prevents any single test from consuming
%  the entire sweep timeout. Elapsed timing aids diagnostic profiling.
run_single_test(Path, ID, _Label, N) :-
    format('~n[~w] EXECUTING: ~w~n', [N, Path]),
    get_time(T0),
    catch(
        call_with_time_limit(
            60,
            (   catch_with_backtrace(
                    ( load_and_run(Path, ID) ->
                        assertz(test_passed(Path)),
                        format('[PASS] ~w~n', [Path])
                    ;   assertz(test_failed(Path, audit_failed, 'load_and_run returned false')),
                        format('[AUDIT FAIL] ~w~n', [Path])
                    ),
                    E,
                    (   assertz(test_failed(Path, exception, E)),
                        format('[FAIL] Exception for ~w: ~w~n', [Path, E]),
                        print_prolog_backtrace(current_output, E)
                    )
                ),
                report_generator:generate_llm_feedback(ID)
            )
        ),
        time_limit_exceeded,
        (   assertz(test_failed(Path, timeout, 'Exceeded 60s per-test limit')),
            format('[TIMEOUT] ~w~n', [Path])
        )
    ),
    get_time(T1),
    Elapsed is T1 - T0,
    format('[ELAPSED] ~w: ~3fs~n', [Path, Elapsed]).

% --- Result Counter & Reporter ---
count_and_report :-
    findall(P, test_passed(P), Ps), length(Ps, PC),
    findall(F, test_failed(F,_,_), Fs), length(Fs, FC),
    writeln(''),
    writeln('=================================================='),
    writeln('           TEST SUITE SUMMARY'),
    writeln('=================================================='),
    format('Passed: ~w~n', [PC]),
    format('Failed: ~w~n', [FC]),
    (FC > 0 -> report_failures ; true),
    writeln('==================================================').

report_failures :-
    writeln('--- FAILED TESTS ---'),
    forall(test_failed(Path, Type, Detail),
           format('~n  - [~w] ~w~n    Reason: ~w~n', [Type, Path, Detail])).

