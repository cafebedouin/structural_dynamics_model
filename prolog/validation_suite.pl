:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/abolition_reading.pl', 'abolition_reading', 'ABOLITION_READING', 1).
test_case('testsets/abolitionist_rights.pl', 'abolitionist_rights', 'ABOLITIONIST_RIGHTS', 2).
test_case('testsets/absolutist_reading.pl', 'absolutist_reading', 'ABSOLUTIST_READING', 3).
test_case('testsets/adaptation_priority.pl', 'adaptation_priority', 'ADAPTATION_PRIORITY', 4).
test_case('testsets/adaptation_priority_reading.pl', 'adaptation_priority_reading', 'ADAPTATION_PRIORITY_READING', 5).
test_case('testsets/ai_risk_governance_priority_contradictions.pl', 'unknown_interval', 'AI_RISK_GOVERNANCE_PRIORITY_CONTRADICTIONS', 6).
test_case('testsets/antisubordination_reading.pl', 'antisubordination_reading', 'ANTISUBORDINATION_READING', 7).
test_case('testsets/autonomy_reading.pl', 'autonomy_reading', 'AUTONOMY_READING', 8).
test_case('testsets/balancing_reading.pl', 'measuring', 'BALANCING_READING', 9).
test_case('testsets/became_thinkable_reading.pl', 'became_thinkable_reading', 'BECAME_THINKABLE_READING', 10).
test_case('testsets/behavioral_competence_reading.pl', 'behavioral_competence_reading', 'BEHAVIORAL_COMPETENCE_READING', 11).
test_case('testsets/beneficiary_agency_reading.pl', '0', 'BENEFICIARY_AGENCY_READING', 12).
test_case('testsets/beneficiary_maintained_reading.pl', 'beneficiary_maintained_reading', 'BENEFICIARY_MAINTAINED_READING', 13).
test_case('testsets/beneficiary_maintenance_reading.pl', 'beneficiary_maintenance_reading', 'BENEFICIARY_MAINTENANCE_READING', 14).
test_case('testsets/biology_reading.pl', 'biology_reading', 'BIOLOGY_READING', 15).
test_case('testsets/birth_reading.pl', 'birth_reading', 'BIRTH_READING', 16).
test_case('testsets/birth_threshold_reading.pl', 'birth_threshold_reading', 'BIRTH_THRESHOLD_READING', 17).
test_case('testsets/bodily_autonomy_primary.pl', 'bodily_autonomy_primary', 'BODILY_AUTONOMY_PRIMARY', 18).
test_case('testsets/border_normative_status_contradictions.pl', 'unknown_interval', 'BORDER_NORMATIVE_STATUS_CONTRADICTIONS', 19).
test_case('testsets/bridge_reading.pl', 'bridge_reading', 'BRIDGE_READING', 20).
test_case('testsets/catastrophe_as_necessary.pl', 'catastrophe_as_necessary', 'CATASTROPHE_AS_NECESSARY', 21).
test_case('testsets/catastrophe_memory_transmission_contradictions.pl', 'unknown_interval', 'CATASTROPHE_MEMORY_TRANSMISSION_CONTRADICTIONS', 22).
test_case('testsets/catastrophic_tail_dominant.pl', 'catastrophic_tail_dominant', 'CATASTROPHIC_TAIL_DOMINANT', 23).
test_case('testsets/catastrophic_tail_reading.pl', 'catastrophic_tail_reading', 'CATASTROPHIC_TAIL_READING', 24).
test_case('testsets/categorical_abolition.pl', '40', 'CATEGORICAL_ABOLITION', 25).
test_case('testsets/civic_eugenic_reading.pl', 'civic_eugenic_reading', 'CIVIC_EUGENIC_READING', 26).
test_case('testsets/civic_right_reading.pl', '0', 'CIVIC_RIGHT_READING', 27).
test_case('testsets/climate_response_imperative_contradictions.pl', 'unknown_interval', 'CLIMATE_RESPONSE_IMPERATIVE_CONTRADICTIONS', 28).
test_case('testsets/co_constitution_reading.pl', '0', 'CO_CONSTITUTION_READING', 29).
test_case('testsets/codification_asymmetry.pl', '0', 'CODIFICATION_ASYMMETRY', 30).
test_case('testsets/collective_militia_reading.pl', 'collective_militia_reading', 'COLLECTIVE_MILITIA_READING', 31).
test_case('testsets/collective_right_reading.pl', 'collective_right_reading', 'COLLECTIVE_RIGHT_READING', 32).
test_case('testsets/colorblind_reading.pl', 'colorblind_reading', 'COLORBLIND_READING', 33).
test_case('testsets/commemorative_husk_reading.pl', 'commemorative_husk_reading', 'COMMEMORATIVE_HUSK_READING', 34).
test_case('testsets/communal_autonomy_reading.pl', 'communal_autonomy_reading', 'COMMUNAL_AUTONOMY_READING', 35).
test_case('testsets/competence_exercise_validity_contradictions.pl', 'unknown_interval', 'COMPETENCE_EXERCISE_VALIDITY_CONTRADICTIONS', 36).
test_case('testsets/competence_reading.pl', 'competence_reading', 'COMPETENCE_READING', 37).
test_case('testsets/composite_reading.pl', 'composite_reading', 'COMPOSITE_READING', 38).
test_case('testsets/conception_reading.pl', 'conception_reading', 'CONCEPTION_READING', 39).
test_case('testsets/conceptual_emergence_reading.pl', 'conceptual_emergence_reading', 'CONCEPTUAL_EMERGENCE_READING', 40).
test_case('testsets/constitutional_hybrid_reading.pl', 'constitutional_hybrid_reading', 'CONSTITUTIONAL_HYBRID_READING', 41).
test_case('testsets/constitutional_supremacy_reading.pl', '0', 'CONSTITUTIONAL_SUPREMACY_READING', 42).
test_case('testsets/consumer_adoption_friction.pl', 'consumer_adoption_friction', 'CONSUMER_ADOPTION_FRICTION', 43).
test_case('testsets/continuationist_reading.pl', 'continuationist_reading', 'CONTINUATIONIST_READING', 44).
test_case('testsets/continuity_reading.pl', '0', 'CONTINUITY_READING', 45).
test_case('testsets/continuous_refresh_hybrid.pl', 'continuous_refresh_hybrid', 'CONTINUOUS_REFRESH_HYBRID', 46).
test_case('testsets/contraction_reading.pl', 'contraction_reading', 'CONTRACTION_READING', 47).
test_case('testsets/convergence_synthesis.pl', 'convergence_synthesis', 'CONVERGENCE_SYNTHESIS', 48).
test_case('testsets/convertibility_constraint_removal.pl', 'convertibility_constraint_removal', 'CONVERTIBILITY_CONSTRAINT_REMOVAL', 49).
test_case('testsets/coordination_lock_in.pl', 'coordination_lock_in', 'COORDINATION_LOCK_IN', 50).
test_case('testsets/correctness_camouflage.pl', 'correctness_camouflage', 'CORRECTNESS_CAMOUFLAGE', 51).
test_case('testsets/credibility_paradox_reading.pl', 'credibility_paradox_reading', 'CREDIBILITY_PARADOX_READING', 52).
test_case('testsets/cyclopean_point_as_manufactured_center.pl', 'cyclopean_point_as_manufactured_center', 'CYCLOPEAN_POINT_AS_MANUFACTURED_CENTER', 53).
test_case('testsets/decentralization_sovereignty_commitment.pl', 'decentralization_sovereignty_commitment', 'DECENTRALIZATION_SOVEREIGNTY_COMMITMENT', 54).
test_case('testsets/degrowth_reading.pl', '0', 'DEGROWTH_READING', 55).
test_case('testsets/deterrence_instrument.pl', 'deterrence_instrument', 'DETERRENCE_INSTRUMENT', 56).
test_case('testsets/deterrence_reading.pl', 'deterrence_reading', 'DETERRENCE_READING', 57).
test_case('testsets/disparity_as_depth_signal.pl', 'disparity_as_depth_signal', 'DISPARITY_AS_DEPTH_SIGNAL', 58).
test_case('testsets/diversity_reading.pl', 'diversity_reading', 'DIVERSITY_READING', 59).
test_case('testsets/domain_partition_reading.pl', '0', 'DOMAIN_PARTITION_READING', 60).
test_case('testsets/drop_confessional_instantiation.pl', 'drop_confessional_instantiation', 'DROP_CONFESSIONAL_INSTANTIATION', 61).
test_case('testsets/dual_priority_reading.pl', 'dual_priority_reading', 'DUAL_PRIORITY_READING', 62).
test_case('testsets/ecumenical_boundary_shift.pl', 'ecumenical_boundary_shift', 'ECUMENICAL_BOUNDARY_SHIFT', 63).
test_case('testsets/end_of_life_decision_authority_contradictions.pl', 'unknown_interval', 'END_OF_LIFE_DECISION_AUTHORITY_CONTRADICTIONS', 64).
test_case('testsets/endogenous_climb_reading.pl', '0', 'ENDOGENOUS_CLIMB_READING', 65).
test_case('testsets/endogenous_reinterpretation_reading.pl', 'endogenous_reinterpretation_reading', 'ENDOGENOUS_REINTERPRETATION_READING', 66).
test_case('testsets/engineered_infrastructure_reading.pl', 'engineered_infrastructure_reading', 'ENGINEERED_INFRASTRUCTURE_READING', 67).
test_case('testsets/episcopal_collegiality_tension.pl', 'episcopal_collegiality_tension', 'EPISCOPAL_COLLEGIALITY_TENSION', 68).
test_case('testsets/equal_protection_clause_contradictions.pl', 'unknown_interval', 'EQUAL_PROTECTION_CLAUSE_CONTRADICTIONS', 69).
test_case('testsets/existential_matrix_reading.pl', '0', 'EXISTENTIAL_MATRIX_READING', 70).
test_case('testsets/existential_risk_reading.pl', 'existential_risk_reading', 'EXISTENTIAL_RISK_READING', 71).
test_case('testsets/exogenous_override_reading.pl', 'exogenous_override_reading', 'EXOGENOUS_OVERRIDE_READING', 72).
test_case('testsets/expected_value_dominant.pl', 'expected_value_dominant', 'EXPECTED_VALUE_DOMINANT', 73).
test_case('testsets/explanatory_closure_mechanism.pl', 'explanatory_closure_mechanism', 'EXPLANATORY_CLOSURE_MECHANISM', 74).
test_case('testsets/federation_membership_kernel_contradictions.pl', 'unknown_interval', 'FEDERATION_MEMBERSHIP_KERNEL_CONTRADICTIONS', 75).
test_case('testsets/first_held_reading.pl', 'first_held_reading', 'FIRST_HELD_READING', 76).
test_case('testsets/fork_diffusion_status_signal.pl', 'fork_diffusion_status_signal', 'FORK_DIFFUSION_STATUS_SIGNAL', 77).
test_case('testsets/frame_absorption_dynamics.pl', '0', 'FRAME_ABSORPTION_DYNAMICS', 78).
test_case('testsets/frame_mismatch_friction.pl', 'frame_mismatch_friction', 'FRAME_MISMATCH_FRICTION', 79).
test_case('testsets/freedom_floor_reading.pl', '0', 'FREEDOM_FLOOR_READING', 80).
test_case('testsets/freedom_of_movement_reading.pl', 'freedom_of_movement_reading', 'FREEDOM_OF_MOVEMENT_READING', 81).
test_case('testsets/freedom_primary.pl', 'freedom_primary', 'FREEDOM_PRIMARY', 82).
test_case('testsets/gender_identity_reading.pl', 'gender_identity_reading', 'GENDER_IDENTITY_READING', 83).
test_case('testsets/gender_justice_fragmentation.pl', 'gender_justice_fragmentation', 'GENDER_JUSTICE_FRAGMENTATION', 84).
test_case('testsets/hanafi_reading.pl', 'hanafi_reading', 'HANAFI_READING', 85).
test_case('testsets/hanbali_reading.pl', '0', 'HANBALI_READING', 86).
test_case('testsets/harm_limited_reading.pl', 'harm_limited_reading', 'HARM_LIMITED_READING', 87).
test_case('testsets/harm_reduction_reading.pl', 'harm_reduction_reading', 'HARM_REDUCTION_READING', 88).
test_case('testsets/harm_threshold_reading.pl', '0', 'HARM_THRESHOLD_READING', 89).
test_case('testsets/husk_reading.pl', 'husk_reading', 'HUSK_READING', 90).
test_case('testsets/hybrid_atrophy_reading.pl', 'hybrid_atrophy_reading', 'HYBRID_ATROPHY_READING', 91).
test_case('testsets/hybrid_pedagogical_reading.pl', 'hybrid_pedagogical_reading', 'HYBRID_PEDAGOGICAL_READING', 92).
test_case('testsets/hybrid_reading.pl', '0', 'HYBRID_READING', 93).
test_case('testsets/hybrid_scaffolding_reading.pl', 'hybrid_scaffolding_reading', 'HYBRID_SCAFFOLDING_READING', 94).
test_case('testsets/individual_right_reading.pl', 'pre', 'INDIVIDUAL_RIGHT_READING', 95).
test_case('testsets/inflation_credibility_constraint.pl', 'inflation_credibility_constraint', 'INFLATION_CREDIBILITY_CONSTRAINT', 96).
test_case('testsets/infrastructure_regulatory_lag.pl', 'infrastructure_regulatory_lag', 'INFRASTRUCTURE_REGULATORY_LAG', 97).
test_case('testsets/institutional_adoption_lag.pl', 'institutional_adoption_lag', 'INSTITUTIONAL_ADOPTION_LAG', 98).
test_case('testsets/institutional_pluralism_equilibrium.pl', 'institutional_pluralism_equilibrium', 'INSTITUTIONAL_PLURALISM_EQUILIBRIUM', 99).
test_case('testsets/institutional_pragmatism_reading.pl', '0', 'INSTITUTIONAL_PRAGMATISM_READING', 100).
test_case('testsets/institutional_reallocation_reading.pl', 'institutional_reallocation_reading', 'INSTITUTIONAL_REALLOCATION_READING', 101).
test_case('testsets/integration_primary.pl', 'integration_primary', 'INTEGRATION_PRIMARY', 102).
test_case('testsets/integration_reading.pl', 'integration_reading', 'INTEGRATION_READING', 103).
test_case('testsets/jurisprudential_method_kernel_contradictions.pl', 'unknown_interval', 'JURISPRUDENTIAL_METHOD_KERNEL_CONTRADICTIONS', 104).
test_case('testsets/kami_buddha_ontology_contradictions.pl', 'unknown_interval', 'KAMI_BUDDHA_ONTOLOGY_CONTRADICTIONS', 105).
test_case('testsets/kodashim_corpus_contradictions.pl', 'unknown_interval', 'KODASHIM_CORPUS_CONTRADICTIONS', 106).
test_case('testsets/latin_correctness_contradictions.pl', 'unknown_interval', 'LATIN_CORRECTNESS_CONTRADICTIONS', 107).
test_case('testsets/legalization_reading.pl', 'legalization_reading', 'LEGALIZATION_READING', 108).
test_case('testsets/legitimacy_of_imposed_practice_contradictions.pl', 'unknown_interval', 'LEGITIMACY_OF_IMPOSED_PRACTICE_CONTRADICTIONS', 109).
test_case('testsets/literacy_extraction_tradeoff.pl', 'literacy_extraction_tradeoff', 'LITERACY_EXTRACTION_TRADEOFF', 110).
test_case('testsets/literary_continuity_reading.pl', 'literary_continuity_reading', 'LITERARY_CONTINUITY_READING', 111).
test_case('testsets/literary_revival_reading.pl', 'literary_revival_reading', 'LITERARY_REVIVAL_READING', 112).
test_case('testsets/liturgical_continuity_reading.pl', 'liturgical_continuity_reading', 'LITURGICAL_CONTINUITY_READING', 113).
test_case('testsets/liturgical_preservation_reading.pl', 'liturgical_preservation_reading', 'LITURGICAL_PRESERVATION_READING', 114).
test_case('testsets/liturgical_vernacularization.pl', 'liturgical_vernacularization', 'LITURGICAL_VERNACULARIZATION', 115).
test_case('testsets/living_constitutionalist_reading.pl', 'living_constitutionalist_reading', 'LIVING_CONSTITUTIONALIST_READING', 116).
test_case('testsets/living_language_status_contradictions.pl', 'unknown_interval', 'LIVING_LANGUAGE_STATUS_CONTRADICTIONS', 117).
test_case('testsets/lock_in_reading.pl', 'lock_in_reading', 'LOCK_IN_READING', 118).
test_case('testsets/market_as_natural_default_contradictions.pl', 'unknown_interval', 'MARKET_AS_NATURAL_DEFAULT_CONTRADICTIONS', 119).
test_case('testsets/mitigation_priority_reading.pl', 'mitigation_priority_reading', 'MITIGATION_PRIORITY_READING', 120).
test_case('testsets/mixed_constitutional_reading.pl', 'mixed_constitutional_reading', 'MIXED_CONSTITUTIONAL_READING', 121).
test_case('testsets/modernization_defection_gradient.pl', 'modernization_defection_gradient', 'MODERNIZATION_DEFECTION_GRADIENT', 122).
test_case('testsets/monarchical_reading.pl', '0', 'MONARCHICAL_READING', 123).
test_case('testsets/monetary_aggregate_collapse.pl', 'monetary_aggregate_collapse', 'MONETARY_AGGREGATE_COLLAPSE', 124).
test_case('testsets/monetary_discretion_expansion.pl', 'monetary_discretion_expansion', 'MONETARY_DISCRETION_EXPANSION', 125).
test_case('testsets/monopoly_transfer_mechanism.pl', 'monopoly_transfer_mechanism', 'MONOPOLY_TRANSFER_MECHANISM', 126).
test_case('testsets/mourning_practice_reading.pl', 'mourning_practice_reading', 'MOURNING_PRACTICE_READING', 127).
test_case('testsets/muslim_uncodified_reading.pl', 'muslim_uncodified_reading', 'MUSLIM_UNCODIFIED_READING', 128).
test_case('testsets/native_generation.pl', '0', 'NATIVE_GENERATION', 129).
test_case('testsets/native_generation_reading.pl', 'native_generation_reading', 'NATIVE_GENERATION_READING', 130).
test_case('testsets/naturalization_reading.pl', 'naturalization_reading', 'NATURALIZATION_READING', 131).
test_case('testsets/near_miss_as_bridge.pl', 'near_miss_as_bridge', 'NEAR_MISS_AS_BRIDGE', 132).
test_case('testsets/near_term_harms_reading.pl', '0', 'NEAR_TERM_HARMS_READING', 133).
test_case('testsets/nuclear_impossibility_kernel_contradictions.pl', 'unknown_interval', 'NUCLEAR_IMPOSSIBILITY_KERNEL_CONTRADICTIONS', 134).
test_case('testsets/option_value_preserving.pl', 'option_value_preserving', 'OPTION_VALUE_PRESERVING', 135).
test_case('testsets/originalist_reading.pl', 'originalist_reading', 'ORIGINALIST_READING', 136).
test_case('testsets/overdetermined_composite_reading.pl', 'overdetermined_composite_reading', 'OVERDETERMINED_COMPOSITE_READING', 137).
test_case('testsets/parallel_adjudication_structure.pl', 'parallel_adjudication_structure', 'PARALLEL_ADJUDICATION_STRUCTURE', 138).
test_case('testsets/parmenidean_ontological_barrier.pl', 'parmenidean_ontological_barrier', 'PARMENIDEAN_ONTOLOGICAL_BARRIER', 139).
test_case('testsets/partition_reading.pl', 'partition_reading', 'PARTITION_READING', 140).
test_case('testsets/performance_only.pl', 'performance_only', 'PERFORMANCE_ONLY', 141).
test_case('testsets/personhood_boundary_contradictions.pl', 'unknown_interval', 'PERSONHOOD_BOUNDARY_CONTRADICTIONS', 142).
test_case('testsets/phonetic_script_mismatch.pl', 'phonetic_script_mismatch', 'PHONETIC_SCRIPT_MISMATCH', 143).
test_case('testsets/plural_marriage_mandate_contradictions.pl', 'unknown_interval', 'PLURAL_MARRIAGE_MANDATE_CONTRADICTIONS', 144).
test_case('testsets/political_swap_reading.pl', '0', 'POLITICAL_SWAP_READING', 145).
test_case('testsets/polycentric_petrification.pl', 'polycentric_petrification', 'POLYCENTRIC_PETRIFICATION', 146).
test_case('testsets/positional_notation_dependency.pl', 'positional_notation_dependency', 'POSITIONAL_NOTATION_DEPENDENCY', 147).
test_case('testsets/power_asymmetry_in_legibility.pl', 'power_asymmetry_in_legibility', 'POWER_ASYMMETRY_IN_LEGIBILITY', 148).
test_case('testsets/pragmatic_incoherence_reading.pl', 'pragmatic_incoherence_reading', 'PRAGMATIC_INCOHERENCE_READING', 149).
test_case('testsets/precautionary_reading.pl', '0', 'PRECAUTIONARY_READING', 150).
test_case('testsets/preparedness_retention_contradictions.pl', 'unknown_interval', 'PREPAREDNESS_RETENTION_CONTRADICTIONS', 151).
test_case('testsets/prohibition_reading.pl', 'prohibition_reading', 'PROHIBITION_READING', 152).
test_case('testsets/proportionality_reading.pl', 'proportionality_reading', 'PROPORTIONALITY_READING', 153).
test_case('testsets/public_health_primary.pl', 'public_health_primary', 'PUBLIC_HEALTH_PRIMARY', 154).
test_case('testsets/punctuated_swap_reading.pl', 'punctuated_swap_reading', 'PUNCTUATED_SWAP_READING', 155).
test_case('testsets/pure_property.pl', 'pure_property', 'PURE_PROPERTY', 156).
test_case('testsets/qualified_sovereignty.pl', 'qualified_sovereignty', 'QUALIFIED_SOVEREIGNTY', 157).
test_case('testsets/qwerty_persistence_mechanism_contradictions.pl', 'unknown_interval', 'QWERTY_PERSISTENCE_MECHANISM_CONTRADICTIONS', 158).
test_case('testsets/rational_dropout_reading.pl', 'rational_dropout_reading', 'RATIONAL_DROPOUT_READING', 159).
test_case('testsets/reachability_contraction_vs_probability_drop.pl', 'reachability_contraction_vs_probability_drop', 'REACHABILITY_CONTRACTION_VS_PROBABILITY_DROP', 160).
test_case('testsets/real_catastrophe_only.pl', 'real_catastrophe_only', 'REAL_CATASTROPHE_ONLY', 161).
test_case('testsets/reconstruction_reading.pl', 'reconstruction_reading', 'RECONSTRUCTION_READING', 162).
test_case('testsets/reformation_event_boundary_contradictions.pl', 'unknown_interval', 'REFORMATION_EVENT_BOUNDARY_CONTRADICTIONS', 163).
test_case('testsets/regulatory_recognition_reading.pl', 'corresponding', 'REGULATORY_RECOGNITION_READING', 164).
test_case('testsets/relational_autonomy.pl', 'relational_autonomy', 'RELATIONAL_AUTONOMY', 165).
test_case('testsets/remedial_reading.pl', 'remedial_reading', 'REMEDIAL_READING', 166).
test_case('testsets/republican_reading.pl', 'republican_reading', 'REPUBLICAN_READING', 167).
test_case('testsets/responsibility_misassignment.pl', 'responsibility_misassignment', 'RESPONSIBILITY_MISASSIGNMENT', 168).
test_case('testsets/retributive_desert.pl', 'retributive_desert', 'RETRIBUTIVE_DESERT', 169).
test_case('testsets/retributive_reading.pl', 'retributive_reading', 'RETRIBUTIVE_READING', 170).
test_case('testsets/revolutionary_legitimacy_scaffold.pl', 'revolutionary_legitimacy_scaffold', 'REVOLUTIONARY_LEGITIMACY_SCAFFOLD', 171).
test_case('testsets/rupture_reading.pl', 'rupture_reading', 'RUPTURE_READING', 172).
test_case('testsets/sanctity_reading.pl', 'sanctity_reading', 'SANCTITY_READING', 173).
test_case('testsets/sartorial_commitment_override.pl', 'sartorial_commitment_override', 'SARTORIAL_COMMITMENT_OVERRIDE', 174).
test_case('testsets/second_amendment_text_contradictions.pl', 'unknown_interval', 'SECOND_AMENDMENT_TEXT_CONTRADICTIONS', 175).
test_case('testsets/security_occupation_tradeoff.pl', 'security_occupation_tradeoff', 'SECURITY_OCCUPATION_TRADEOFF', 176).
test_case('testsets/self_determination_reading.pl', 'self_determination_reading', 'SELF_DETERMINATION_READING', 177).
test_case('testsets/sex_biology_reading.pl', 'from', 'SEX_BIOLOGY_READING', 178).
test_case('testsets/simulation_as_proxy.pl', 'simulation_as_proxy', 'SIMULATION_AS_PROXY', 179).
test_case('testsets/simulation_as_sufficient.pl', 'simulation_as_sufficient', 'SIMULATION_AS_SUFFICIENT', 180).
test_case('testsets/sound_money_scarcity_constraint.pl', 'sound_money_scarcity_constraint', 'SOUND_MONEY_SCARCITY_CONSTRAINT', 181).
test_case('testsets/sound_money_stability_paradox.pl', 'sound_money_stability_paradox', 'SOUND_MONEY_STABILITY_PARADOX', 182).
test_case('testsets/sovereign_legitimacy_contradictions.pl', 'unknown_interval', 'SOVEREIGN_LEGITIMACY_CONTRADICTIONS', 183).
test_case('testsets/sovereignty_primary.pl', '0', 'SOVEREIGNTY_PRIMARY', 184).
test_case('testsets/sovereignty_reading.pl', 'sovereignty_reading', 'SOVEREIGNTY_READING', 185).
test_case('testsets/spatial_access_conflict.pl', 'spatial_access_conflict', 'SPATIAL_ACCESS_CONFLICT', 186).
test_case('testsets/speculative_narrative_volatility.pl', '0', 'SPECULATIVE_NARRATIVE_VOLATILITY', 187).
test_case('testsets/speculative_price_volatility_trap.pl', 'speculative_price_volatility_trap', 'SPECULATIVE_PRICE_VOLATILITY_TRAP', 188).
test_case('testsets/speech_protection_boundary_contradictions.pl', 'unknown_interval', 'SPEECH_PROTECTION_BOUNDARY_CONTRADICTIONS', 189).
test_case('testsets/spontaneous_order_reading.pl', '0', 'SPONTANEOUS_ORDER_READING', 190).
test_case('testsets/standing_army_structural_threat.pl', 'standing_army_structural_threat', 'STANDING_ARMY_STRUCTURAL_THREAT', 191).
test_case('testsets/state_killing_authority_contradictions.pl', 'unknown_interval', 'STATE_KILLING_AUTHORITY_CONTRADICTIONS', 192).
test_case('testsets/state_modernization_extraction.pl', 'state_modernization_extraction', 'STATE_MODERNIZATION_EXTRACTION', 193).
test_case('testsets/state_role_time_collapse.pl', 'state_role_time_collapse', 'STATE_ROLE_TIME_COLLAPSE', 194).
test_case('testsets/statute_of_anne_ip_foundation_contradictions.pl', 'unknown_interval', 'STATUTE_OF_ANNE_IP_FOUNDATION_CONTRADICTIONS', 195).
test_case('testsets/structural_contraction_reading.pl', 'from', 'STRUCTURAL_CONTRACTION_READING', 196).
test_case('testsets/study_as_archiving.pl', 'study_as_archiving', 'STUDY_AS_ARCHIVING', 197).
test_case('testsets/study_as_exercise.pl', 'study_as_exercise', 'STUDY_AS_EXERCISE', 198).
test_case('testsets/study_as_occupation.pl', 'study_as_occupation', 'STUDY_AS_OCCUPATION', 199).
test_case('testsets/subsidy_capture_reading.pl', 'subsidy_capture_reading', 'SUBSIDY_CAPTURE_READING', 200).
test_case('testsets/substance_control_authority_contradictions.pl', 'unknown_interval', 'SUBSTANCE_CONTROL_AUTHORITY_CONTRADICTIONS', 201).
test_case('testsets/substitution_archive.pl', 'substitution_archive', 'SUBSTITUTION_ARCHIVE', 202).
test_case('testsets/supply_cap_scarcity.pl', 'supply_cap_scarcity', 'SUPPLY_CAP_SCARCITY', 203).
test_case('testsets/survival_competence_reading.pl', 'survival_competence_reading', 'SURVIVAL_COMPETENCE_READING', 204).
test_case('testsets/swap_henrician_substitution.pl', 'swap_henrician_substitution', 'SWAP_HENRICIAN_SUBSTITUTION', 205).
test_case('testsets/temporal_boundary_constraint.pl', 'temporal_boundary_constraint', 'TEMPORAL_BOUNDARY_CONSTRAINT', 206).
test_case('testsets/temporal_decay_pressure.pl', 'temporal_decay_pressure', 'TEMPORAL_DECAY_PRESSURE', 207).
test_case('testsets/temporal_kernel_drift.pl', '1994', 'TEMPORAL_KERNEL_DRIFT', 208).
test_case('testsets/territorial_sovereignty_legitimacy_contradictions.pl', 'unknown_interval', 'TERRITORIAL_SOVEREIGNTY_LEGITIMACY_CONTRADICTIONS', 209).
test_case('testsets/textualist_reading.pl', 'textualist_reading', 'TEXTUALIST_READING', 210).
test_case('testsets/theological_climb_reading.pl', 'theological_climb_reading', 'THEOLOGICAL_CLIMB_READING', 211).
test_case('testsets/transmission_as_conceptual_import.pl', 'time', 'TRANSMISSION_AS_CONCEPTUAL_IMPORT', 212).
test_case('testsets/transmission_bypass.pl', 'transmission_bypass', 'TRANSMISSION_BYPASS', 213).
test_case('testsets/unified_manifestation_reading.pl', 'unified_manifestation_reading', 'UNIFIED_MANIFESTATION_READING', 214).
test_case('testsets/universality_paradox_reading.pl', 'universality_paradox_reading', 'UNIVERSALITY_PARADOX_READING', 215).
test_case('testsets/us_constitution_text_contradictions.pl', 'unknown_interval', 'US_CONSTITUTION_TEXT_CONTRADICTIONS', 216).
test_case('testsets/vaccine_mandate_balance_contradictions.pl', 'unknown_interval', 'VACCINE_MANDATE_BALANCE_CONTRADICTIONS', 217).
test_case('testsets/vatican_ii_magisterial_authority_contradictions.pl', 'unknown_interval', 'VATICAN_II_MAGISTERIAL_AUTHORITY_CONTRADICTIONS', 218).
test_case('testsets/viability_reading.pl', 'viability_reading', 'VIABILITY_READING', 219).
test_case('testsets/vulnerability_protection_reading.pl', 'vulnerability_protection_reading', 'VULNERABILITY_PROTECTION_READING', 220).
test_case('testsets/welfare_reading.pl', 'welfare_reading', 'WELFARE_READING', 221).
test_case('testsets/welfare_regulated_use.pl', 'welfare_regulated_use', 'WELFARE_REGULATED_USE', 222).
test_case('testsets/woman_female_category_contradictions.pl', 'unknown_interval', 'WOMAN_FEMALE_CATEGORY_CONTRADICTIONS', 223).

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

