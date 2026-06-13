:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/abrahamic_covenant__isaac_covenant_reading.pl', 'abrahamic_covenant__isaac_covenant_reading', 'ABRAHAMIC_COVENANT__ISAAC_COVENANT_READING', 1).
test_case('testsets/abrahamic_covenant__ishmael_covenant_reading.pl', 'abrahamic_covenant__ishmael_covenant_reading', 'ABRAHAMIC_COVENANT__ISHMAEL_COVENANT_READING', 2).
test_case('testsets/abrahamic_covenant__land_promise_constraint.pl', 'abrahamic_covenant__land_promise_constraint', 'ABRAHAMIC_COVENANT__LAND_PROMISE_CONSTRAINT', 3).
test_case('testsets/acceptable_risk_for_energy__catastrophic_tail_dominant.pl', 'acceptable_risk_for_energy__catastrophic_tail_dominant', 'ACCEPTABLE_RISK_FOR_ENERGY__CATASTROPHIC_TAIL_DOMINANT', 4).
test_case('testsets/acceptable_risk_for_energy__comparative_risk_dominant.pl', 'acceptable_risk_for_energy__comparative_risk_dominant', 'ACCEPTABLE_RISK_FOR_ENERGY__COMPARATIVE_RISK_DOMINANT', 5).
test_case('testsets/acceptable_risk_for_energy__expected_value_dominant.pl', 'acceptable_risk_for_energy__expected_value_dominant', 'ACCEPTABLE_RISK_FOR_ENERGY__EXPECTED_VALUE_DOMINANT', 6).
test_case('testsets/adjunctification_of_university_teaching_c0.pl', 'adjunctification_of_university_teaching_c0', 'ADJUNCTIFICATION_OF_UNIVERSITY_TEACHING_C0', 7).
test_case('testsets/ai_risk_governance_priority__bridge_reading.pl', 'ai_risk_governance_priority__bridge_reading', 'AI_RISK_GOVERNANCE_PRIORITY__BRIDGE_READING', 8).
test_case('testsets/ai_risk_governance_priority__existential_risk_reading.pl', 'ai_risk_governance_priority__existential_risk_reading', 'AI_RISK_GOVERNANCE_PRIORITY__EXISTENTIAL_RISK_READING', 9).
test_case('testsets/ai_risk_governance_priority__near_term_harms_reading.pl', 'ai_risk_governance_priority__near_term_harms_reading', 'AI_RISK_GOVERNANCE_PRIORITY__NEAR_TERM_HARMS_READING', 10).
test_case('testsets/all_men_created_equal__originalist_reading.pl', 'all_men_created_equal__originalist_reading', 'ALL_MEN_CREATED_EQUAL__ORIGINALIST_READING', 11).
test_case('testsets/all_men_created_equal__textualist_paradox_reading.pl', 'all_men_created_equal__textualist_paradox_reading', 'ALL_MEN_CREATED_EQUAL__TEXTUALIST_PARADOX_READING', 12).
test_case('testsets/all_men_created_equal__universalist_reading.pl', 'all_men_created_equal__universalist_reading', 'ALL_MEN_CREATED_EQUAL__UNIVERSALIST_READING', 13).
test_case('testsets/article_17_complementarity__international_oversight_reading.pl', 'article_17_complementarity__international_oversight_reading', 'ARTICLE_17_COMPLEMENTARITY__INTERNATIONAL_OVERSIGHT_READING', 14).
test_case('testsets/article_17_complementarity__national_primacy_reading.pl', 'article_17_complementarity__national_primacy_reading', 'ARTICLE_17_COMPLEMENTARITY__NATIONAL_PRIMACY_READING', 15).
test_case('testsets/article_9_war_renunciation__collective_self_defense_reading.pl', 'article_9_war_renunciation__collective_self_defense_reading', 'ARTICLE_9_WAR_RENUNCIATION__COLLECTIVE_SELF_DEFENSE_READING', 16).
test_case('testsets/article_9_war_renunciation__inherent_right_reading.pl', 'article_9_war_renunciation__inherent_right_reading', 'ARTICLE_9_WAR_RENUNCIATION__INHERENT_RIGHT_READING', 17).
test_case('testsets/article_9_war_renunciation__strict_pacifist_reading.pl', 'article_9_war_renunciation__strict_pacifist_reading', 'ARTICLE_9_WAR_RENUNCIATION__STRICT_PACIFIST_READING', 18).
test_case('testsets/biblical_authority__conciliar_reading.pl', 'biblical_authority__conciliar_reading', 'BIBLICAL_AUTHORITY__CONCILIAR_READING', 19).
test_case('testsets/biblical_authority__sola_scriptura_reading.pl', 'biblical_authority__sola_scriptura_reading', 'BIBLICAL_AUTHORITY__SOLA_SCRIPTURA_READING', 20).
test_case('testsets/biblical_authority__tradition_scripture_reading.pl', 'biblical_authority__tradition_scripture_reading', 'BIBLICAL_AUTHORITY__TRADITION_SCRIPTURE_READING', 21).
test_case('testsets/bitcoin_whitepaper_purpose__electronic_cash_reading.pl', 'bitcoin_whitepaper_purpose__electronic_cash_reading', 'BITCOIN_WHITEPAPER_PURPOSE__ELECTRONIC_CASH_READING', 22).
test_case('testsets/bitcoin_whitepaper_purpose__nakamoto_oracle_opacity.pl', 'bitcoin_whitepaper_purpose__nakamoto_oracle_opacity', 'BITCOIN_WHITEPAPER_PURPOSE__NAKAMOTO_ORACLE_OPACITY', 23).
test_case('testsets/bitcoin_whitepaper_purpose__store_of_value_reading.pl', 'bitcoin_whitepaper_purpose__store_of_value_reading', 'BITCOIN_WHITEPAPER_PURPOSE__STORE_OF_VALUE_READING', 24).
test_case('testsets/catastrophe_memory_transmission__hybrid_embedded_reading.pl', 'catastrophe_memory_transmission__hybrid_embedded_reading', 'CATASTROPHE_MEMORY_TRANSMISSION__HYBRID_EMBEDDED_READING', 25).
test_case('testsets/catastrophe_memory_transmission__operational_competence_reading.pl', 'catastrophe_memory_transmission__operational_competence_reading', 'CATASTROPHE_MEMORY_TRANSMISSION__OPERATIONAL_COMPETENCE_READING', 26).
test_case('testsets/catastrophe_memory_transmission__symbol_continuity_reading.pl', 'catastrophe_memory_transmission__symbol_continuity_reading', 'CATASTROPHE_MEMORY_TRANSMISSION__SYMBOL_CONTINUITY_READING', 27).
test_case('testsets/climate_mitigation_imperative__opportunity_cost_reading.pl', 'climate_mitigation_imperative__opportunity_cost_reading', 'CLIMATE_MITIGATION_IMPERATIVE__OPPORTUNITY_COST_READING', 28).
test_case('testsets/climate_mitigation_imperative__portfolio_optimization_reading.pl', 'climate_mitigation_imperative__portfolio_optimization_reading', 'CLIMATE_MITIGATION_IMPERATIVE__PORTFOLIO_OPTIMIZATION_READING', 29).
test_case('testsets/climate_mitigation_imperative__systems_transition_reading.pl', 'climate_mitigation_imperative__systems_transition_reading', 'CLIMATE_MITIGATION_IMPERATIVE__SYSTEMS_TRANSITION_READING', 30).
test_case('testsets/competence_exercise_requirement__catastrophe_as_necessary_anchor.pl', 'competence_exercise_requirement__catastrophe_as_necessary_anchor', 'COMPETENCE_EXERCISE_REQUIREMENT__CATASTROPHE_AS_NECESSARY_ANCHOR', 31).
test_case('testsets/competence_exercise_requirement__hybrid_dependency.pl', 'competence_exercise_requirement__hybrid_dependency', 'COMPETENCE_EXERCISE_REQUIREMENT__HYBRID_DEPENDENCY', 32).
test_case('testsets/competence_exercise_requirement__simulation_as_adequate_exercise.pl', 'competence_exercise_requirement__simulation_as_adequate_exercise', 'COMPETENCE_EXERCISE_REQUIREMENT__SIMULATION_AS_ADEQUATE_EXERCISE', 33).
test_case('testsets/competence_exercise_validity__continuous_refresh_hybrid.pl', 'competence_exercise_validity__continuous_refresh_hybrid', 'COMPETENCE_EXERCISE_VALIDITY__CONTINUOUS_REFRESH_HYBRID', 34).
test_case('testsets/competence_exercise_validity__real_catastrophe_only.pl', 'competence_exercise_validity__real_catastrophe_only', 'COMPETENCE_EXERCISE_VALIDITY__REAL_CATASTROPHE_ONLY', 35).
test_case('testsets/competence_exercise_validity__simulation_as_proxy.pl', 'competence_exercise_validity__simulation_as_proxy', 'COMPETENCE_EXERCISE_VALIDITY__SIMULATION_AS_PROXY', 36).
test_case('testsets/constitutional_text_authority__living_constitutionalist_reading.pl', 'constitutional_text_authority__living_constitutionalist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__LIVING_CONSTITUTIONALIST_READING', 37).
test_case('testsets/constitutional_text_authority__originalist_reading.pl', 'constitutional_text_authority__originalist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__ORIGINALIST_READING', 38).
test_case('testsets/constitutional_text_authority__positivist_reading.pl', 'constitutional_text_authority__positivist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__POSITIVIST_READING', 39).
test_case('testsets/correct_latin__continuity_reading.pl', 'correct_latin__continuity_reading', 'CORRECT_LATIN__CONTINUITY_READING', 40).
test_case('testsets/correct_latin__discontinuity_reading.pl', 'correct_latin__discontinuity_reading', 'CORRECT_LATIN__DISCONTINUITY_READING', 41).
test_case('testsets/correct_latin__hybrid_reading.pl', 'correct_latin__hybrid_reading', 'CORRECT_LATIN__HYBRID_READING', 42).
test_case('testsets/correct_latin_kernel__continuity_reading.pl', 'correct_latin_kernel__continuity_reading', 'CORRECT_LATIN_KERNEL__CONTINUITY_READING', 43).
test_case('testsets/correct_latin_kernel__discontinuity_reading.pl', 'correct_latin_kernel__discontinuity_reading', 'CORRECT_LATIN_KERNEL__DISCONTINUITY_READING', 44).
test_case('testsets/correct_latin_kernel__hybrid_reading.pl', 'correct_latin_kernel__hybrid_reading', 'CORRECT_LATIN_KERNEL__HYBRID_READING', 45).
test_case('testsets/demographic_skill_mismatch_c0.pl', 'demographic_skill_mismatch_c0', 'DEMOGRAPHIC_SKILL_MISMATCH_C0', 46).
test_case('testsets/digital_money_origin__became_thinkable_reading.pl', 'digital_money_origin__became_thinkable_reading', 'DIGITAL_MONEY_ORIGIN__BECAME_THINKABLE_READING', 47).
test_case('testsets/digital_money_origin__first_held_reading.pl', 'digital_money_origin__first_held_reading', 'DIGITAL_MONEY_ORIGIN__FIRST_HELD_READING', 48).
test_case('testsets/digital_money_origin__regulatory_recognition_reading.pl', 'digital_money_origin__regulatory_recognition_reading', 'DIGITAL_MONEY_ORIGIN__REGULATORY_RECOGNITION_READING', 49).
test_case('testsets/divine_legitimacy_substrate__amun_polytheistic_reading.pl', 'divine_legitimacy_substrate__amun_polytheistic_reading', 'DIVINE_LEGITIMACY_SUBSTRATE__AMUN_POLYTHEISTIC_READING', 50).
test_case('testsets/divine_legitimacy_substrate__atenist_monotheistic_reading.pl', 'divine_legitimacy_substrate__atenist_monotheistic_reading', 'DIVINE_LEGITIMACY_SUBSTRATE__ATENIST_MONOTHEISTIC_READING', 51).
test_case('testsets/divine_legitimacy_substrate__folk_syncretistic_reading.pl', 'divine_legitimacy_substrate__folk_syncretistic_reading', 'DIVINE_LEGITIMACY_SUBSTRATE__FOLK_SYNCRETISTIC_READING', 52).
test_case('testsets/dollar_gold_convertibility__policy_flexible_reading.pl', 'dollar_gold_convertibility__policy_flexible_reading', 'DOLLAR_GOLD_CONVERTIBILITY__POLICY_FLEXIBLE_READING', 53).
test_case('testsets/dollar_gold_convertibility__strict_convertibility_reading.pl', 'dollar_gold_convertibility__strict_convertibility_reading', 'DOLLAR_GOLD_CONVERTIBILITY__STRICT_CONVERTIBILITY_READING', 54).
test_case('testsets/dollar_gold_convertibility__triffin_structural_reading.pl', 'dollar_gold_convertibility__triffin_structural_reading', 'DOLLAR_GOLD_CONVERTIBILITY__TRIFFIN_STRUCTURAL_READING', 55).
test_case('testsets/dual_class_legitimacy__disclosure_consent.pl', 'dual_class_legitimacy__disclosure_consent', 'DUAL_CLASS_LEGITIMACY__DISCLOSURE_CONSENT', 56).
test_case('testsets/dual_class_legitimacy__founder_stewardship.pl', 'dual_class_legitimacy__founder_stewardship', 'DUAL_CLASS_LEGITIMACY__FOUNDER_STEWARDSHIP', 57).
test_case('testsets/dual_class_legitimacy__minority_extraction.pl', 'dual_class_legitimacy__minority_extraction', 'DUAL_CLASS_LEGITIMACY__MINORITY_EXTRACTION', 58).
test_case('testsets/end_of_life_authority__autonomy_reading.pl', 'end_of_life_authority__autonomy_reading', 'END_OF_LIFE_AUTHORITY__AUTONOMY_READING', 59).
test_case('testsets/end_of_life_authority__sanctity_reading.pl', 'end_of_life_authority__sanctity_reading', 'END_OF_LIFE_AUTHORITY__SANCTITY_READING', 60).
test_case('testsets/end_of_life_authority__slippery_slope_mechanism.pl', 'end_of_life_authority__slippery_slope_mechanism', 'END_OF_LIFE_AUTHORITY__SLIPPERY_SLOPE_MECHANISM', 61).
test_case('testsets/family_law_authority__christian_canonical_reading.pl', 'family_law_authority__christian_canonical_reading', 'FAMILY_LAW_AUTHORITY__CHRISTIAN_CANONICAL_READING', 62).
test_case('testsets/family_law_authority__hindu_dharmashastra_reading.pl', 'family_law_authority__hindu_dharmashastra_reading', 'FAMILY_LAW_AUTHORITY__HINDU_DHARMASHASTRA_READING', 63).
test_case('testsets/family_law_authority__muslim_shariat_reading.pl', 'family_law_authority__muslim_shariat_reading', 'FAMILY_LAW_AUTHORITY__MUSLIM_SHARIAT_READING', 64).
test_case('testsets/family_law_authority__parsi_zoroastrian_reading.pl', 'family_law_authority__parsi_zoroastrian_reading', 'FAMILY_LAW_AUTHORITY__PARSI_ZOROASTRIAN_READING', 65).
test_case('testsets/family_law_authority__secular_contractual_reading.pl', 'family_law_authority__secular_contractual_reading', 'FAMILY_LAW_AUTHORITY__SECULAR_CONTRACTUAL_READING', 66).
test_case('testsets/federation_membership__integration_reading.pl', 'federation_membership__integration_reading', 'FEDERATION_MEMBERSHIP__INTEGRATION_READING', 67).
test_case('testsets/federation_membership__sovereignty_reading.pl', 'federation_membership__sovereignty_reading', 'FEDERATION_MEMBERSHIP__SOVEREIGNTY_READING', 68).
test_case('testsets/federation_membership_obligations__integration_primary.pl', 'federation_membership_obligations__integration_primary', 'FEDERATION_MEMBERSHIP_OBLIGATIONS__INTEGRATION_PRIMARY', 69).
test_case('testsets/federation_membership_obligations__member_sovereignty_primary.pl', 'federation_membership_obligations__member_sovereignty_primary', 'FEDERATION_MEMBERSHIP_OBLIGATIONS__MEMBER_SOVEREIGNTY_PRIMARY', 70).
test_case('testsets/federation_membership_obligations__selective_solidarity.pl', 'federation_membership_obligations__selective_solidarity', 'FEDERATION_MEMBERSHIP_OBLIGATIONS__SELECTIVE_SOLIDARITY', 71).
test_case('testsets/feud_obligation_kernel__christianized_pacification_reading.pl', 'feud_obligation_kernel__christianized_pacification_reading', 'FEUD_OBLIGATION_KERNEL__CHRISTIANIZED_PACIFICATION_READING', 72).
test_case('testsets/feud_obligation_kernel__extraction_cycle_reading.pl', 'feud_obligation_kernel__extraction_cycle_reading', 'FEUD_OBLIGATION_KERNEL__EXTRACTION_CYCLE_READING', 73).
test_case('testsets/feud_obligation_kernel__stateless_coordination_reading.pl', 'feud_obligation_kernel__stateless_coordination_reading', 'FEUD_OBLIGATION_KERNEL__STATELESS_COORDINATION_READING', 74).
test_case('testsets/fifth_republic_constitution__cohabitation_equilibrium_reading.pl', 'fifth_republic_constitution__cohabitation_equilibrium_reading', 'FIFTH_REPUBLIC_CONSTITUTION__COHABITATION_EQUILIBRIUM_READING', 75).
test_case('testsets/fifth_republic_constitution__hyper_presidential_reading.pl', 'fifth_republic_constitution__hyper_presidential_reading', 'FIFTH_REPUBLIC_CONSTITUTION__HYPER_PRESIDENTIAL_READING', 76).
test_case('testsets/fifth_republic_constitution__parliamentary_constraint_reading.pl', 'fifth_republic_constitution__parliamentary_constraint_reading', 'FIFTH_REPUBLIC_CONSTITUTION__PARLIAMENTARY_CONSTRAINT_READING', 77).
test_case('testsets/gold_fiat_transition_mechanism__automatic_constraint_reading.pl', 'gold_fiat_transition_mechanism__automatic_constraint_reading', 'GOLD_FIAT_TRANSITION_MECHANISM__AUTOMATIC_CONSTRAINT_READING', 78).
test_case('testsets/gold_fiat_transition_mechanism__composite_overdetermination_reading.pl', 'gold_fiat_transition_mechanism__composite_overdetermination_reading', 'GOLD_FIAT_TRANSITION_MECHANISM__COMPOSITE_OVERDETERMINATION_READING', 79).
test_case('testsets/gold_fiat_transition_mechanism__creditor_discipline_reading.pl', 'gold_fiat_transition_mechanism__creditor_discipline_reading', 'GOLD_FIAT_TRANSITION_MECHANISM__CREDITOR_DISCIPLINE_READING', 80).
test_case('testsets/gpl_copyleft_scope__enforcement_vacuum_reading.pl', 'gpl_copyleft_scope__enforcement_vacuum_reading', 'GPL_COPYLEFT_SCOPE__ENFORCEMENT_VACUUM_READING', 81).
test_case('testsets/gpl_copyleft_scope__narrow_scope_reading.pl', 'gpl_copyleft_scope__narrow_scope_reading', 'GPL_COPYLEFT_SCOPE__NARROW_SCOPE_READING', 82).
test_case('testsets/gpl_copyleft_scope__strong_copyleft_reading.pl', 'gpl_copyleft_scope__strong_copyleft_reading', 'GPL_COPYLEFT_SCOPE__STRONG_COPYLEFT_READING', 83).
test_case('testsets/human_dignity_ai_governance__magisterial_integralist_reading.pl', 'human_dignity_ai_governance__magisterial_integralist_reading', 'HUMAN_DIGNITY_AI_GOVERNANCE__MAGISTERIAL_INTEGRALIST_READING', 84).
test_case('testsets/human_dignity_ai_governance__pluralist_pragmatic_reading.pl', 'human_dignity_ai_governance__pluralist_pragmatic_reading', 'HUMAN_DIGNITY_AI_GOVERNANCE__PLURALIST_PRAGMATIC_READING', 85).
test_case('testsets/human_dignity_ai_governance__secular_humanist_reading.pl', 'human_dignity_ai_governance__secular_humanist_reading', 'HUMAN_DIGNITY_AI_GOVERNANCE__SECULAR_HUMANIST_READING', 86).
test_case('testsets/human_dignity_ai_governance__techno_optimist_reading.pl', 'human_dignity_ai_governance__techno_optimist_reading', 'HUMAN_DIGNITY_AI_GOVERNANCE__TECHNO_OPTIMIST_READING', 87).
test_case('testsets/human_transcendence_pathway__babel_reading.pl', 'human_transcendence_pathway__babel_reading', 'HUMAN_TRANSCENDENCE_PATHWAY__BABEL_READING', 88).
test_case('testsets/human_transcendence_pathway__jerusalem_reading.pl', 'human_transcendence_pathway__jerusalem_reading', 'HUMAN_TRANSCENDENCE_PATHWAY__JERUSALEM_READING', 89).
test_case('testsets/human_transcendence_pathway__technocratic_vs_incarnational_reading.pl', 'human_transcendence_pathway__technocratic_vs_incarnational_reading', 'HUMAN_TRANSCENDENCE_PATHWAY__TECHNOCRATIC_VS_INCARNATIONAL_READING', 90).
test_case('testsets/institutional_trust_erosion_c0.pl', 'institutional_trust_erosion_c0', 'INSTITUTIONAL_TRUST_EROSION_C0', 91).
test_case('testsets/jewish_territorial_claim__cultural_zionism_reading.pl', 'jewish_territorial_claim__cultural_zionism_reading', 'JEWISH_TERRITORIAL_CLAIM__CULTURAL_ZIONISM_READING', 92).
test_case('testsets/jewish_territorial_claim__political_zionism_reading.pl', 'jewish_territorial_claim__political_zionism_reading', 'JEWISH_TERRITORIAL_CLAIM__POLITICAL_ZIONISM_READING', 93).
test_case('testsets/jewish_territorial_claim__revisionist_zionism_reading.pl', 'jewish_territorial_claim__revisionist_zionism_reading', 'JEWISH_TERRITORIAL_CLAIM__REVISIONIST_ZIONISM_READING', 94).
test_case('testsets/jurisprudential_method_kernel__hanafi_reading.pl', 'jurisprudential_method_kernel__hanafi_reading', 'JURISPRUDENTIAL_METHOD_KERNEL__HANAFI_READING', 95).
test_case('testsets/jurisprudential_method_kernel__hanbali_reading.pl', 'jurisprudential_method_kernel__hanbali_reading', 'JURISPRUDENTIAL_METHOD_KERNEL__HANBALI_READING', 96).
test_case('testsets/jurisprudential_method_kernel__maliki_reading.pl', 'jurisprudential_method_kernel__maliki_reading', 'JURISPRUDENTIAL_METHOD_KERNEL__MALIKI_READING', 97).
test_case('testsets/jurisprudential_method_kernel__shafii_reading.pl', 'jurisprudential_method_kernel__shafii_reading', 'JURISPRUDENTIAL_METHOD_KERNEL__SHAFII_READING', 98).
test_case('testsets/kjv_text_1611__exclusive_inspiration_reading.pl', 'kjv_text_1611__exclusive_inspiration_reading', 'KJV_TEXT_1611__EXCLUSIVE_INSPIRATION_READING', 99).
test_case('testsets/kjv_text_1611__functional_equivalence_reading.pl', 'kjv_text_1611__functional_equivalence_reading', 'KJV_TEXT_1611__FUNCTIONAL_EQUIVALENCE_READING', 100).
test_case('testsets/kjv_text_1611__revisable_translation_reading.pl', 'kjv_text_1611__revisable_translation_reading', 'KJV_TEXT_1611__REVISABLE_TRANSLATION_READING', 101).
test_case('testsets/kodashim_obligation__study_as_archive.pl', 'kodashim_obligation__study_as_archive', 'KODASHIM_OBLIGATION__STUDY_AS_ARCHIVE', 102).
test_case('testsets/kodashim_obligation__study_as_performance.pl', 'kodashim_obligation__study_as_performance', 'KODASHIM_OBLIGATION__STUDY_AS_PERFORMANCE', 103).
test_case('testsets/kodashim_obligation__study_as_preparation.pl', 'kodashim_obligation__study_as_preparation', 'KODASHIM_OBLIGATION__STUDY_AS_PREPARATION', 104).
test_case('testsets/literacy_acquisition_kernel__balanced_literacy_reading.pl', 'literacy_acquisition_kernel__balanced_literacy_reading', 'LITERACY_ACQUISITION_KERNEL__BALANCED_LITERACY_READING', 105).
test_case('testsets/literacy_acquisition_kernel__phonics_reading.pl', 'literacy_acquisition_kernel__phonics_reading', 'LITERACY_ACQUISITION_KERNEL__PHONICS_READING', 106).
test_case('testsets/literacy_acquisition_kernel__structured_literacy_reading.pl', 'literacy_acquisition_kernel__structured_literacy_reading', 'LITERACY_ACQUISITION_KERNEL__STRUCTURED_LITERACY_READING', 107).
test_case('testsets/literacy_acquisition_kernel__whole_language_reading.pl', 'literacy_acquisition_kernel__whole_language_reading', 'LITERACY_ACQUISITION_KERNEL__WHOLE_LANGUAGE_READING', 108).
test_case('testsets/market_as_natural_default__beneficiary_maintained_reading.pl', 'market_as_natural_default__beneficiary_maintained_reading', 'MARKET_AS_NATURAL_DEFAULT__BENEFICIARY_MAINTAINED_READING', 109).
test_case('testsets/market_as_natural_default__hybrid_amnesia_reading.pl', 'market_as_natural_default__hybrid_amnesia_reading', 'MARKET_AS_NATURAL_DEFAULT__HYBRID_AMNESIA_READING', 110).
test_case('testsets/market_as_natural_default__lapsed_alternative_reading.pl', 'market_as_natural_default__lapsed_alternative_reading', 'MARKET_AS_NATURAL_DEFAULT__LAPSED_ALTERNATIVE_READING', 111).
test_case('testsets/market_naturalization__beneficiary_maintained_reading.pl', 'market_naturalization__beneficiary_maintained_reading', 'MARKET_NATURALIZATION__BENEFICIARY_MAINTAINED_READING', 112).
test_case('testsets/market_naturalization__hybrid_reading.pl', 'market_naturalization__hybrid_reading', 'MARKET_NATURALIZATION__HYBRID_READING', 113).
test_case('testsets/market_naturalization__lapsed_alternative_reading.pl', 'market_naturalization__lapsed_alternative_reading', 'MARKET_NATURALIZATION__LAPSED_ALTERNATIVE_READING', 114).
test_case('testsets/marriage_commitment_legitimacy__endogenous_reinterpretation_reading.pl', 'marriage_commitment_legitimacy__endogenous_reinterpretation_reading', 'MARRIAGE_COMMITMENT_LEGITIMACY__ENDOGENOUS_REINTERPRETATION_READING', 115).
test_case('testsets/marriage_commitment_legitimacy__exogenous_override_reading.pl', 'marriage_commitment_legitimacy__exogenous_override_reading', 'MARRIAGE_COMMITMENT_LEGITIMACY__EXOGENOUS_OVERRIDE_READING', 116).
test_case('testsets/marriage_commitment_legitimacy__hybrid_pragmatic_reading.pl', 'marriage_commitment_legitimacy__hybrid_pragmatic_reading', 'MARRIAGE_COMMITMENT_LEGITIMACY__HYBRID_PRAGMATIC_READING', 117).
test_case('testsets/monetary_anchor_principle__overdetermined_composite_reading.pl', 'monetary_anchor_principle__overdetermined_composite_reading', 'MONETARY_ANCHOR_PRINCIPLE__OVERDETERMINED_COMPOSITE_READING', 118).
test_case('testsets/monetary_anchor_principle__punctuated_swap_reading.pl', 'monetary_anchor_principle__punctuated_swap_reading', 'MONETARY_ANCHOR_PRINCIPLE__PUNCTUATED_SWAP_READING', 119).
test_case('testsets/monetary_anchor_principle__triffin_inevitability_reading.pl', 'monetary_anchor_principle__triffin_inevitability_reading', 'MONETARY_ANCHOR_PRINCIPLE__TRIFFIN_INEVITABILITY_READING', 120).
test_case('testsets/montevideo_statehood_criteria__constitutive_reading.pl', 'montevideo_statehood_criteria__constitutive_reading', 'MONTEVIDEO_STATEHOOD_CRITERIA__CONSTITUTIVE_READING', 121).
test_case('testsets/montevideo_statehood_criteria__declaratory_reading.pl', 'montevideo_statehood_criteria__declaratory_reading', 'MONTEVIDEO_STATEHOOD_CRITERIA__DECLARATORY_READING', 122).
test_case('testsets/montevideo_statehood_criteria__hybrid_reading.pl', 'montevideo_statehood_criteria__hybrid_reading', 'MONTEVIDEO_STATEHOOD_CRITERIA__HYBRID_READING', 123).
test_case('testsets/nafta_jurisdictional_boundary__capital_supremacy_reading.pl', 'nafta_jurisdictional_boundary__capital_supremacy_reading', 'NAFTA_JURISDICTIONAL_BOUNDARY__CAPITAL_SUPREMACY_READING', 124).
test_case('testsets/nafta_jurisdictional_boundary__embedded_liberalism_reading.pl', 'nafta_jurisdictional_boundary__embedded_liberalism_reading', 'NAFTA_JURISDICTIONAL_BOUNDARY__EMBEDDED_LIBERALISM_READING', 125).
test_case('testsets/nafta_jurisdictional_boundary__sovereignty_primacy_reading.pl', 'nafta_jurisdictional_boundary__sovereignty_primacy_reading', 'NAFTA_JURISDICTIONAL_BOUNDARY__SOVEREIGNTY_PRIMACY_READING', 126).
test_case('testsets/naskh_principle__classical_abrogation.pl', 'naskh_principle__classical_abrogation', 'NASKH_PRINCIPLE__CLASSICAL_ABROGATION', 127).
test_case('testsets/naskh_principle__contextual_harmonization.pl', 'naskh_principle__contextual_harmonization', 'NASKH_PRINCIPLE__CONTEXTUAL_HARMONIZATION', 128).
test_case('testsets/naskh_principle__progressive_restriction.pl', 'naskh_principle__progressive_restriction', 'NASKH_PRINCIPLE__PROGRESSIVE_RESTRICTION', 129).
test_case('testsets/organization_floor_c0.pl', 'organization_floor_c0', 'ORGANIZATION_FLOOR_C0', 130).
test_case('testsets/personhood_boundary__birth_threshold_reading.pl', 'personhood_boundary__birth_threshold_reading', 'PERSONHOOD_BOUNDARY__BIRTH_THRESHOLD_READING', 131).
test_case('testsets/personhood_boundary__fitness_contingent_reading.pl', 'personhood_boundary__fitness_contingent_reading', 'PERSONHOOD_BOUNDARY__FITNESS_CONTINGENT_READING', 132).
test_case('testsets/personhood_boundary__potential_based_reading.pl', 'personhood_boundary__potential_based_reading', 'PERSONHOOD_BOUNDARY__POTENTIAL_BASED_READING', 133).
test_case('testsets/preparedness_commitment__competence_reading.pl', 'preparedness_commitment__competence_reading', 'PREPAREDNESS_COMMITMENT__COMPETENCE_READING', 134).
test_case('testsets/preparedness_commitment__husk_reading.pl', 'preparedness_commitment__husk_reading', 'PREPAREDNESS_COMMITMENT__HUSK_READING', 135).
test_case('testsets/preparedness_commitment__hybrid_reading.pl', 'preparedness_commitment__hybrid_reading', 'PREPAREDNESS_COMMITMENT__HYBRID_READING', 136).
test_case('testsets/provincial_sovereignty_boundary__compact_federalism.pl', 'provincial_sovereignty_boundary__compact_federalism', 'PROVINCIAL_SOVEREIGNTY_BOUNDARY__COMPACT_FEDERALISM', 137).
test_case('testsets/provincial_sovereignty_boundary__constitutional_subordination.pl', 'provincial_sovereignty_boundary__constitutional_subordination', 'PROVINCIAL_SOVEREIGNTY_BOUNDARY__CONSTITUTIONAL_SUBORDINATION', 138).
test_case('testsets/provincial_sovereignty_boundary__resource_sovereignty_primacy.pl', 'provincial_sovereignty_boundary__resource_sovereignty_primacy', 'PROVINCIAL_SOVEREIGNTY_BOUNDARY__RESOURCE_SOVEREIGNTY_PRIMACY', 139).
test_case('testsets/public_health_mandate_authority__bodily_autonomy_primary.pl', 'public_health_mandate_authority__bodily_autonomy_primary', 'PUBLIC_HEALTH_MANDATE_AUTHORITY__BODILY_AUTONOMY_PRIMARY', 140).
test_case('testsets/public_health_mandate_authority__proportionality_reading.pl', 'public_health_mandate_authority__proportionality_reading', 'PUBLIC_HEALTH_MANDATE_AUTHORITY__PROPORTIONALITY_READING', 141).
test_case('testsets/public_health_mandate_authority__public_health_primary.pl', 'public_health_mandate_authority__public_health_primary', 'PUBLIC_HEALTH_MANDATE_AUTHORITY__PUBLIC_HEALTH_PRIMARY', 142).
test_case('testsets/qwerty_persistence__incumbent_preservation_reading.pl', 'qwerty_persistence__incumbent_preservation_reading', 'QWERTY_PERSISTENCE__INCUMBENT_PRESERVATION_READING', 143).
test_case('testsets/qwerty_persistence__lapsed_alternatives_reading.pl', 'qwerty_persistence__lapsed_alternatives_reading', 'QWERTY_PERSISTENCE__LAPSED_ALTERNATIVES_READING', 144).
test_case('testsets/reading_acquisition_legitimacy__balanced_literacy_integration.pl', 'reading_acquisition_legitimacy__balanced_literacy_integration', 'READING_ACQUISITION_LEGITIMACY__BALANCED_LITERACY_INTEGRATION', 145).
test_case('testsets/reading_acquisition_legitimacy__phonics_decoding_primacy.pl', 'reading_acquisition_legitimacy__phonics_decoding_primacy', 'READING_ACQUISITION_LEGITIMACY__PHONICS_DECODING_PRIMACY', 146).
test_case('testsets/reading_acquisition_legitimacy__structured_literacy_remediation.pl', 'reading_acquisition_legitimacy__structured_literacy_remediation', 'READING_ACQUISITION_LEGITIMACY__STRUCTURED_LITERACY_REMEDIATION', 147).
test_case('testsets/remonstrance_authority__crown_reading.pl', 'remonstrance_authority__crown_reading', 'REMONSTRANCE_AUTHORITY__CROWN_READING', 148).
test_case('testsets/remonstrance_authority__magistrate_reading.pl', 'remonstrance_authority__magistrate_reading', 'REMONSTRANCE_AUTHORITY__MAGISTRATE_READING', 149).
test_case('testsets/sacrifice_obligation_kernel__messianic_suspension_reading.pl', 'sacrifice_obligation_kernel__messianic_suspension_reading', 'SACRIFICE_OBLIGATION_KERNEL__MESSIANIC_SUSPENSION_READING', 150).
test_case('testsets/sacrifice_obligation_kernel__performance_only_reading.pl', 'sacrifice_obligation_kernel__performance_only_reading', 'SACRIFICE_OBLIGATION_KERNEL__PERFORMANCE_ONLY_READING', 151).
test_case('testsets/sacrifice_obligation_kernel__study_as_exercise_reading.pl', 'sacrifice_obligation_kernel__study_as_exercise_reading', 'SACRIFICE_OBLIGATION_KERNEL__STUDY_AS_EXERCISE_READING', 152).
test_case('testsets/sacrifice_obligation_kernel__symbolic_archive_reading.pl', 'sacrifice_obligation_kernel__symbolic_archive_reading', 'SACRIFICE_OBLIGATION_KERNEL__SYMBOLIC_ARCHIVE_READING', 153).
test_case('testsets/scale_ceiling_c0.pl', 'scale_ceiling_c0', 'SCALE_CEILING_C0', 154).
test_case('testsets/second_amendment_boundary__individual_right_reading.pl', 'second_amendment_boundary__individual_right_reading', 'SECOND_AMENDMENT_BOUNDARY__INDIVIDUAL_RIGHT_READING', 155).
test_case('testsets/second_amendment_boundary__insurrectionist_reading.pl', 'second_amendment_boundary__insurrectionist_reading', 'SECOND_AMENDMENT_BOUNDARY__INSURRECTIONIST_READING', 156).
test_case('testsets/second_amendment_boundary__militia_conditioned_reading.pl', 'second_amendment_boundary__militia_conditioned_reading', 'SECOND_AMENDMENT_BOUNDARY__MILITIA_CONDITIONED_READING', 157).
test_case('testsets/shinbutsu_ontological_substrate__domain_partition_reading.pl', 'shinbutsu_ontological_substrate__domain_partition_reading', 'SHINBUTSU_ONTOLOGICAL_SUBSTRATE__DOMAIN_PARTITION_READING', 158).
test_case('testsets/shinbutsu_ontological_substrate__incoherent_bundle_reading.pl', 'shinbutsu_ontological_substrate__incoherent_bundle_reading', 'SHINBUTSU_ONTOLOGICAL_SUBSTRATE__INCOHERENT_BUNDLE_READING', 159).
test_case('testsets/shinbutsu_ontological_substrate__syncretic_fusion_reading.pl', 'shinbutsu_ontological_substrate__syncretic_fusion_reading', 'SHINBUTSU_ONTOLOGICAL_SUBSTRATE__SYNCRETIC_FUSION_READING', 160).
test_case('testsets/simultaneous_veneration__domain_partition_reading.pl', 'simultaneous_veneration__domain_partition_reading', 'SIMULTANEOUS_VENERATION__DOMAIN_PARTITION_READING', 161).
test_case('testsets/simultaneous_veneration__ontological_fusion_reading.pl', 'simultaneous_veneration__ontological_fusion_reading', 'SIMULTANEOUS_VENERATION__ONTOLOGICAL_FUSION_READING', 162).
test_case('testsets/simultaneous_veneration__pragmatic_incoherence_reading.pl', 'simultaneous_veneration__pragmatic_incoherence_reading', 'SIMULTANEOUS_VENERATION__PRAGMATIC_INCOHERENCE_READING', 163).
test_case('testsets/sovereign_legitimacy__constitutional_hybrid_reading.pl', 'sovereign_legitimacy__constitutional_hybrid_reading', 'SOVEREIGN_LEGITIMACY__CONSTITUTIONAL_HYBRID_READING', 164).
test_case('testsets/sovereign_legitimacy__monarchical_reading.pl', 'sovereign_legitimacy__monarchical_reading', 'SOVEREIGN_LEGITIMACY__MONARCHICAL_READING', 165).
test_case('testsets/sovereign_legitimacy__republican_reading.pl', 'sovereign_legitimacy__republican_reading', 'SOVEREIGN_LEGITIMACY__REPUBLICAN_READING', 166).
test_case('testsets/substance_control_legitimacy__harm_reduction_reading.pl', 'substance_control_legitimacy__harm_reduction_reading', 'SUBSTANCE_CONTROL_LEGITIMACY__HARM_REDUCTION_READING', 167).
test_case('testsets/substance_control_legitimacy__legalization_reading.pl', 'substance_control_legitimacy__legalization_reading', 'SUBSTANCE_CONTROL_LEGITIMACY__LEGALIZATION_READING', 168).
test_case('testsets/substance_control_legitimacy__prohibition_reading.pl', 'substance_control_legitimacy__prohibition_reading', 'SUBSTANCE_CONTROL_LEGITIMACY__PROHIBITION_READING', 169).
test_case('testsets/territorial_sovereignty_legitimacy__covenant_continuity_reading.pl', 'territorial_sovereignty_legitimacy__covenant_continuity_reading', 'TERRITORIAL_SOVEREIGNTY_LEGITIMACY__COVENANT_CONTINUITY_READING', 170).
test_case('testsets/territorial_sovereignty_legitimacy__existential_matrix_reading.pl', 'territorial_sovereignty_legitimacy__existential_matrix_reading', 'TERRITORIAL_SOVEREIGNTY_LEGITIMACY__EXISTENTIAL_MATRIX_READING', 171).
test_case('testsets/territorial_sovereignty_legitimacy__self_determination_reading.pl', 'territorial_sovereignty_legitimacy__self_determination_reading', 'TERRITORIAL_SOVEREIGNTY_LEGITIMACY__SELF_DETERMINATION_READING', 172).
test_case('testsets/us_constitution_interpretive__living_constitution_reading.pl', 'us_constitution_interpretive__living_constitution_reading', 'US_CONSTITUTION_INTERPRETIVE__LIVING_CONSTITUTION_READING', 173).
test_case('testsets/us_constitution_interpretive__originalist_reading.pl', 'us_constitution_interpretive__originalist_reading', 'US_CONSTITUTION_INTERPRETIVE__ORIGINALIST_READING', 174).
test_case('testsets/us_constitution_interpretive__popular_constitutionalism_reading.pl', 'us_constitution_interpretive__popular_constitutionalism_reading', 'US_CONSTITUTION_INTERPRETIVE__POPULAR_CONSTITUTIONALISM_READING', 175).
test_case('testsets/waitangi_sovereignty_allocation__crown_sovereignty_reading.pl', 'waitangi_sovereignty_allocation__crown_sovereignty_reading', 'WAITANGI_SOVEREIGNTY_ALLOCATION__CROWN_SOVEREIGNTY_READING', 176).
test_case('testsets/waitangi_sovereignty_allocation__partnership_reading.pl', 'waitangi_sovereignty_allocation__partnership_reading', 'WAITANGI_SOVEREIGNTY_ALLOCATION__PARTNERSHIP_READING', 177).
test_case('testsets/waitangi_sovereignty_allocation__rangatiratanga_reading.pl', 'waitangi_sovereignty_allocation__rangatiratanga_reading', 'WAITANGI_SOVEREIGNTY_ALLOCATION__RANGATIRATANGA_READING', 178).
test_case('testsets/westphalian_sovereignty__absolute_sovereignty.pl', 'westphalian_sovereignty__absolute_sovereignty', 'WESTPHALIAN_SOVEREIGNTY__ABSOLUTE_SOVEREIGNTY', 179).
test_case('testsets/westphalian_sovereignty__conditional_sovereignty.pl', 'westphalian_sovereignty__conditional_sovereignty', 'WESTPHALIAN_SOVEREIGNTY__CONDITIONAL_SOVEREIGNTY', 180).
test_case('testsets/westphalian_sovereignty__graduated_sovereignty.pl', 'westphalian_sovereignty__graduated_sovereignty', 'WESTPHALIAN_SOVEREIGNTY__GRADUATED_SOVEREIGNTY', 181).
test_case('testsets/woman_female_category__gender_identity_reading.pl', 'woman_female_category__gender_identity_reading', 'WOMAN_FEMALE_CATEGORY__GENDER_IDENTITY_READING', 182).
test_case('testsets/woman_female_category__hybrid_contextual_reading.pl', 'woman_female_category__hybrid_contextual_reading', 'WOMAN_FEMALE_CATEGORY__HYBRID_CONTEXTUAL_READING', 183).
test_case('testsets/woman_female_category__sex_biology_reading.pl', 'woman_female_category__sex_biology_reading', 'WOMAN_FEMALE_CATEGORY__SEX_BIOLOGY_READING', 184).
test_case('testsets/wto_treaty_framework__developmental_reading.pl', 'wto_treaty_framework__developmental_reading', 'WTO_TREATY_FRAMEWORK__DEVELOPMENTAL_READING', 185).
test_case('testsets/wto_treaty_framework__market_access_reading.pl', 'wto_treaty_framework__market_access_reading', 'WTO_TREATY_FRAMEWORK__MARKET_ACCESS_READING', 186).

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

