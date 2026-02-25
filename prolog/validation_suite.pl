:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/absorbing_markov_chains.pl', 'absorbing_markov_chains', 'ABSORBING_MARKOV_CHAINS', 1).
test_case('testsets/abstraction_boundary_overrun.pl', 'abstraction_boundary_overrun', 'ABSTRACTION_BOUNDARY_OVERRUN', 2).
test_case('testsets/abstraction_leakage.pl', 'abstraction_leakage', 'ABSTRACTION_LEAKAGE', 3).
test_case('testsets/academic_fashion_modernism_2026.pl', 'academic_fashion_modernism_2026', 'ACADEMIC_FASHION_MODERNISM_2026', 4).
test_case('testsets/academic_tenure_system.pl', 'academic_tenure_system', 'ACADEMIC_TENURE_SYSTEM', 5).
test_case('testsets/access_arbitrage.pl', 'access_arbitrage', 'ACCESS_ARBITRAGE', 6).
test_case('testsets/acip_hep_b_infant_mandate.pl', 'acip_hep_b_infant_mandate', 'ACIP_HEP_B_INFANT_MANDATE', 7).
test_case('testsets/ad_fus_coordination.pl', 'ad_fus_coordination', 'AD_FUS_COORDINATION', 8).
test_case('testsets/ad_synaptic_deficit.pl', 'ad_synaptic_deficit', 'AD_SYNAPTIC_DEFICIT', 9).
test_case('testsets/adaptive_lag_trap.pl', 'adaptive_lag_trap', 'ADAPTIVE_LAG_TRAP', 10).
test_case('testsets/adversarial_truth_decay.pl', 'adversarial_truth_decay', 'ADVERSARIAL_TRUTH_DECAY', 11).
test_case('testsets/adverse_possession.pl', 'adverse_possession', 'ADVERSE_POSSESSION', 12).
test_case('testsets/advice_as_dangerous_gift.pl', 'advice_as_dangerous_gift', 'ADVICE_AS_DANGEROUS_GIFT', 13).
test_case('testsets/agency_atrophy.pl', 'agency_atrophy', 'AGENCY_ATROPHY', 14).
test_case('testsets/agg1_genetic_determinism.pl', 'agg1_genetic_determinism', 'AGG1_GENETIC_DETERMINISM', 15).
test_case('testsets/aging_well_assessment.pl', 'aging_well_assessment', 'AGING_WELL_ASSESSMENT', 16).
test_case('testsets/ai_adoption_stigma.pl', 'ai_adoption_stigma', 'AI_ADOPTION_STIGMA', 17).
test_case('testsets/ai_auditability_gap.pl', 'ai_auditability_gap', 'AI_AUDITABILITY_GAP', 18).
test_case('testsets/ai_banal_capture.pl', 'ai_banal_capture', 'AI_BANAL_CAPTURE', 19).
test_case('testsets/ai_cognitive_diversity_arbitrage.pl', 'ai_cognitive_diversity_arbitrage', 'AI_COGNITIVE_DIVERSITY_ARBITRAGE', 20).
test_case('testsets/ai_driven_surveillance_sensor_layer.pl', 'ai_driven_surveillance_sensor_layer', 'AI_DRIVEN_SURVEILLANCE_SENSOR_LAYER', 21).
test_case('testsets/ai_edu_decentralization.pl', 'ai_edu_decentralization', 'AI_EDU_DECENTRALIZATION', 22).
test_case('testsets/ai_nonconsensual_content_facilitation.pl', 'ai_nonconsensual_content_facilitation', 'AI_NONCONSENSUAL_CONTENT_FACILITATION', 23).
test_case('testsets/ai_performance_watermark.pl', 'ai_performance_watermark', 'AI_PERFORMANCE_WATERMARK', 24).
test_case('testsets/ai_professional_displacement.pl', 'ai_professional_displacement', 'AI_PROFESSIONAL_DISPLACEMENT', 25).
test_case('testsets/ai_religion_regulation.pl', 'ai_religion_regulation', 'AI_RELIGION_REGULATION', 26).
test_case('testsets/ai_scholar_citation_trap.pl', 'ai_scholar_citation_trap', 'AI_SCHOLAR_CITATION_TRAP', 27).
test_case('testsets/ai_superpowers_race_2026.pl', 'ai_superpowers_race_2026', 'AI_SUPERPOWERS_RACE_2026', 28).
test_case('testsets/ai_task_horizon_reliability.pl', 'ai_task_horizon_reliability', 'AI_TASK_HORIZON_RELIABILITY', 29).
test_case('testsets/ai_training_data_dependency.pl', 'ai_training_data_dependency', 'AI_TRAINING_DATA_DEPENDENCY', 30).
test_case('testsets/airport_slot_use_it_or_lose_it.pl', 'airport_slot_use_it_or_lose_it', 'AIRPORT_SLOT_USE_IT_OR_LOSE_IT', 31).
test_case('testsets/algeria_france_colonial_legacy.pl', 'algeria_france_colonial_legacy', 'ALGERIA_FRANCE_COLONIAL_LEGACY', 32).
test_case('testsets/algorithmic_bias.pl', 'algorithmic_bias', 'ALGORITHMIC_BIAS', 33).
test_case('testsets/algorithmic_epistemic_capture.pl', 'algorithmic_epistemic_capture', 'ALGORITHMIC_EPISTEMIC_CAPTURE', 34).
test_case('testsets/alignment_tax_tradeoff.pl', 'alignment_tax_tradeoff', 'ALIGNMENT_TAX_TRADEOFF', 35).
test_case('testsets/alternative_sovereignty_scaffold.pl', 'alternative_sovereignty_scaffold', 'ALTERNATIVE_SOVEREIGNTY_SCAFFOLD', 36).
test_case('testsets/altruistic_misery_paradox_2026.pl', 'altruistic_misery_paradox_2026', 'ALTRUISTIC_MISERY_PARADOX_2026', 37).
test_case('testsets/alzheimers_levetiracetam.pl', 'alzheimers_levetiracetam', 'ALZHEIMERS_LEVETIRACETAM', 38).
test_case('testsets/alzheimers_nlrp3_inflammasome.pl', 'alzheimers_nlrp3_inflammasome', 'ALZHEIMERS_NLRP3_INFLAMMASOME', 39).
test_case('testsets/amish_technological_renunciation.pl', 'amish_technological_renunciation', 'AMISH_TECHNOLOGICAL_RENUNCIATION', 40).
test_case('testsets/ancestral_pueblo_hydrology.pl', 'ancestral_pueblo_hydrology', 'ANCESTRAL_PUEBLO_HYDROLOGY', 41).
test_case('testsets/ancient_antibiotic_resistance.pl', 'ancient_antibiotic_resistance', 'ANCIENT_ANTIBIOTIC_RESISTANCE', 42).
test_case('testsets/ancient_grudge_verona.pl', 'ancient_grudge_verona', 'ANCIENT_GRUDGE_VERONA', 43).
test_case('testsets/anticipatory_capacity_failure.pl', 'anticipatory_capacity_failure', 'ANTICIPATORY_CAPACITY_FAILURE', 44).
test_case('testsets/antifragility.pl', 'antifragility', 'ANTIFRAGILITY', 45).
test_case('testsets/antikythera_knowledge_loss.pl', 'antikythera_knowledge_loss', 'ANTIKYTHERA_KNOWLEDGE_LOSS', 46).
test_case('testsets/antikythera_planetary_model.pl', 'antikythera_planetary_model', 'ANTIKYTHERA_PLANETARY_MODEL', 47).
test_case('testsets/apartheid_nuclear_program.pl', 'apartheid_nuclear_program', 'APARTHEID_NUCLEAR_PROGRAM', 48).
test_case('testsets/ape_cognition_framework.pl', 'ape_cognition_framework', 'APE_COGNITION_FRAMEWORK', 49).
test_case('testsets/appropriations_brinkmanship.pl', 'appropriations_brinkmanship', 'APPROPRIATIONS_BRINKMANSHIP', 50).
test_case('testsets/arctic_geopolitical_flashpoint.pl', 'arctic_geopolitical_flashpoint', 'ARCTIC_GEOPOLITICAL_FLASHPOINT', 51).
test_case('testsets/arctic_maritime_control.pl', 'arctic_maritime_control', 'ARCTIC_MARITIME_CONTROL', 52).
test_case('testsets/arg_ev_tariff.pl', 'arg_ev_tariff', 'ARG_EV_TARIFF', 53).
test_case('testsets/armra_colostrum_regulation.pl', 'armra_colostrum_regulation', 'ARMRA_COLOSTRUM_REGULATION', 54).
test_case('testsets/arrows_impossibility_theorem.pl', 'arrows_impossibility_theorem', 'ARROWS_IMPOSSIBILITY_THEOREM', 55).
test_case('testsets/art_market_decoupling.pl', 'art_market_decoupling', 'ART_MARKET_DECOUPLING', 56).
test_case('testsets/artificial_scarcity_scaffold.pl', 'artificial_scarcity_scaffold', 'ARTIFICIAL_SCARCITY_SCAFFOLD', 57).
test_case('testsets/artificial_snow_2026.pl', 'artificial_snow_2026', 'ARTIFICIAL_SNOW_2026', 58).
test_case('testsets/asce_7_22_seismic_design.pl', 'asce_7_22_seismic_design', 'ASCE_7_22_SEISMIC_DESIGN', 59).
test_case('testsets/asean_ceasefire_2011.pl', 'asean_ceasefire_2011', 'ASEAN_CEASEFIRE_2011', 60).
test_case('testsets/asml_high_na_euv_access.pl', 'asml_high_na_euv_access', 'ASML_HIGH_NA_EUV_ACCESS', 61).
test_case('testsets/asshole_filter_2015.pl', 'asshole_filter_2015', 'ASSHOLE_FILTER_2015', 62).
test_case('testsets/astm_d638_tensile_testing.pl', 'astm_d638_tensile_testing', 'ASTM_D638_TENSILE_TESTING', 63).
test_case('testsets/asymmetric_burden_distribution.pl', 'asymmetric_burden_distribution', 'ASYMMETRIC_BURDEN_DISTRIBUTION', 64).
test_case('testsets/asymmetric_computational_difficulty.pl', 'asymmetric_computational_difficulty', 'ASYMMETRIC_COMPUTATIONAL_DIFFICULTY', 65).
test_case('testsets/atrophied_optimization_piton.pl', 'atrophied_optimization_piton', 'ATROPHIED_OPTIMIZATION_PITON', 66).
test_case('testsets/attention_as_bottleneck_resource.pl', 'attention_as_bottleneck_resource', 'ATTENTION_AS_BOTTLENECK_RESOURCE', 67).
test_case('testsets/attention_market_cannibalization.pl', 'attention_market_cannibalization', 'ATTENTION_MARKET_CANNIBALIZATION', 68).
test_case('testsets/attribution_ambiguity_triplet_sc.pl', 'attribution_ambiguity_triplet_sc', 'ATTRIBUTION_AMBIGUITY_TRIPLET_SC', 69).
test_case('testsets/attritional_warfare_doctrine_ru_ua_2026.pl', 'attritional_warfare_doctrine_ru_ua_2026', 'ATTRITIONAL_WARFARE_DOCTRINE_RU_UA_2026', 70).
test_case('testsets/au_social_media_ban_u16.pl', 'au_social_media_ban_u16', 'AU_SOCIAL_MEDIA_BAN_U16', 71).
test_case('testsets/australia_social_ban_2026.pl', 'australia_social_ban_2026', 'AUSTRALIA_SOCIAL_BAN_2026', 72).
test_case('testsets/authoritarian_power_paradox.pl', 'authoritarian_power_paradox', 'AUTHORITARIAN_POWER_PARADOX', 73).
test_case('testsets/automatic_enrollment_defaults.pl', 'automatic_enrollment_defaults', 'AUTOMATIC_ENROLLMENT_DEFAULTS', 74).
test_case('testsets/autonomous_toolchain_sprawl.pl', 'autonomous_toolchain_sprawl', 'AUTONOMOUS_TOOLCHAIN_SPRAWL', 75).
test_case('testsets/availability_heuristic.pl', 'availability_heuristic', 'AVAILABILITY_HEURISTIC', 76).
test_case('testsets/average_is_over_2026.pl', 'average_is_over_2026', 'AVERAGE_IS_OVER_2026', 77).
test_case('testsets/awareness_without_leverage.pl', 'awareness_without_leverage', 'AWARENESS_WITHOUT_LEVERAGE', 78).
test_case('testsets/axiom_of_choice_determinacy.pl', 'axiom_of_choice_determinacy', 'AXIOM_OF_CHOICE_DETERMINACY', 79).
test_case('testsets/axiom_reasoner_2026.pl', 'axiom_reasoner_2026', 'AXIOM_REASONER_2026', 80).
test_case('testsets/banach_fixed_point.pl', 'banach_fixed_point', 'BANACH_FIXED_POINT', 81).
test_case('testsets/banach_fixed_point_theorem.pl', 'banach_fixed_point_theorem', 'BANACH_FIXED_POINT_THEOREM', 82).
test_case('testsets/bangladesh_july_national_charter.pl', 'bangladesh_july_national_charter', 'BANGLADESH_JULY_NATIONAL_CHARTER', 83).
test_case('testsets/base_pair_complementarity.pl', 'base_pair_complementarity', 'BASE_PAIR_COMPLEMENTARITY', 84).
test_case('testsets/basel_problem_convergence.pl', 'basel_problem_convergence', 'BASEL_PROBLEM_CONVERGENCE', 85).
test_case('testsets/bay_of_pigs_operational_silo.pl', 'bay_of_pigs_operational_silo', 'BAY_OF_PIGS_OPERATIONAL_SILO', 86).
test_case('testsets/bayes_theorem.pl', 'bayes_theorem', 'BAYES_THEOREM', 87).
test_case('testsets/bedouin_sedentary_transition.pl', 'bedouin_sedentary_transition', 'BEDOUIN_SEDENTARY_TRANSITION', 88).
test_case('testsets/beehiiv_platform_model.pl', 'beehiiv_platform_model', 'BEEHIIV_PLATFORM_MODEL', 89).
test_case('testsets/belief_argument_conclusion.pl', 'belief_argument_conclusion', 'BELIEF_ARGUMENT_CONCLUSION', 90).
test_case('testsets/berkshire_compounding_culture.pl', 'berkshire_compounding_culture', 'BERKSHIRE_COMPOUNDING_CULTURE', 91).
test_case('testsets/bgs_eigenvector_thermalization.pl', 'bgs_eigenvector_thermalization', 'BGS_EIGENVECTOR_THERMALIZATION', 92).
test_case('testsets/bgs_spectral_universality.pl', 'bgs_spectral_universality', 'BGS_SPECTRAL_UNIVERSALITY', 93).
test_case('testsets/bh_merger_gravitational_infall.pl', 'bh_merger_gravitational_infall', 'BH_MERGER_GRAVITATIONAL_INFALL', 94).
test_case('testsets/big_data_astrophysics_arbitrage.pl', 'big_data_astrophysics_arbitrage', 'BIG_DATA_ASTROPHYSICS_ARBITRAGE', 95).
test_case('testsets/biological_curiosity.pl', 'biological_curiosity', 'BIOLOGICAL_CURIOSITY', 96).
test_case('testsets/biological_specification.pl', 'biological_specification', 'BIOLOGICAL_SPECIFICATION', 97).
test_case('testsets/bip_narrative_illusion.pl', 'bip_narrative_illusion', 'BIP_NARRATIVE_ILLUSION', 98).
test_case('testsets/birthday_paradox_collison.pl', 'birthday_paradox_collison', 'BIRTHDAY_PARADOX_COLLISON', 99).
test_case('testsets/blackstone_carried_interest_taxation.pl', 'blackstone_carried_interest_taxation', 'BLACKSTONE_CARRIED_INTEREST_TAXATION', 100).
test_case('testsets/blackstone_conflicts_of_interest.pl', 'blackstone_conflicts_of_interest', 'BLACKSTONE_CONFLICTS_OF_INTEREST', 101).
test_case('testsets/blackstone_smd_control.pl', 'blackstone_smd_control', 'BLACKSTONE_SMD_CONTROL', 102).
test_case('testsets/bnpl_payment_systems.pl', 'bnpl_payment_systems', 'BNPL_PAYMENT_SYSTEMS', 103).
test_case('testsets/board_of_peace_2026.pl', 'board_of_peace_2026', 'BOARD_OF_PEACE_2026', 104).
test_case('testsets/boe_base_rate_policy_2024.pl', 'boe_base_rate_policy_2024', 'BOE_BASE_RATE_POLICY_2024', 105).
test_case('testsets/boiled_pineapple_trend_2026.pl', 'boiled_pineapple_trend_2026', 'BOILED_PINEAPPLE_TREND_2026', 106).
test_case('testsets/bonbon_drop_sticker_craze.pl', 'bonbon_drop_sticker_craze', 'BONBON_DROP_STICKER_CRAZE', 107).
test_case('testsets/boom_bust_path_dependency.pl', 'boom_bust_path_dependency', 'BOOM_BUST_PATH_DEPENDENCY', 108).
test_case('testsets/bor_tax_exemption_nl.pl', 'bor_tax_exemption_nl', 'BOR_TAX_EXEMPTION_NL', 109).
test_case('testsets/borsuk_ulam_theorem.pl', 'borsuk_ulam_theorem', 'BORSUK_ULAM_THEOREM', 110).
test_case('testsets/boundary_dissolution_risk.pl', 'boundary_dissolution_risk', 'BOUNDARY_DISSOLUTION_RISK', 111).
test_case('testsets/brain_network_paradigm_2026.pl', 'brain_network_paradigm_2026', 'BRAIN_NETWORK_PARADIGM_2026', 112).
test_case('testsets/brazil_2026_general_elections.pl', 'brazil_2026_general_elections', 'BRAZIL_2026_GENERAL_ELECTIONS', 113).
test_case('testsets/brazil_hiv_vtn_elimination.pl', 'brazil_hiv_vtn_elimination', 'BRAZIL_HIV_VTN_ELIMINATION', 114).
test_case('testsets/brazil_mexico_financial_requirement.pl', 'brazil_mexico_financial_requirement', 'BRAZIL_MEXICO_FINANCIAL_REQUIREMENT', 115).
test_case('testsets/broke_vs_poor_grocery_math.pl', 'broke_vs_poor_grocery_math', 'BROKE_VS_POOR_GROCERY_MATH', 116).
test_case('testsets/brouwer_fixed_point.pl', 'brouwer_fixed_point', 'BROUWER_FIXED_POINT', 117).
test_case('testsets/buffons_needle_pi_estimation.pl', 'buffons_needle_pi_estimation', 'BUFFONS_NEEDLE_PI_ESTIMATION', 118).
test_case('testsets/burali_forte_paradox.pl', 'burali_forte_paradox', 'BURALI_FORTE_PARADOX', 119).
test_case('testsets/burden_of_proof_engineering_safety.pl', 'burden_of_proof_engineering_safety', 'BURDEN_OF_PROOF_ENGINEERING_SAFETY', 120).
test_case('testsets/burden_of_proof_legal_criminal.pl', 'burden_of_proof_legal_criminal', 'BURDEN_OF_PROOF_LEGAL_CRIMINAL', 121).
test_case('testsets/burden_of_proof_scientific_empirical.pl', 'burden_of_proof_scientific_empirical', 'BURDEN_OF_PROOF_SCIENTIFIC_EMPIRICAL', 122).
test_case('testsets/bureaucratic_legibility_collapse.pl', 'bureaucratic_legibility_collapse', 'BUREAUCRATIC_LEGIBILITY_COLLAPSE', 123).
test_case('testsets/bureaucratic_self_preservation.pl', 'bureaucratic_self_preservation', 'BUREAUCRATIC_SELF_PRESERVATION', 124).
test_case('testsets/bushman_money_magic.pl', 'bushman_money_magic', 'BUSHMAN_MONEY_MAGIC', 125).
test_case('testsets/busy_beaver_noncomputability.pl', 'busy_beaver_noncomputability', 'BUSY_BEAVER_NONCOMPUTABILITY', 126).
test_case('testsets/bwb_adeg_rewesale_conditions.pl', 'bwb_adeg_rewesale_conditions', 'BWB_ADEG_REWESALE_CONDITIONS', 127).
test_case('testsets/c_physical_blue_wavelength.pl', 'c_physical_blue_wavelength', 'C_PHYSICAL_BLUE_WAVELENGTH', 128).
test_case('testsets/cab_la_patent_access.pl', 'cab_la_patent_access', 'CAB_LA_PATENT_ACCESS', 129).
test_case('testsets/canada_germany_ai_pact.pl', 'canada_germany_ai_pact', 'CANADA_GERMANY_AI_PACT', 130).
test_case('testsets/canada_goose_realignment_2026.pl', 'canada_goose_realignment_2026', 'CANADA_GOOSE_REALIGNMENT_2026', 131).
test_case('testsets/canal_panama_influence.pl', 'canal_panama_influence', 'CANAL_PANAMA_INFLUENCE', 132).
test_case('testsets/cancer_chronotherapy_timing.pl', 'cancer_chronotherapy_timing', 'CANCER_CHRONOTHERAPY_TIMING', 133).
test_case('testsets/cantors_diagonal_argument.pl', 'cantors_diagonal_argument', 'CANTORS_DIAGONAL_ARGUMENT', 134).
test_case('testsets/cap_theorem.pl', 'cap_theorem', 'CAP_THEOREM', 135).
test_case('testsets/capability_eval_overhang.pl', 'capability_eval_overhang', 'CAPABILITY_EVAL_OVERHANG', 136).
test_case('testsets/capital_misallocation_spiral.pl', 'capital_misallocation_spiral', 'CAPITAL_MISALLOCATION_SPIRAL', 137).
test_case('testsets/capital_rotation_ai_narrative.pl', 'capital_rotation_ai_narrative', 'CAPITAL_ROTATION_AI_NARRATIVE', 138).
test_case('testsets/car_ownership_norm_us.pl', 'car_ownership_norm_us', 'CAR_OWNERSHIP_NORM_US', 139).
test_case('testsets/carbon_credit_markets_2026.pl', 'carbon_credit_markets_2026', 'CARBON_CREDIT_MARKETS_2026', 140).
test_case('testsets/carrying_capacity.pl', 'carrying_capacity', 'CARRYING_CAPACITY', 141).
test_case('testsets/cartel_drone_surveillance_el_paso.pl', 'cartel_drone_surveillance_el_paso', 'CARTEL_DRONE_SURVEILLANCE_EL_PASO', 142).
test_case('testsets/cascading_constraint_failure.pl', 'cascading_constraint_failure', 'CASCADING_CONSTRAINT_FAILURE', 143).
test_case('testsets/cascading_uncertainty_2026.pl', 'cascading_uncertainty_2026', 'CASCADING_UNCERTAINTY_2026', 144).
test_case('testsets/cfius_hiefo_emcore_divestment.pl', 'cfius_hiefo_emcore_divestment', 'CFIUS_HIEFO_EMCORE_DIVESTMENT', 145).
test_case('testsets/cg_israelgaza_20231012.pl', 'cg_israelgaza_20231012', 'CG_ISRAELGAZA_20231012', 146).
test_case('testsets/chaitins_omega_undecidability.pl', 'chaitins_omega_undecidability', 'CHAITINS_OMEGA_UNDECIDABILITY', 147).
test_case('testsets/challenger_o_ring_integrity.pl', 'challenger_o_ring_integrity', 'CHALLENGER_O_RING_INTEGRITY', 148).
test_case('testsets/champions_bass_fishing_exclusion.pl', 'champions_bass_fishing_exclusion', 'CHAMPIONS_BASS_FISHING_EXCLUSION', 149).
test_case('testsets/child_marriage.pl', 'child_marriage', 'CHILD_MARRIAGE', 150).
test_case('testsets/china_africa_zero_tariff_2026.pl', 'china_africa_zero_tariff_2026', 'CHINA_AFRICA_ZERO_TARIFF_2026', 151).
test_case('testsets/china_critical_mineral_chokepoint.pl', 'china_critical_mineral_chokepoint', 'CHINA_CRITICAL_MINERAL_CHOKEPOINT', 152).
test_case('testsets/china_ev_export_oversupply.pl', 'china_ev_export_oversupply', 'CHINA_EV_EXPORT_OVERSUPPLY', 153).
test_case('testsets/china_export_led_growth.pl', 'china_export_led_growth', 'CHINA_EXPORT_LED_GROWTH', 154).
test_case('testsets/china_japan_watchlist_2026.pl', 'china_japan_watchlist_2026', 'CHINA_JAPAN_WATCHLIST_2026', 155).
test_case('testsets/china_rare_earth_dominance.pl', 'china_rare_earth_dominance', 'CHINA_RARE_EARTH_DOMINANCE', 156).
test_case('testsets/china_taiwan_reunification_mandate.pl', 'china_taiwan_reunification_mandate', 'CHINA_TAIWAN_REUNIFICATION_MANDATE', 157).
test_case('testsets/china_vactrain_standard.pl', 'china_vactrain_standard', 'CHINA_VACTRAIN_STANDARD', 158).
test_case('testsets/choice_architecture_design.pl', 'choice_architecture_design', 'CHOICE_ARCHITECTURE_DESIGN', 159).
test_case('testsets/chrome_imagen2_integration.pl', 'chrome_imagen2_integration', 'CHROME_IMAGEN2_INTEGRATION', 160).
test_case('testsets/church_turing_thesis.pl', 'church_turing_thesis', 'CHURCH_TURING_THESIS', 161).
test_case('testsets/cia_fbi_legal_wall.pl', 'cia_fbi_legal_wall', 'CIA_FBI_LEGAL_WALL', 162).
test_case('testsets/cinderella_midnight_deadline.pl', 'cinderella_midnight_deadline', 'CINDERELLA_MIDNIGHT_DEADLINE', 163).
test_case('testsets/circadian_decoupling_arbitrage.pl', 'circadian_decoupling_arbitrage', 'CIRCADIAN_DECOUPLING_ARBITRAGE', 164).
test_case('testsets/citation_collapse_dynamics.pl', 'citation_collapse_dynamics', 'CITATION_COLLAPSE_DYNAMICS', 165).
test_case('testsets/civilizational_lifecycle_solara.pl', 'civilizational_lifecycle_solara', 'CIVILIZATIONAL_LIFECYCLE_SOLARA', 166).
test_case('testsets/civilizational_maintenance_debt.pl', 'civilizational_maintenance_debt', 'CIVILIZATIONAL_MAINTENANCE_DEBT', 167).
test_case('testsets/clawderberg_recursive_slop.pl', 'clawderberg_recursive_slop', 'CLAWDERBERG_RECURSIVE_SLOP', 168).
test_case('testsets/click_chemistry_paradigm_2026.pl', 'click_chemistry_paradigm_2026', 'CLICK_CHEMISTRY_PARADIGM_2026', 169).
test_case('testsets/climate_attribution_2026.pl', 'climate_attribution_2026', 'CLIMATE_ATTRIBUTION_2026', 170).
test_case('testsets/climate_event_attribution.pl', 'climate_event_attribution', 'CLIMATE_EVENT_ATTRIBUTION', 171).
test_case('testsets/dccp_tech_corps.pl', 'dccp_tech_corps', 'DCCP_TECH_CORPS', 172).
test_case('testsets/eu_safe_third_country_policy.pl', 'eu_safe_third_country_policy', 'EU_SAFE_THIRD_COUNTRY_POLICY', 173).
test_case('testsets/mars_rover_positioning_system.pl', 'mars_rover_positioning_system', 'MARS_ROVER_POSITIONING_SYSTEM', 174).
test_case('testsets/meta_amd_ai_chip_deal.pl', 'meta_amd_ai_chip_deal', 'META_AMD_AI_CHIP_DEAL', 175).
test_case('testsets/moltbot_religion.pl', 'moltbot_religion', 'MOLTBOT_RELIGION', 176).
test_case('testsets/n26usc469_real_estate_exemption.pl', 'n26usc469_real_estate_exemption', 'N26USC469_REAL_ESTATE_EXEMPTION', 177).
test_case('testsets/n8k_tv_limit_2026.pl', 'n8k_tv_limit_2026', 'N8K_TV_LIMIT_2026', 178).
test_case('testsets/nl_gay_prime_minister_norm.pl', 'nl_gay_prime_minister_norm', 'NL_GAY_PRIME_MINISTER_NORM', 179).
test_case('testsets/post_office_horizon_scandal.pl', 'post_office_horizon_scandal', 'POST_OFFICE_HORIZON_SCANDAL', 180).
test_case('testsets/rail_fleet_electrification_mandate.pl', 'rail_fleet_electrification_mandate', 'RAIL_FLEET_ELECTRIFICATION_MANDATE', 181).
test_case('testsets/section_232_tariffs.pl', 'section_232_tariffs', 'SECTION_232_TARIFFS', 182).

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

