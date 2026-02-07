:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/26usc469_real_estate_exemption.pl', '26usc469_real_estate_exemption', '26USC469_REAL_ESTATE_EXEMPTION', 1).
test_case('testsets/academic_fashion_modernism_2026.pl', 'academic_fashion_modernism_2026', 'ACADEMIC_FASHION_MODERNISM_2026', 2).
test_case('testsets/academic_peer_review_gatekeeping.pl', 'academic_peer_review_gatekeeping', 'ACADEMIC_PEER_REVIEW_GATEKEEPING', 3).
test_case('testsets/academic_tenure_system.pl', 'academic_tenure_system', 'ACADEMIC_TENURE_SYSTEM', 4).
test_case('testsets/ad_fus_coordination.pl', 'ad_fus_coordination', 'AD_FUS_COORDINATION', 5).
test_case('testsets/ad_synaptic_deficit.pl', 'ad_synaptic_deficit', 'AD_SYNAPTIC_DEFICIT', 6).
test_case('testsets/adverse_possession.pl', 'adverse_possession', 'ADVERSE_POSSESSION', 7).
test_case('testsets/advice_as_dangerous_gift.pl', 'advice_as_dangerous_gift', 'ADVICE_AS_DANGEROUS_GIFT', 8).
test_case('testsets/agentive_optimism_2026.pl', 'agent_opt_2026', 'AGENTIVE_OPTIMISM_2026', 9).
test_case('testsets/ai_auditability_gap.pl', 'ai_auditability_gap', 'AI_AUDITABILITY_GAP', 10).
test_case('testsets/ai_cognitive_diversity_arbitrage.pl', 'ai_cognitive_diversity_arbitrage', 'AI_COGNITIVE_DIVERSITY_ARBITRAGE', 11).
test_case('testsets/ai_driven_surveillance_sensor_layer.pl', 'ai_driven_surveillance_sensor_layer', 'AI_DRIVEN_SURVEILLANCE_SENSOR_LAYER', 12).
test_case('testsets/ai_edu_decentralization.pl', 'ai_edu_decentralization', 'AI_EDU_DECENTRALIZATION', 13).
test_case('testsets/ai_evaluators_matching.pl', 'ai_evaluators_matching', 'AI_EVALUATORS_MATCHING', 14).
test_case('testsets/ai_professional_displacement.pl', 'ai_professional_displacement', 'AI_PROFESSIONAL_DISPLACEMENT', 15).
test_case('testsets/ai_superpowers_race_2026.pl', 'ai_superpowers_2026', 'AI_SUPERPOWERS_RACE_2026', 16).
test_case('testsets/algorithmic_bias.pl', 'algorithmic_bias', 'ALGORITHMIC_BIAS', 17).
test_case('testsets/algorithmic_epistemic_capture.pl', 'algorithmic_epistemic_capture', 'ALGORITHMIC_EPISTEMIC_CAPTURE', 18).
test_case('testsets/alignment_tax_tradeoff.pl', 'alignment_tax_tradeoff', 'ALIGNMENT_TAX_TRADEOFF', 19).
test_case('testsets/altruistic_misery_paradox_2026.pl', 'altruistic_misery_paradox_2026', 'ALTRUISTIC_MISERY_PARADOX_2026', 20).
test_case('testsets/amish_technological_renunciation.pl', 'amish_technological_renunciation', 'AMISH_TECHNOLOGICAL_RENUNCIATION', 21).
test_case('testsets/ancestral_pueblo_hydrology.pl', 'ancestral_pueblo_hydrology', 'ANCESTRAL_PUEBLO_HYDROLOGY', 22).
test_case('testsets/anticipatory_capacity_failure.pl', 'anticipatory_capacity_failure', 'ANTICIPATORY_CAPACITY_FAILURE', 23).
test_case('testsets/antifragility.pl', 'antifragility', 'ANTIFRAGILITY', 24).
test_case('testsets/apartheid_nuclear_program.pl', 'apartheid_nuclear_program', 'APARTHEID_NUCLEAR_PROGRAM', 25).
test_case('testsets/arctic_geopolitical_flashpoint.pl', 'arctic_geopolitical_flashpoint', 'ARCTIC_GEOPOLITICAL_FLASHPOINT', 26).
test_case('testsets/arrows_impossibility_theorem.pl', 'arrows_impossibility_theorem', 'ARROWS_IMPOSSIBILITY_THEOREM', 27).
test_case('testsets/art_market_decoupling.pl', 'art_market_decoupling', 'ART_MARKET_DECOUPLING', 28).
test_case('testsets/asce_7_22_seismic_design.pl', 'asce_cycle_22', 'ASCE_7_22_SEISMIC_DESIGN', 29).
test_case('testsets/asshole_filter_2015.pl', 'asshole_filter_2015', 'ASSHOLE_FILTER_2015', 30).
test_case('testsets/astm_d638_tensile_testing.pl', 'industrial_testing_regime', 'ASTM_D638_TENSILE_TESTING', 31).
test_case('testsets/asymmetric_computational_difficulty.pl', 'asymmetric_computational_difficulty', 'ASYMMETRIC_COMPUTATIONAL_DIFFICULTY', 32).
test_case('testsets/atrophied_optimization_piton.pl', 'atrophied_optimization_piton', 'ATROPHIED_OPTIMIZATION_PITON', 33).
test_case('testsets/australia_social_ban_2026.pl', 'australia_social_ban_2026', 'AUSTRALIA_SOCIAL_BAN_2026', 34).
test_case('testsets/authoritarian_power_paradox.pl', 'authoritarian_power_paradox', 'AUTHORITARIAN_POWER_PARADOX', 35).
test_case('testsets/automatic_enrollment_defaults.pl', 'automatic_enrollment_defaults', 'AUTOMATIC_ENROLLMENT_DEFAULTS', 36).
test_case('testsets/availability_heuristic.pl', 'availability_heuristic', 'AVAILABILITY_HEURISTIC', 37).
test_case('testsets/axiom_of_choice_determinacy.pl', 'ac_era', 'AXIOM_OF_CHOICE_DETERMINACY', 38).
test_case('testsets/axiom_reasoner_2026.pl', 'axiom_reasoner_2026', 'AXIOM_REASONER_2026', 39).
test_case('testsets/banach_fixed_point.pl', 'banach_fixed_point', 'BANACH_FIXED_POINT', 40).
test_case('testsets/banach_tarski_paradox.pl', 'banach_tarski_era', 'BANACH_TARSKI_PARADOX', 41).
test_case('testsets/bangladesh_july_national_charter.pl', 'bangladesh_july_national_charter', 'BANGLADESH_JULY_NATIONAL_CHARTER', 42).
test_case('testsets/base_pair_complementarity.pl', 'dna_structure_discovery', 'BASE_PAIR_COMPLEMENTARITY', 43).
test_case('testsets/basel_problem_convergence.pl', 'basel_era', 'BASEL_PROBLEM_CONVERGENCE', 44).
test_case('testsets/bay_of_pigs_operational_silo.pl', 'bay_of_pigs_operational_silo', 'BAY_OF_PIGS_OPERATIONAL_SILO', 45).
test_case('testsets/bedouin_sedentary_transition.pl', 'bedouin_sedentary_transition', 'BEDOUIN_SEDENTARY_TRANSITION', 46).
test_case('testsets/belief_argument_conclusion.pl', 'belief_argument_conclusion', 'BELIEF_ARGUMENT_CONCLUSION', 47).
test_case('testsets/berkshire_compounding_culture.pl', 'berkshire_compounding_culture', 'BERKSHIRE_COMPOUNDING_CULTURE', 48).
test_case('testsets/big_data_astrophysics_arbitrage.pl', 'big_data_astrophysics_arbitrage', 'BIG_DATA_ASTROPHYSICS_ARBITRAGE', 49).
test_case('testsets/biological_curiosity.pl', 'biological_curiosity', 'BIOLOGICAL_CURIOSITY', 50).
test_case('testsets/birthday_paradox_collison.pl', 'birthday_era', 'BIRTHDAY_PARADOX_COLLISON', 51).
test_case('testsets/blackstone_carried_interest_taxation.pl', 'blackstone_ipo_restructuring', 'BLACKSTONE_CARRIED_INTEREST_TAXATION', 52).
test_case('testsets/boiled_pineapple_trend_2026.pl', 'boiled_pineapple_trend_2026', 'BOILED_PINEAPPLE_TREND_2026', 53).
test_case('testsets/brain_network_paradigm_2026.pl', 'brain_network_paradigm_2026', 'BRAIN_NETWORK_PARADIGM_2026', 54).
test_case('testsets/brouwer_fixed_point.pl', 'brouwer_era', 'BROUWER_FIXED_POINT', 55).
test_case('testsets/buffons_needle_pi_estimation.pl', 'buffon_era', 'BUFFONS_NEEDLE_PI_ESTIMATION', 56).
test_case('testsets/burali_forte_paradox.pl', 'burali_forti_interval', 'BURALI_FORTE_PARADOX', 57).
test_case('testsets/burden_of_proof_legal_criminal.pl', 'criminal_trial_sequence', 'BURDEN_OF_PROOF_LEGAL_CRIMINAL', 58).
test_case('testsets/burden_of_proof_scientific_empirical.pl', 'scientific_revolution_current', 'BURDEN_OF_PROOF_SCIENTIFIC_EMPIRICAL', 59).
test_case('testsets/busy_beaver_noncomputability.pl', 'busy_beaver_era', 'BUSY_BEAVER_NONCOMPUTABILITY', 60).
test_case('testsets/cantor_set_topology.pl', 'cantor_set_topology', 'CANTOR_SET_TOPOLOGY', 61).
test_case('testsets/cap_theorem.pl', 'cap_theorem_interval', 'CAP_THEOREM', 62).
test_case('testsets/central_limit_theorem_convergence.pl', 'clt_era', 'CENTRAL_LIMIT_THEOREM_CONVERGENCE', 63).
test_case('testsets/chaitins_omega_undecidability.pl', 'chaitin_era', 'CHAITINS_OMEGA_UNDECIDABILITY', 64).
test_case('testsets/choice_architecture_design.pl', 'rope_design', 'CHOICE_ARCHITECTURE_DESIGN', 65).
test_case('testsets/church_turing_thesis.pl', 'church_turing_era', 'CHURCH_TURING_THESIS', 66).
test_case('testsets/cinderella_midnight_deadline.pl', 'midnight_deadline_period', 'CINDERELLA_MIDNIGHT_DEADLINE', 67).
test_case('testsets/click_chemistry_paradigm_2026.pl', 'click_chemistry_paradigm_2026', 'CLICK_CHEMISTRY_PARADIGM_2026', 68).
test_case('testsets/climate_attribution_2026.pl', 'climate_attribution_2026', 'CLIMATE_ATTRIBUTION_2026', 69).
test_case('testsets/climate_target_one_point_five.pl', 'climate_target_one_point_five', 'CLIMATE_TARGET_ONE_POINT_FIVE', 70).
test_case('testsets/clt_convergence_2026.pl', 'clt_convergence_2026', 'CLT_CONVERGENCE_2026', 71).
test_case('testsets/coffee_cardiovascular_2026.pl', 'coffee_cardiovascular_2026', 'COFFEE_CARDIOVASCULAR_2026', 72).
test_case('testsets/coinbase_crypto_volatility.pl', 'coinbase_ipo_window', 'COINBASE_CRYPTO_VOLATILITY', 73).
test_case('testsets/collatz_conjecture_determinism.pl', 'collatz_era', 'COLLATZ_CONJECTURE_DETERMINISM', 74).
test_case('testsets/columbia_2026_elections.pl', 'colombia_2026_presidential_election', 'COLUMBIA_2026_ELECTIONS', 75).
test_case('testsets/comitatus_bond.pl', 'comitatus_era', 'COMITATUS_BOND', 76).
test_case('testsets/communal_narcissism_social_trap.pl', 'communal_narcissism_social_trap', 'COMMUNAL_NARCISSISM_SOCIAL_TRAP', 77).
test_case('testsets/confirmation_bias.pl', 'confirmation_bias_interval', 'CONFIRMATION_BIAS', 78).
test_case('testsets/constitutional_supremacy.pl', 'constitutional_supremacy', 'CONSTITUTIONAL_SUPREMACY', 79).
test_case('testsets/continuum_hypothesis_undecidability.pl', 'continuum_era', 'CONTINUUM_HYPOTHESIS_UNDECIDABILITY', 80).
test_case('testsets/conways_game_of_life_dynamics.pl', 'conway_era', 'CONWAYS_GAME_OF_LIFE_DYNAMICS', 81).
test_case('testsets/copyleft_viral_licensing.pl', 'copyleft_viral_licensing', 'COPYLEFT_VIRAL_LICENSING', 82).
test_case('testsets/copyright_protection.pl', 'copyright_protection', 'COPYRIGHT_PROTECTION', 83).
test_case('testsets/countable_infinity_cardinality.pl', 'countable_infinity_cardinality', 'COUNTABLE_INFINITY_CARDINALITY', 84).
test_case('testsets/cow_field_poop.pl', 'cow_field_poop', 'COW_FIELD_POOP', 85).
test_case('testsets/creative_commons_licensing.pl', 'creative_commons_licensing', 'CREATIVE_COMMONS_LICENSING', 86).
test_case('testsets/crispr_genomic_rewrite_2026.pl', 'crispr_genomic_rewrite_2026', 'CRISPR_GENOMIC_REWRITE_2026', 87).
test_case('testsets/cuban_missile_crisis_excomm_delibration.pl', 'thirteen_days_crisis', 'CUBAN_MISSILE_CRISIS_EXCOMM_DELIBRATION', 88).
test_case('testsets/cultural_refragmentation_2026.pl', 'cultural_refragmentation_2026', 'CULTURAL_REFRAGMENTATION_2026', 89).
test_case('testsets/cuny_light_2026.pl', 'cuny_light_2026', 'CUNY_LIGHT_2026', 90).
test_case('testsets/currys_paradox.pl', 'currys_paradox_interval', 'CURRYS_PARADOX', 91).
test_case('testsets/damped_harmonics.pl', 'damped_harmonics', 'DAMPED_HARMONICS', 92).
test_case('testsets/deferential_realism_core.pl', 'dr_recursive_int', 'DEFERENTIAL_REALISM_CORE', 93).
test_case('testsets/dldr_information_policy.pl', 'dldr_policy_interval', 'DLDR_INFORMATION_POLICY', 94).
test_case('testsets/dunbars_number.pl', 'dunbar_interval', 'DUNBARS_NUMBER', 95).
test_case('testsets/e2ee_digital_privacy_2026.pl', 'e2ee_digital_privacy_2026', 'E2EE_DIGITAL_PRIVACY_2026', 96).
test_case('testsets/educational_unbundling_implementation.pl', 'implementation_window_2026', 'EDUCATIONAL_UNBUNDLING_IMPLEMENTATION', 97).
test_case('testsets/electrification_scale_2026.pl', 'electrification_scale_2026', 'ELECTRIFICATION_SCALE_2026', 98).
test_case('testsets/elencher_identity_transformation.pl', 'elencher_identity_transformation', 'ELENCHER_IDENTITY_TRANSFORMATION', 99).
test_case('testsets/empty_tomb_transformation.pl', 'empty_tomb_transformation', 'EMPTY_TOMB_TRANSFORMATION', 100).
test_case('testsets/endowment_effect.pl', 'endowment_interval', 'ENDOWMENT_EFFECT', 101).
test_case('testsets/epigenetics_complexity_2026.pl', 'epigenetics_complexity_2026', 'EPIGENETICS_COMPLEXITY_2026', 102).
test_case('testsets/epstein_espionage_crisis_2026.pl', 'epstein_espionage_2026', 'EPSTEIN_ESPIONAGE_CRISIS_2026', 103).
test_case('testsets/epstein_files_2026.pl', 'epstein_files_2026', 'EPSTEIN_FILES_2026', 104).
test_case('testsets/epstein_kgb_honeytrap.pl', 'epstein_honeytrap', 'EPSTEIN_KGB_HONEYTRAP', 105).
test_case('testsets/ergo_autolykos_asic_resistance.pl', 'ergo_autolykos_asic_resistance', 'ERGO_AUTOLYKOS_ASIC_RESISTANCE', 106).
test_case('testsets/ergo_dexy_gold_protocol.pl', 'dexy_gold_interval', 'ERGO_DEXY_GOLD_PROTOCOL', 107).
test_case('testsets/ergo_lets_protocol.pl', 'ergo_lets_interval', 'ERGO_LETS_PROTOCOL', 108).
test_case('testsets/ergo_mixer_protocol.pl', 'ergo_mixer_protocol', 'ERGO_MIXER_PROTOCOL', 109).
test_case('testsets/ergo_nipopows.pl', 'ergo_nipopows', 'ERGO_NIPOPOWS', 110).
test_case('testsets/ergo_sig_usd_protocol.pl', 'sig_usd_interval', 'ERGO_SIG_USD_PROTOCOL', 111).
test_case('testsets/ergo_storage_rent_mechanism.pl', 'ergo_storage_rent_mechanism', 'ERGO_STORAGE_RENT_MECHANISM', 112).
test_case('testsets/ergodic_theorems.pl', 'ergodic_interval', 'ERGODIC_THEOREMS', 113).
test_case('testsets/euler_characteristic_topology.pl', 'euler_era', 'EULER_CHARACTERISTIC_TOPOLOGY', 114).
test_case('testsets/exoplanet_habitability_arbitrage.pl', 'exoplanetary_habitability_arbitrage', 'EXOPLANET_HABITABILITY_ARBITRAGE', 115).
test_case('testsets/extraordinary_narrative_shift.pl', 'extraordinary_narrative_shift', 'EXTRAORDINARY_NARRATIVE_SHIFT', 116).
test_case('testsets/fair_use_doctrine.pl', 'fair_use_doctrine', 'FAIR_USE_DOCTRINE', 117).
test_case('testsets/fed_shutdown_2026.pl', 'fed_shutdown_2026', 'FED_SHUTDOWN_2026', 118).
test_case('testsets/feigenbaum_universality.pl', 'feigenbaum_universality', 'FEIGENBAUM_UNIVERSALITY', 119).
test_case('testsets/fgh_hierarchy_2026.pl', 'fgh_hierarchy_2026', 'FGH_HIERARCHY_2026', 120).
test_case('testsets/finite_simple_groups_classification.pl', 'cfsg_era', 'FINITE_SIMPLE_GROUPS_CLASSIFICATION', 121).
test_case('testsets/fittss_law.pl', 'fittss_law_interval', 'FITTSS_LAW', 122).
test_case('testsets/fmeca_procedures_1980.pl', 'fmeca_standard_era', 'FMECA_PROCEDURES_1980', 123).
test_case('testsets/fmt_oncology_realignment_2026.pl', 'fmt_oncology_2026', 'FMT_ONCOLOGY_REALIGNMENT_2026', 124).
test_case('testsets/four_color_theorem_topological_bound.pl', 'four_color_era', 'FOUR_COLOR_THEOREM_TOPOLOGICAL_BOUND', 125).
test_case('testsets/france_cordon_sanitaire_2026.pl', 'france_cordon_sanitaire_2026', 'FRANCE_CORDON_SANITAIRE_2026', 126).
test_case('testsets/fundamental_theorem_of_algebra.pl', 'fta_era', 'FUNDAMENTAL_THEOREM_OF_ALGEBRA', 127).
test_case('testsets/galois_theory_symmetry.pl', 'galois_era', 'GALOIS_THEORY_SYMMETRY', 128).
test_case('testsets/gauss_bonnet_topology.pl', 'gauss_bonnet_era', 'GAUSS_BONNET_TOPOLOGY', 129).
test_case('testsets/genetic_algorithms_evolution.pl', 'genetic_algorithms_evolution', 'GENETIC_ALGORITHMS_EVOLUTION', 130).
test_case('testsets/gita_kurukshetra.pl', 'gita_kurukshetra', 'GITA_KURUKSHETRA', 131).
test_case('testsets/global_economic_anxiety_2026.pl', 'global_economic_anxiety_2026', 'GLOBAL_ECONOMIC_ANXIETY_2026', 132).
test_case('testsets/glp1_payload_efficiency_pivot.pl', 'glp1_market_impact_2026', 'GLP1_PAYLOAD_EFFICIENCY_PIVOT', 133).
test_case('testsets/godels_incompleteness_theorems.pl', 'goedel_era', 'GODELS_INCOMPLETENESS_THEOREMS', 134).
test_case('testsets/gold_piton_2026.pl', 'gold_piton_2026', 'GOLD_PITON_2026', 135).
test_case('testsets/goldbach_conjecture.pl', 'goldbach_era', 'GOLDBACH_CONJECTURE', 136).
test_case('testsets/golden_handcuffs.pl', 'golden_handcuffs', 'GOLDEN_HANDCUFFS', 137).
test_case('testsets/gradient_descent_optimization.pl', 'gradient_descent_optimization', 'GRADIENT_DESCENT_OPTIMIZATION', 138).
test_case('testsets/graph_coloring_complexity.pl', 'graph_coloring_interval', 'GRAPH_COLORING_COMPLEXITY', 139).
test_case('testsets/gs1_gln_identification.pl', 'gln_standard_lifecycle', 'GS1_GLN_IDENTIFICATION', 140).
test_case('testsets/gs1_standardized_identification.pl', 'gs1_spec_v25', 'GS1_STANDARDIZED_IDENTIFICATION', 141).
test_case('testsets/halting_problem_undecidability.pl', 'halting_era', 'HALTING_PROBLEM_UNDECIDABILITY', 142).
test_case('testsets/hamiltonian_path_complexity.pl', 'hamiltonian_path_complexity', 'HAMILTONIAN_PATH_COMPLEXITY', 143).
test_case('testsets/hawthorne_effect.pl', 'hawthorne_interval', 'HAWTHORNE_EFFECT', 144).
test_case('testsets/hegemonic_entropy_2026.pl', 'hegemonic_entropy_2026', 'HEGEMONIC_ENTROPY_2026', 145).
test_case('testsets/heisenberg_uncertainty.pl', 'heisenberg_interval', 'HEISENBERG_UNCERTAINTY', 146).
test_case('testsets/helsinki_bus_theory.pl', 'helsinki_bus_theory', 'HELSINKI_BUS_THEORY', 147).
test_case('testsets/heuristic_optimization.pl', 'heuristic_optimization', 'HEURISTIC_OPTIMIZATION', 148).
test_case('testsets/hilberts_hotel_infinity.pl', 'hilberts_hotel_infinity', 'HILBERTS_HOTEL_INFINITY', 149).
test_case('testsets/hiv_prep_prevention_2026.pl', 'hiv_prep_prevention_2026', 'HIV_PREP_PREVENTION_2026', 150).
test_case('testsets/indexical_relativity_core.pl', 'meta_logic_01', 'INDEXICAL_RELATIVITY_CORE', 151).
test_case('testsets/individual_revolution_autonomy.pl', 'individual_revolution_autonomy', 'INDIVIDUAL_REVOLUTION_AUTONOMY', 152).
test_case('testsets/information_foraging_theory.pl', 'information_foraging_theory', 'INFORMATION_FORAGING_THEORY', 153).
test_case('testsets/informational_time_2026.pl', 'informational_time_2026', 'INFORMATIONAL_TIME_2026', 154).
test_case('testsets/inner_model_theory_constraints.pl', 'inner_model_era', 'INNER_MODEL_THEORY_CONSTRAINTS', 155).
test_case('testsets/juvenile_underclass_2026.pl', 'juvenile_underclass_2026', 'JUVENILE_UNDERCLASS_2026', 156).
test_case('testsets/keltner_relationship_evaluation.pl', 'keltner_relationship_evaluation', 'KELTNER_RELATIONSHIP_EVALUATION', 157).
test_case('testsets/kidney_exchange_market.pl', 'kidney_exchange_market', 'KIDNEY_EXCHANGE_MARKET', 158).
test_case('testsets/kirby_paris_theorem.pl', 'kirby_paris_interval', 'KIRBY_PARIS_THEOREM', 159).
test_case('testsets/kjv_great_awakening.pl', 'kjv_great_awakening', 'KJV_GREAT_AWAKENING', 160).
test_case('testsets/kjv_linguistic_residue.pl', 'kjv_linguistic_residue', 'KJV_LINGUISTIC_RESIDUE', 161).
test_case('testsets/kjv_puritan_new_world_exit.pl', 'kjv_puritan_new_world_exit', 'KJV_PURITAN_NEW_WORLD_EXIT', 162).
test_case('testsets/kjv_textual_authority.pl', 'kjv_textual_authority', 'KJV_TEXTUAL_AUTHORITY', 163).
test_case('testsets/kleene_recursion_theorem.pl', 'kleene_era', 'KLEENE_RECURSION_THEOREM', 164).
test_case('testsets/landscape_of_fear_2026.pl', 'landscape_of_fear_2026', 'LANDSCAPE_OF_FEAR_2026', 165).
test_case('testsets/large_cardinals_foundations.pl', 'large_cardinal_era', 'LARGE_CARDINALS_FOUNDATIONS', 166).
test_case('testsets/law_of_diminishing_returns.pl', 'diminishing_returns_interval', 'LAW_OF_DIMINISHING_RETURNS', 167).
test_case('testsets/layered_brain_processing.pl', 'layered_brain_processing', 'LAYERED_BRAIN_PROCESSING', 168).
test_case('testsets/liar_paradox.pl', 'liar_paradox_interval', 'LIAR_PARADOX', 169).
test_case('testsets/lindy_effect.pl', 'lindy_effect', 'LINDY_EFFECT', 170).
test_case('testsets/litany_of_the_real.pl', 'litany_of_the_real', 'LITANY_OF_THE_REAL', 171).
test_case('testsets/lln_convergence.pl', 'lln_era', 'LLN_CONVERGENCE', 172).
test_case('testsets/lobs_theorem.pl', 'lobs_theorem_interval', 'LOBS_THEOREM', 173).
test_case('testsets/local_vs_global_optima.pl', 'local_vs_global_optima', 'LOCAL_VS_GLOBAL_OPTIMA', 174).
test_case('testsets/logistic_map_dynamics.pl', 'logistic_map_era', 'LOGISTIC_MAP_DYNAMICS', 175).
test_case('testsets/lorenz_attractor_dynamics.pl', 'lorenz_era', 'LORENZ_ATTRACTOR_DYNAMICS', 176).
test_case('testsets/lowenheim_skolem_theorem.pl', 'lowenheim_skolem_interval', 'LOWENHEIM_SKOLEM_THEOREM', 177).
test_case('testsets/lsd_microdosing_professional_openness.pl', 'lsd_microdosing_professional_openness', 'LSD_MICRODOSING_PROFESSIONAL_OPENNESS', 178).
test_case('testsets/magna_carta_liberties.pl', 'magna_carta_liberties', 'MAGNA_CARTA_LIBERTIES', 179).
test_case('testsets/maha_recovery_2026.pl', 'maha_recovery_2026', 'MAHA_RECOVERY_2026', 180).
test_case('testsets/manganese_catalysis_2026.pl', 'manganese_catalysis_2026', 'MANGANESE_CATALYSIS_2026', 181).
test_case('testsets/marriage_problem.pl', 'optimal_stopping_marriage', 'MARRIAGE_PROBLEM', 182).
test_case('testsets/mars_rovers_navigational_autonomy.pl', 'mars_autonomy_evolution', 'MARS_ROVERS_NAVIGATIONAL_AUTONOMY', 183).
test_case('testsets/martian_signal_latency.pl', 'mariner_to_msl_era', 'MARTIAN_SIGNAL_LATENCY', 184).
test_case('testsets/matching_markets_general.pl', 'matching_markets_general', 'MATCHING_MARKETS_GENERAL', 185).
test_case('testsets/max_flow.pl', 'max_flow_min_cut', 'MAX_FLOW', 186).
test_case('testsets/med_diet_consensus_2026.pl', 'med_diet_consensus_2026', 'MED_DIET_CONSENSUS_2026', 187).
test_case('testsets/micro_robot_electronics_integration.pl', 'ek_robot_integration_cycle', 'MICRO_ROBOT_ELECTRONICS_INTEGRATION', 188).
test_case('testsets/microbiome_symbiosis.pl', 'microbiome_symbiosis', 'MICROBIOME_SYMBIOSIS', 189).
test_case('testsets/minimax_decision_rule.pl', 'minimax_era', 'MINIMAX_DECISION_RULE', 190).
test_case('testsets/minnesota_sovereignty_2026.pl', 'minnesota_sovereignty_2026', 'MINNESOTA_SOVEREIGNTY_2026', 191).
test_case('testsets/mit_tfus_consciousness_2026.pl', 'mit_tfus_2026', 'MIT_TFUS_CONSCIOUSNESS_2026', 192).
test_case('testsets/moltbook_breach_2026.pl', 'moltbook_breach_2026', 'MOLTBOOK_BREACH_2026', 193).
test_case('testsets/mom_z14_galaxy_2026.pl', 'mom_z14_2026', 'MOM_Z14_GALAXY_2026', 194).
test_case('testsets/monty_hall_conditional_probability.pl', 'probability_paradox_era', 'MONTY_HALL_CONDITIONAL_PROBABILITY', 195).
test_case('testsets/moores_law.pl', 'moores_law_interval', 'MOORES_LAW', 196).
test_case('testsets/narcissistic_ego_maintenance.pl', 'narcissistic_ego_maintenance', 'NARCISSISTIC_EGO_MAINTENANCE', 197).
test_case('testsets/narrative_engineering_2026.pl', 'narrative_engineering_2026', 'NARRATIVE_ENGINEERING_2026', 198).
test_case('testsets/new_civilizational_rope.pl', 'decentralized_infrastructure_rope', 'NEW_CIVILIZATIONAL_ROPE', 199).
test_case('testsets/newtons_method_convergence.pl', 'newton_era', 'NEWTONS_METHOD_CONVERGENCE', 200).
test_case('testsets/no_cloning_theorem.pl', 'no_cloning_interval', 'NO_CLONING_THEOREM', 201).
test_case('testsets/noethers_theorem_symmetry.pl', 'noether_era', 'NOETHERS_THEOREM_SYMMETRY', 202).
test_case('testsets/nonstandard_arithmetic_models.pl', 'skolem_era', 'NONSTANDARD_ARITHMETIC_MODELS', 203).
test_case('testsets/omelet_perfection_complexity.pl', 'omelet_perfection_interval', 'OMELET_PERFECTION_COMPLEXITY', 204).
test_case('testsets/open_source_commons.pl', 'open_source_commons', 'OPEN_SOURCE_COMMONS', 205).
test_case('testsets/openbsd_netiquette_protocol.pl', 'openbsd_netiquette_protocol', 'OPENBSD_NETIQUETTE_PROTOCOL', 206).
test_case('testsets/other_peoples_troubles_2026.pl', 'other_peoples_troubles_2026', 'OTHER_PEOPLES_TROUBLES_2026', 207).
test_case('testsets/overton_window.pl', 'overton_interval', 'OVERTON_WINDOW', 208).
test_case('testsets/p_vs_np.pl', 'p_vs_np_interval', 'P_VS_NP', 209).
test_case('testsets/pareto_principle.pl', 'pareto_principle', 'PARETO_PRINCIPLE', 210).
test_case('testsets/peano_curve_mapping.pl', 'peano_era', 'PEANO_CURVE_MAPPING', 211).
test_case('testsets/permissive_software_licensing.pl', 'permissive_software_licensing', 'PERMISSIVE_SOFTWARE_LICENSING', 212).
test_case('testsets/planetary_diet_constraint_2026.pl', 'planetary_diet_constraint_2026', 'PLANETARY_DIET_CONSTRAINT_2026', 213).
test_case('testsets/platform_cooperativism_governance.pl', 'platform_cooperativism_governance', 'PLATFORM_COOPERATIVISM_GOVERNANCE', 214).
test_case('testsets/poincare_conjucture.pl', 'poincare_era', 'POINCARE_CONJUCTURE', 215).
test_case('testsets/postman_survival_protocol.pl', 'postman_survival_interval', 'POSTMAN_SURVIVAL_PROTOCOL', 216).
test_case('testsets/prime_number_theorem.pl', 'pnt_era', 'PRIME_NUMBER_THEOREM', 217).
test_case('testsets/private_identity_integration.pl', 'private_identity_integration', 'PRIVATE_IDENTITY_INTEGRATION', 218).
test_case('testsets/project_vault_2026.pl', 'project_vault_2026', 'PROJECT_VAULT_2026', 219).
test_case('testsets/proof_of_work_consensus.pl', 'proof_of_work_consensus', 'PROOF_OF_WORK_CONSENSUS', 220).
test_case('testsets/public_domain_commons.pl', 'public_domain_commons', 'PUBLIC_DOMAIN_COMMONS', 221).
test_case('testsets/pythagorean_theorem_geometric_constancy.pl', 'pythagorean_era', 'PYTHAGOREAN_THEOREM_GEOMETRIC_CONSTANCY', 222).
test_case('testsets/quantum_entanglement_protocol.pl', 'interstellar_quantum_link', 'QUANTUM_ENTANGLEMENT_PROTOCOL', 223).
test_case('testsets/quantum_nonlocality_2026.pl', 'quantum_nonlocality_2026', 'QUANTUM_NONLOCALITY_2026', 224).
test_case('testsets/quine_self_replication.pl', 'quine_era', 'QUINE_SELF_REPLICATION', 225).
test_case('testsets/qwerty_vs_dvorak.pl', 'qwerty_lockin_interval', 'QWERTY_VS_DVORAK', 226).
test_case('testsets/rare_earth_coop_2026.pl', 'rare_earth_coop_2026', 'RARE_EARTH_COOP_2026', 227).
test_case('testsets/reciprocity_laws_math.pl', 'reciprocity_laws_math', 'RECIPROCITY_LAWS_MATH', 228).
test_case('testsets/relativity_of_simultaneity.pl', 'relativity_of_simultaneity', 'RELATIVITY_OF_SIMULTANEITY', 229).
test_case('testsets/relativity_physical_invariance.pl', 'relativity_physical_invariance', 'RELATIVITY_PHYSICAL_INVARIANCE', 230).
test_case('testsets/rfc9293_interoperability.pl', 'tcp_rfc9293_interoperability', 'RFC9293_INTEROPERABILITY', 231).
test_case('testsets/rfc9293_state_machine.pl', 'rfc9293_state_machine', 'RFC9293_STATE_MACHINE', 232).
test_case('testsets/rices_theorem_undecidability.pl', 'rices_era', 'RICES_THEOREM_UNDECIDABILITY', 233).
test_case('testsets/riot_incentive_loop_2026.pl', 'riot_incentive_loop_2026', 'RIOT_INCENTIVE_LOOP_2026', 234).
test_case('testsets/rogue_wave_control_2026.pl', 'rogue_wave_control_2026', 'ROGUE_WAVE_CONTROL_2026', 235).
test_case('testsets/rotmigration_decision_threshold.pl', 'migration_theory_review', 'ROTMIGRATION_DECISION_THRESHOLD', 236).
test_case('testsets/s1_visa.pl', 'visa_ipo_window', 'S1_VISA', 237).
test_case('testsets/s1_visa_judgment_sharing_agreement.pl', 'visa_litigation_ringfencing', 'S1_VISA_JUDGMENT_SHARING_AGREEMENT', 238).
test_case('testsets/sadhu_integrity_protocol.pl', 'sadhu_integrity_protocol', 'SADHU_INTEGRITY_PROTOCOL', 239).
test_case('testsets/sat_csp_complexity.pl', 'sat_csp_interval', 'SAT_CSP_COMPLEXITY', 240).
test_case('testsets/shannon_entropy_limit.pl', 'shannon_entropy_limit', 'SHANNON_ENTROPY_LIMIT', 241).
test_case('testsets/shitty_feedback_handling.pl', 'shitty_feedback_handling', 'SHITTY_FEEDBACK_HANDLING', 242).
test_case('testsets/silver_scarcity_mountain_2026.pl', 'silver_scarcity_2026', 'SILVER_SCARCITY_MOUNTAIN_2026', 243).
test_case('testsets/skills_based_hiring.pl', 'skills_based_hiring', 'SKILLS_BASED_HIRING', 244).
test_case('testsets/skolems_paradox.pl', 'skolems_paradox_interval', 'SKOLEMS_PARADOX', 245).
test_case('testsets/social_narrative_casting.pl', 'social_narrative_casting', 'SOCIAL_NARRATIVE_CASTING', 246).
test_case('testsets/solar_system_weirdness.pl', 'solar_system_weirdness', 'SOLAR_SYSTEM_WEIRDNESS', 247).
test_case('testsets/somatic_focusing_awareness.pl', 'somatic_focusing_awareness', 'SOMATIC_FOCUSING_AWARENESS', 248).
test_case('testsets/sorites_paradox.pl', 'sorites_interval', 'SORITES_PARADOX', 249).
test_case('testsets/south_china_sea_arbitration_2016_2026.pl', 'scs_legal_era', 'SOUTH_CHINA_SEA_ARBITRATION_2016_2026', 250).
test_case('testsets/stable_marriage_coordination.pl', 'matching_theory_era', 'STABLE_MARRIAGE_COORDINATION', 251).
test_case('testsets/strange_attractor_dynamics.pl', 'strange_attractor_dynamics', 'STRANGE_ATTRACTOR_DYNAMICS', 252).
test_case('testsets/sts86_ascent_checklist.pl', 'sts86_ascent_checklist', 'STS86_ASCENT_CHECKLIST', 253).
test_case('testsets/sturgeons_law.pl', 'sturgeon_interval', 'STURGEONS_LAW', 254).
test_case('testsets/suslin_hypothesis_undecidability.pl', 'suslin_era', 'SUSLIN_HYPOTHESIS_UNDECIDABILITY', 255).
test_case('testsets/sylow_theorems_group_theory.pl', 'sylow_era', 'SYLOW_THEOREMS_GROUP_THEORY', 256).
test_case('testsets/tarski_undefinability.pl', 'tarski_interval', 'TARSKI_UNDEFINABILITY', 257).
test_case('testsets/thai_article_112_mountain.pl', 'thai_article_112_mountain', 'THAI_ARTICLE_112_MOUNTAIN', 258).
test_case('testsets/three_body_unpredicability.pl', 'celestial_mechanics_era', 'THREE_BODY_UNPREDICABILITY', 259).
test_case('testsets/trade_secret_law.pl', 'trade_secret_law', 'TRADE_SECRET_LAW', 260).
test_case('testsets/transient_event_detection.pl', 'transient_event_detection', 'TRANSIENT_EVENT_DETECTION', 261).
test_case('testsets/traveling_salesperson_problem.pl', 'traveling_salesperson_problem', 'TRAVELING_SALESPERSON_PROBLEM', 262).
test_case('testsets/udhr_1946.pl', 'un_era', 'UDHR_1946', 263).
test_case('testsets/ulysses_chp01.pl', 'ulysses_tower_1904', 'ULYSSES_CHP01', 264).
test_case('testsets/ulysses_chp02.pl', 'ulysses_school_1904', 'ULYSSES_CHP02', 265).
test_case('testsets/ulysses_chp03.pl', 'ulysses_proteus_1904', 'ULYSSES_CHP03', 266).
test_case('testsets/ulysses_chp04.pl', 'ulysses_calypso_1904', 'ULYSSES_CHP04', 267).
test_case('testsets/ulysses_chp05.pl', 'ulysses_lotus_1904', 'ULYSSES_CHP05', 268).
test_case('testsets/ulysses_chp06.pl', 'ulysses_hades_1904', 'ULYSSES_CHP06', 269).
test_case('testsets/ulysses_chp07.pl', 'ulysses_aeolus_1904', 'ULYSSES_CHP07', 270).
test_case('testsets/ulysses_chp08.pl', 'ulysses_lestrygonians_1904', 'ULYSSES_CHP08', 271).
test_case('testsets/ulysses_chp09.pl', 'ulysses_scylla_1904', 'ULYSSES_CHP09', 272).
test_case('testsets/ulysses_chp10.pl', 'ulysses_rocks_1904', 'ULYSSES_CHP10', 273).
test_case('testsets/ulysses_chp11.pl', 'ulysses_sirens_1904', 'ULYSSES_CHP11', 274).
test_case('testsets/ulysses_chp12.pl', 'ulysses_cyclops_1904', 'ULYSSES_CHP12', 275).
test_case('testsets/ulysses_chp13.pl', 'ulysses_nausicaa_1904', 'ULYSSES_CHP13', 276).
test_case('testsets/ulysses_chp14.pl', 'ulysses_oxen_1904', 'ULYSSES_CHP14', 277).
test_case('testsets/ulysses_chp15.pl', 'ulysses_circe_1904', 'ULYSSES_CHP15', 278).
test_case('testsets/ulysses_chp16.pl', 'ulysses_eumaeus_1904', 'ULYSSES_CHP16', 279).
test_case('testsets/ulysses_chp17.pl', 'ulysses_ithaca_1904', 'ULYSSES_CHP17', 280).
test_case('testsets/ulysses_chp18.pl', 'ulysses_penelope_1904', 'ULYSSES_CHP18', 281).
test_case('testsets/unclos_2026.pl', 'unclos_era', 'UNCLOS_2026', 282).
test_case('testsets/universal_mathematics_communication.pl', 'scientific_consensus_period', 'UNIVERSAL_MATHEMATICS_COMMUNICATION', 283).
test_case('testsets/value_alignment_drift.pl', 'value_alignment_drift', 'VALUE_ALIGNMENT_DRIFT', 284).
test_case('testsets/van_der_waerden_theorem.pl', 'van_der_waerden_interval', 'VAN_DER_WAERDEN_THEOREM', 285).
test_case('testsets/vertebrate_turning_point_2026.pl', 'vertebrate_genetics_2026', 'VERTEBRATE_TURNING_POINT_2026', 286).
test_case('testsets/vienna_quantum_superposition_2026.pl', 'vienna_quantum_superposition_2026', 'VIENNA_QUANTUM_SUPERPOSITION_2026', 287).
test_case('testsets/viral_transmission_rates.pl', 'viral_transmission_rates', 'VIRAL_TRANSMISSION_RATES', 288).
test_case('testsets/visibility_bias_governance.pl', 'visibility_bias_governance', 'VISIBILITY_BIAS_GOVERNANCE', 289).
test_case('testsets/whitehead_problem_undecidability.pl', 'whitehead_era', 'WHITEHEAD_PROBLEM_UNDECIDABILITY', 290).
test_case('testsets/wikipedia_crowdsourcing_2026.pl', 'wikipedia_crowdsourcing_2026', 'WIKIPEDIA_CROWDSOURCING_2026', 291).
test_case('testsets/wikipedia_notability_requirement_2026.pl', 'wikipedia_notability_requirement_2026', 'WIKIPEDIA_NOTABILITY_REQUIREMENT_2026', 292).
test_case('testsets/winners_curse.pl', 'winners_curse', 'WINNERS_CURSE', 293).
test_case('testsets/winter_olympics_2026.pl', 'milano_cortina_2026', 'WINTER_OLYMPICS_2026', 294).
test_case('testsets/xi_mao_ideological_centralization.pl', 'xi_mao_ideological_centralization', 'XI_MAO_IDEOLOGICAL_CENTRALIZATION', 295).
test_case('testsets/zipfs_law.pl', 'zipfs_law', 'ZIPFS_LAW', 296).
test_case('testsets/zombie_reasoning_2026.pl', 'zombie_reasoning_2026', 'ZOMBIE_REASONING_2026', 297).

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
run_single_test(Path, ID, _Label, N) :-
    format('~n[~w] EXECUTING: ~w~n', [N, Path]),
    catch_with_backtrace(
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
    report_generator:generate_llm_feedback(ID).

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

