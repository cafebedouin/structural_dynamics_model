:- use_module(scenario_manager).
:- dynamic test_passed/1.
:- dynamic test_failed/2.

run_dynamic_suite :-
    retractall(test_passed(_)),
    retractall(test_failed(_, _)),
    writeln('--- STARTING DYNAMIC VALIDATION ---'),
    test_file('./datasets/26usc469_data.pl', 'tra86_regulatory_cycle', '26USC469', 1),
    test_file('./datasets/asce_7_data.pl', 'design_compliance_cycle', 'ASCE_7', 2),
    test_file('./datasets/astm_d638_22_data.pl', 'tensile_test_procedure', 'ASTM_D638_22', 3),
    test_file('./datasets/axiom_001_core_data.pl', 'diagonalization_cycle', 'AXIOM_001_CORE', 4),
    test_file('./datasets/axiom_006_core_data.pl', 'axiom_contraction_cycle', 'AXIOM_006_CORE', 5),
    test_file('./datasets/axiom_112_core_data.pl', 'wandering_convergence_01', 'AXIOM_112_CORE', 6),
    test_file('./datasets/bay_of_pigs_data.pl', 'bay_of_pigs_invasion', 'BAY_OF_PIGS', 7),
    test_file('./datasets/bhagavad_gita_data.pl', 'kurukshetra_dialogue', 'BHAGAVAD_GITA', 8),
    test_file('./datasets/cytokine_storm_data.pl', 'cytokine_storm_cycle', 'CYTOKINE_STORM', 9),
    test_file('./datasets/dna_watson_crick_data.pl', 'discovery_validation_period', 'DNA_WATSON_CRICK', 10),
    test_file('./datasets/einsteins_relativity_data.pl', 'relativity_transition', 'EINSTEINS_RELATIVITY', 11),
    test_file('./datasets/encryption_data.pl', 'encryption_regulation_epoch', 'ENCRYPTION', 12),
    test_file('./datasets/end_of_usd_reserve_data.pl', 'dollar_hegemony_collapse', 'END_OF_USD_RESERVE', 13),
    test_file('./datasets/faint_blue_data.pl', 'faint_blue_cycle', 'FAINT_BLUE', 14),
    test_file('./datasets/fourth_french_republic_data.pl', 'may_1958_crisis', 'FOURTH_FRENCH_REPUBLIC', 15),
    test_file('./datasets/gift_of_the_magi_data.pl', 'christmas_sacrifice_interval', 'GIFT_OF_THE_MAGI', 16),
    test_file('./datasets/gs_1_barcodes_data.pl', 'gs1_standardization_cycle', 'GS_1_BARCODES', 17),
    test_file('./datasets/halting_problem_data.pl', 'i1', 'HALTING_PROBLEM', 18),
    test_file('./datasets/hammurabi_data.pl', 'hammurabi_codification', 'HAMMURABI', 19),
    test_file('./datasets/information_theory_data.pl', 'information_revolution', 'INFORMATION_THEORY', 20),
    test_file('./datasets/iran_khamenei_succession_data.pl', 'iran_succession_horizon', 'IRAN_KHAMENEI_SUCCESSION', 21),
    test_file('./datasets/late_bronze_age_collapse_data.pl', 'lba_collapse_cycle', 'LATE_BRONZE_AGE_COLLAPSE', 22),
    test_file('./datasets/lehman_data.pl', 'lehman_collapse_cycle', 'LEHMAN', 23),
    test_file('./datasets/mars_climate_orbiter_data.pl', 'mco_mission_duration', 'MARS_CLIMATE_ORBITER', 24),
    test_file('./datasets/maupassant_necklace_data.pl', 'debt_cycle_1880', 'MAUPASSANT_NECKLACE', 25),
    test_file('./datasets/military_venezuela_data.pl', 'intervention_pressure_cycle', 'MILITARY_VENEZUELA', 26),
    test_file('./datasets/pharma_patents_data.pl', 'trips_standardization_cycle', 'PHARMA_PATENTS', 27),
    test_file('./datasets/rfc9293_data.pl', 'tcp_connection_lifecycle', 'RFC9293', 28),
    test_file('./datasets/rime_data.pl', 'voyage_of_penance', 'RIME', 29),
    test_file('./datasets/roman_empire_decline_data.pl', 'roman_decline_and_fall', 'ROMAN_EMPIRE_DECLINE', 30),
    test_file('./datasets/rotation_seven_data.pl', 'rot7_cycle', 'ROTATION_SEVEN', 31),
    test_file('./datasets/ssrn_1105657_data.pl', 'constitutional_review_period', 'SSRN_1105657', 32),
    test_file('./datasets/sts86_ascent_data.pl', 'ascent_to_orbit_cycle', 'STS86_ASCENT', 33),
    test_file('./datasets/tell_tale_heart_data.pl', 'the_guilt_cycle', 'TELL_TALE_HEART', 34),
    test_file('./datasets/tenant_protections_data.pl', 'rent_regulation_cycle', 'TENANT_PROTECTIONS', 35),
    test_file('./datasets/tfr83_economic_security_data.pl', 'economic_adjustment_cycle', 'TFR83_ECONOMIC_SECURITY', 36),
    test_file('./datasets/the_calm_data.pl', 'antarctic_traverse', 'THE_CALM', 37),
    test_file('./datasets/to_build_a_fire_data.pl', 'the_survival_arc', 'TO_BUILD_A_FIRE', 38),
    test_file('./datasets/us_civil_war_data.pl', 'us_civil_war', 'US_CIVIL_WAR', 39),
    count_and_report.

test_file(Path, ID, Label, N) :-
    format('~n[~w] DOMAIN: ~w (~w)~n', [N, Label, Path]),
    catch((run_scenario(Path, ID), assertz(test_passed(Path))), E, (assertz(test_failed(Path, E)), format('[FAIL] ~w~n', [E]))).

count_and_report :-
    findall(P, test_passed(P), Ps), length(Ps, PC), findall(F, test_failed(F,_), Fs), length(Fs, FC),
    format('~nDONE: ~w Passed, ~w Failed~n', [PC, FC]).
