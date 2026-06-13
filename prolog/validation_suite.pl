:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/actinide_replenishment_mechanism_contradictions.pl', 'unknown_interval', 'ACTINIDE_REPLENISHMENT_MECHANISM_CONTRADICTIONS', 1).
test_case('testsets/actinide_replenishment_mechanism_flat_control.pl', 'actinide_replenishment_mechanism_flat_control', 'ACTINIDE_REPLENISHMENT_MECHANISM_FLAT_CONTROL', 2).
test_case('testsets/adjunctification_of_university_teaching_c0.pl', 'adjunctification_of_university_teaching_c0', 'ADJUNCTIFICATION_OF_UNIVERSITY_TEACHING_C0', 3).
test_case('testsets/animal_status_kernel__property_reading.pl', 'animal_status_kernel__property_reading', 'ANIMAL_STATUS_KERNEL__PROPERTY_READING', 4).
test_case('testsets/basic_law_interpretive_authority__parliamentary_sovereignty_reading.pl', 'basic_law_interpretive_authority__parliamentary_sovereignty_reading', 'BASIC_LAW_INTERPRETIVE_AUTHORITY__PARLIAMENTARY_SOVEREIGNTY_READING', 5).
test_case('testsets/basic_law_interpretive_boundary__parliamentary_sovereignty_reading.pl', 'basic_law_interpretive_boundary__parliamentary_sovereignty_reading', 'BASIC_LAW_INTERPRETIVE_BOUNDARY__PARLIAMENTARY_SOVEREIGNTY_READING', 6).
test_case('testsets/bitcoin_whitepaper_purpose__nakamoto_oracle_opacity.pl', 'bitcoin_whitepaper_purpose__nakamoto_oracle_opacity', 'BITCOIN_WHITEPAPER_PURPOSE__NAKAMOTO_ORACLE_OPACITY', 7).
test_case('testsets/border_control_legitimacy__freedom_of_movement_primary.pl', 'border_control_legitimacy__freedom_of_movement_primary', 'BORDER_CONTROL_LEGITIMACY__FREEDOM_OF_MOVEMENT_PRIMARY', 8).
test_case('testsets/catastrophe_memory_kernel__boundary_maintenance_reading.pl', 'catastrophe_memory_kernel__boundary_maintenance_reading', 'CATASTROPHE_MEMORY_KERNEL__BOUNDARY_MAINTENANCE_READING', 9).
test_case('testsets/demographic_resource_allocation.pl', 'demographic_resource_allocation', 'DEMOGRAPHIC_RESOURCE_ALLOCATION', 10).
test_case('testsets/demographic_skill_mismatch_c0.pl', 'demographic_skill_mismatch_c0', 'DEMOGRAPHIC_SKILL_MISMATCH_C0', 11).
test_case('testsets/divine_legitimacy_substrate__folk_syncretistic_reading.pl', 'divine_legitimacy_substrate__folk_syncretistic_reading', 'DIVINE_LEGITIMACY_SUBSTRATE__FOLK_SYNCRETISTIC_READING', 12).
test_case('testsets/doomsday_clock_metric__hybrid_legitimacy_reading.pl', 'doomsday_clock_metric__hybrid_legitimacy_reading', 'DOOMSDAY_CLOCK_METRIC__HYBRID_LEGITIMACY_READING', 13).
test_case('testsets/dueling_disappearance_mechanism__overdetermined_composite_reading.pl', 'dueling_disappearance_mechanism__overdetermined_composite_reading', 'DUELING_DISAPPEARANCE_MECHANISM__OVERDETERMINED_COMPOSITE_READING', 14).
test_case('testsets/equal_protection_kernel__colorblind_reading.pl', 'equal_protection_kernel__colorblind_reading', 'EQUAL_PROTECTION_KERNEL__COLORBLIND_READING', 15).
test_case('testsets/fourteenth_amendment_equal_protection__formal_equality_reading.pl', 'fourteenth_amendment_equal_protection__formal_equality_reading', 'FOURTEENTH_AMENDMENT_EQUAL_PROTECTION__FORMAL_EQUALITY_READING', 16).
test_case('testsets/gita_kurukshetra_discourse__gandhian_allegorical_reading.pl', 'gita_kurukshetra_discourse__gandhian_allegorical_reading', 'GITA_KURUKSHETRA_DISCOURSE__GANDHIAN_ALLEGORICAL_READING', 17).
test_case('testsets/historical_treaty_substrate__stewardship_reading.pl', 'historical_treaty_substrate__stewardship_reading', 'HISTORICAL_TREATY_SUBSTRATE__STEWARDSHIP_READING', 18).
test_case('testsets/institutional_trust_erosion_c0.pl', 'institutional_trust_erosion_c0', 'INSTITUTIONAL_TRUST_EROSION_C0', 19).
test_case('testsets/jewish_self_determination__indigenous_return_reading.pl', 'jewish_self_determination__indigenous_return_reading', 'JEWISH_SELF_DETERMINATION__INDIGENOUS_RETURN_READING', 20).
test_case('testsets/jewish_sovereignty_palestine__cultural_zionist_reading.pl', 'jewish_sovereignty_palestine__cultural_zionist_reading', 'JEWISH_SOVEREIGNTY_PALESTINE__CULTURAL_ZIONIST_READING', 21).
test_case('testsets/jewish_sovereignty_palestine__settler_colonial_reading.pl', 'jewish_sovereignty_palestine__settler_colonial_reading', 'JEWISH_SOVEREIGNTY_PALESTINE__SETTLER_COLONIAL_READING', 22).
test_case('testsets/jewish_territorial_claim__labor_zionism_reading.pl', 'jewish_territorial_claim__labor_zionism_reading', 'JEWISH_TERRITORIAL_CLAIM__LABOR_ZIONISM_READING', 23).
test_case('testsets/john_1_1_logos__non_incarnational_monotheist.pl', 'john_1_1_logos__non_incarnational_monotheist', 'JOHN_1_1_LOGOS__NON_INCARNATIONAL_MONOTHEIST', 24).
test_case('testsets/lausanne_minority_protections__guarantor_reading.pl', 'lausanne_minority_protections__guarantor_reading', 'LAUSANNE_MINORITY_PROTECTIONS__GUARANTOR_READING', 25).
test_case('testsets/livelihood_security_reading.pl', 'livelihood_security_reading', 'LIVELIHOOD_SECURITY_READING', 26).
test_case('testsets/lycurgan_laws__demographic_trap_reading.pl', 'lycurgan_laws__demographic_trap_reading', 'LYCURGAN_LAWS__DEMOGRAPHIC_TRAP_READING', 27).
test_case('testsets/maat_order_principle__reciprocity_reading.pl', 'maat_order_principle__reciprocity_reading', 'MAAT_ORDER_PRINCIPLE__RECIPROCITY_READING', 28).
test_case('testsets/marriage_authority__judicial_harmonization_reading.pl', 'marriage_authority__judicial_harmonization_reading', 'MARRIAGE_AUTHORITY__JUDICIAL_HARMONIZATION_READING', 29).
test_case('testsets/neutron_star_bombardment_reading.pl', 'neutron_star_bombardment_reading', 'NEUTRON_STAR_BOMBARDMENT_READING', 30).
test_case('testsets/nicene_creed_authority__liturgical_habituation_reading.pl', 'nicene_creed_authority__liturgical_habituation_reading', 'NICENE_CREED_AUTHORITY__LITURGICAL_HABITUATION_READING', 31).
test_case('testsets/organization_floor_c0.pl', 'organization_floor_c0', 'ORGANIZATION_FLOOR_C0', 32).
test_case('testsets/performance_legitimacy_contradictions.pl', 'unknown_interval', 'PERFORMANCE_LEGITIMACY_CONTRADICTIONS', 33).
test_case('testsets/performance_legitimacy_flat_control.pl', 'performance_legitimacy_flat_control', 'PERFORMANCE_LEGITIMACY_FLAT_CONTROL', 34).
test_case('testsets/press_reformation_causality__strategic_deployment.pl', 'press_reformation_causality__strategic_deployment', 'PRESS_REFORMATION_CAUSALITY__STRATEGIC_DEPLOYMENT', 35).
test_case('testsets/press_reformation_causation__mutual_shaping.pl', 'press_reformation_causation__mutual_shaping', 'PRESS_REFORMATION_CAUSATION__MUTUAL_SHAPING', 36).
test_case('testsets/press_reformation_causation__strategic_deployment.pl', 'press_reformation_causation__strategic_deployment', 'PRESS_REFORMATION_CAUSATION__STRATEGIC_DEPLOYMENT', 37).
test_case('testsets/property_sector_overhang.pl', 'property_sector_overhang', 'PROPERTY_SECTOR_OVERHANG', 38).
test_case('testsets/qualitative_development_reading.pl', 'qualitative_development_reading', 'QUALITATIVE_DEVELOPMENT_READING', 39).
test_case('testsets/quantitative_growth_reading.pl', 'quantitative_growth_reading', 'QUANTITATIVE_GROWTH_READING', 40).
test_case('testsets/radiative_levitation_stratification.pl', 'radiative_levitation_stratification', 'RADIATIVE_LEVITATION_STRATIFICATION', 41).
test_case('testsets/reading_acquisition_legitimacy__whole_language_meaning_primacy.pl', 'reading_acquisition_legitimacy__whole_language_meaning_primacy', 'READING_ACQUISITION_LEGITIMACY__WHOLE_LANGUAGE_MEANING_PRIMACY', 42).
test_case('testsets/refugee_convention_text__expansive_humanitarian_reading.pl', 'refugee_convention_text__expansive_humanitarian_reading', 'REFUGEE_CONVENTION_TEXT__EXPANSIVE_HUMANITARIAN_READING', 43).
test_case('testsets/scale_ceiling_c0.pl', 'scale_ceiling_c0', 'SCALE_CEILING_C0', 44).
test_case('testsets/secession_legitimacy_boundary__constitutional_impossibility_reading.pl', 'secession_legitimacy_boundary__constitutional_impossibility_reading', 'SECESSION_LEGITIMACY_BOUNDARY__CONSTITUTIONAL_IMPOSSIBILITY_READING', 45).
test_case('testsets/sex_gender_category__identity_reading.pl', 'sex_gender_category__identity_reading', 'SEX_GENDER_CATEGORY__IDENTITY_READING', 46).
test_case('testsets/shinbutsu_ontological_commitment__incoherence_reading.pl', 'shinbutsu_ontological_commitment__incoherence_reading', 'SHINBUTSU_ONTOLOGICAL_COMMITMENT__INCOHERENCE_READING', 47).
test_case('testsets/speech_protection_kernel__absolutist_reading.pl', 'speech_protection_kernel__absolutist_reading', 'SPEECH_PROTECTION_KERNEL__ABSOLUTIST_READING', 48).
test_case('testsets/state_killing_legitimacy__abolition_reading.pl', 'state_killing_legitimacy__abolition_reading', 'STATE_KILLING_LEGITIMACY__ABOLITION_READING', 49).
test_case('testsets/statutory_debt_ceiling__coordination_scaffold_reading.pl', 'statutory_debt_ceiling__coordination_scaffold_reading', 'STATUTORY_DEBT_CEILING__COORDINATION_SCAFFOLD_READING', 50).
test_case('testsets/superheavy_decay_reading.pl', 'superheavy_decay_reading', 'SUPERHEAVY_DECAY_READING', 51).
test_case('testsets/techno_nationalist_reading.pl', 'techno_nationalist_reading', 'TECHNO_NATIONALIST_READING', 52).
test_case('testsets/udhr_article_3__negative_liberty_reading.pl', 'udhr_article_3__negative_liberty_reading', 'UDHR_ARTICLE_3__NEGATIVE_LIBERTY_READING', 53).
test_case('testsets/vedic_corpus_social_prescription__colonial_orientalist_reading.pl', 'vedic_corpus_social_prescription__colonial_orientalist_reading', 'VEDIC_CORPUS_SOCIAL_PRESCRIPTION__COLONIAL_ORIENTALIST_READING', 54).
test_case('testsets/zero_mathematical_status__parmenidean_rejection.pl', 'zero_mathematical_status__parmenidean_rejection', 'ZERO_MATHEMATICAL_STATUS__PARMENIDEAN_REJECTION', 55).
test_case('testsets/zero_mathematical_status__placeholder_reading.pl', 'zero_mathematical_status__placeholder_reading', 'ZERO_MATHEMATICAL_STATUS__PLACEHOLDER_READING', 56).
test_case('testsets/zionist_legitimacy_basis__national_liberation_reading.pl', 'zionist_legitimacy_basis__national_liberation_reading', 'ZIONIST_LEGITIMACY_BASIS__NATIONAL_LIBERATION_READING', 57).

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

