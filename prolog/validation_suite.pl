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
test_case('testsets/antisubordination_reading.pl', 'antisubordination_reading', 'ANTISUBORDINATION_READING', 5).
test_case('testsets/became_thinkable_reading.pl', 'became_thinkable_reading', 'BECAME_THINKABLE_READING', 6).
test_case('testsets/behavioral_competence_reading.pl', 'behavioral_competence_reading', 'BEHAVIORAL_COMPETENCE_READING', 7).
test_case('testsets/beneficiary_agency_reading.pl', '0', 'BENEFICIARY_AGENCY_READING', 8).
test_case('testsets/beneficiary_maintained_reading.pl', 'beneficiary_maintained_reading', 'BENEFICIARY_MAINTAINED_READING', 9).
test_case('testsets/biology_reading.pl', 'biology_reading', 'BIOLOGY_READING', 10).
test_case('testsets/birth_reading.pl', 'birth_reading', 'BIRTH_READING', 11).
test_case('testsets/birth_threshold_reading.pl', 'birth_threshold_reading', 'BIRTH_THRESHOLD_READING', 12).
test_case('testsets/bodily_autonomy_primary.pl', 'bodily_autonomy_primary', 'BODILY_AUTONOMY_PRIMARY', 13).
test_case('testsets/catastrophe_as_necessary.pl', 'catastrophe_as_necessary', 'CATASTROPHE_AS_NECESSARY', 14).
test_case('testsets/catastrophic_tail_dominant.pl', 'catastrophic_tail_dominant', 'CATASTROPHIC_TAIL_DOMINANT', 15).
test_case('testsets/civic_right_reading.pl', '0', 'CIVIC_RIGHT_READING', 16).
test_case('testsets/co_constitution_reading.pl', '0', 'CO_CONSTITUTION_READING', 17).
test_case('testsets/collective_right_reading.pl', 'collective_right_reading', 'COLLECTIVE_RIGHT_READING', 18).
test_case('testsets/colorblind_reading.pl', 'colorblind_reading', 'COLORBLIND_READING', 19).
test_case('testsets/commemorative_husk_reading.pl', 'commemorative_husk_reading', 'COMMEMORATIVE_HUSK_READING', 20).
test_case('testsets/communal_autonomy_reading.pl', 'communal_autonomy_reading', 'COMMUNAL_AUTONOMY_READING', 21).
test_case('testsets/competence_reading.pl', '0', 'COMPETENCE_READING', 22).
test_case('testsets/constitutional_supremacy_reading.pl', '0', 'CONSTITUTIONAL_SUPREMACY_READING', 23).
test_case('testsets/continuationist_reading.pl', 'continuationist_reading', 'CONTINUATIONIST_READING', 24).
test_case('testsets/continuity_reading.pl', '0', 'CONTINUITY_READING', 25).
test_case('testsets/contraction_reading.pl', 'contraction_reading', 'CONTRACTION_READING', 26).
test_case('testsets/convertibility_constraint_removal.pl', 'convertibility_constraint_removal', 'CONVERTIBILITY_CONSTRAINT_REMOVAL', 27).
test_case('testsets/coordination_lock_in.pl', 'coordination_lock_in', 'COORDINATION_LOCK_IN', 28).
test_case('testsets/decentralization_sovereignty_commitment.pl', 'decentralization_sovereignty_commitment', 'DECENTRALIZATION_SOVEREIGNTY_COMMITMENT', 29).
test_case('testsets/deterrence_reading.pl', 'deterrence_reading', 'DETERRENCE_READING', 30).
test_case('testsets/domain_partition_reading.pl', 'domain_partition_reading', 'DOMAIN_PARTITION_READING', 31).
test_case('testsets/drop_confessional_instantiation.pl', 'drop_confessional_instantiation', 'DROP_CONFESSIONAL_INSTANTIATION', 32).
test_case('testsets/dual_priority_reading.pl', 'dual_priority_reading', 'DUAL_PRIORITY_READING', 33).
test_case('testsets/ecumenical_boundary_shift.pl', 'ecumenical_boundary_shift', 'ECUMENICAL_BOUNDARY_SHIFT', 34).
test_case('testsets/episcopal_collegiality_tension.pl', 'episcopal_collegiality_tension', 'EPISCOPAL_COLLEGIALITY_TENSION', 35).
test_case('testsets/expected_value_dominant.pl', 'expected_value_dominant', 'EXPECTED_VALUE_DOMINANT', 36).
test_case('testsets/first_held_reading.pl', 'first_held_reading', 'FIRST_HELD_READING', 37).
test_case('testsets/fork_diffusion_status_signal.pl', 'fork_diffusion_status_signal', 'FORK_DIFFUSION_STATUS_SIGNAL', 38).
test_case('testsets/freedom_floor_reading.pl', 'freedom_floor_reading', 'FREEDOM_FLOOR_READING', 39).
test_case('testsets/freedom_of_movement_reading.pl', 'freedom_of_movement_reading', 'FREEDOM_OF_MOVEMENT_READING', 40).
test_case('testsets/hanafi_reading.pl', 'hanafi_reading', 'HANAFI_READING', 41).
test_case('testsets/hanbali_reading.pl', 'hanbali_reading', 'HANBALI_READING', 42).
test_case('testsets/harm_threshold_reading.pl', '0', 'HARM_THRESHOLD_READING', 43).
test_case('testsets/husk_reading.pl', 'husk_reading', 'HUSK_READING', 44).
test_case('testsets/hybrid_atrophy_reading.pl', 'hybrid_atrophy_reading', 'HYBRID_ATROPHY_READING', 45).
test_case('testsets/hybrid_reading.pl', 'hybrid_reading', 'HYBRID_READING', 46).
test_case('testsets/individual_right_reading.pl', 'individual_right_reading', 'INDIVIDUAL_RIGHT_READING', 47).
test_case('testsets/inflation_credibility_constraint.pl', 'inflation_credibility_constraint', 'INFLATION_CREDIBILITY_CONSTRAINT', 48).
test_case('testsets/integration_primary.pl', 'integration_primary', 'INTEGRATION_PRIMARY', 49).
test_case('testsets/legalization_reading.pl', '0', 'LEGALIZATION_READING', 50).
test_case('testsets/literary_revival_reading.pl', 'literary_revival_reading', 'LITERARY_REVIVAL_READING', 51).
test_case('testsets/liturgical_continuity_reading.pl', 'liturgical_continuity_reading', 'LITURGICAL_CONTINUITY_READING', 52).
test_case('testsets/liturgical_vernacularization.pl', 'liturgical_vernacularization', 'LITURGICAL_VERNACULARIZATION', 53).
test_case('testsets/living_constitutionalist_reading.pl', 'living_constitutionalist_reading', 'LIVING_CONSTITUTIONALIST_READING', 54).
test_case('testsets/mixed_constitutional_reading.pl', 'mixed_constitutional_reading', 'MIXED_CONSTITUTIONAL_READING', 55).
test_case('testsets/monarchical_reading.pl', 'monarchical_reading', 'MONARCHICAL_READING', 56).
test_case('testsets/monetary_discretion_expansion.pl', 'monetary_discretion_expansion', 'MONETARY_DISCRETION_EXPANSION', 57).
test_case('testsets/monopoly_transfer_mechanism.pl', 'monopoly_transfer_mechanism', 'MONOPOLY_TRANSFER_MECHANISM', 58).
test_case('testsets/mourning_practice_reading.pl', 'mourning_practice_reading', 'MOURNING_PRACTICE_READING', 59).
test_case('testsets/muslim_uncodified_reading.pl', 'muslim_uncodified_reading', 'MUSLIM_UNCODIFIED_READING', 60).
test_case('testsets/native_generation.pl', '0', 'NATIVE_GENERATION', 61).
test_case('testsets/native_generation_reading.pl', 'native_generation_reading', 'NATIVE_GENERATION_READING', 62).
test_case('testsets/near_miss_as_bridge.pl', 'near_miss_as_bridge', 'NEAR_MISS_AS_BRIDGE', 63).
test_case('testsets/option_value_preserving.pl', 'option_value_preserving', 'OPTION_VALUE_PRESERVING', 64).
test_case('testsets/originalist_reading.pl', 'originalist_reading', 'ORIGINALIST_READING', 65).
test_case('testsets/partition_reading.pl', 'partition_reading', 'PARTITION_READING', 66).
test_case('testsets/phonetic_script_mismatch.pl', 'phonetic_script_mismatch', 'PHONETIC_SCRIPT_MISMATCH', 67).
test_case('testsets/positional_notation_dependency.pl', 'positional_notation_dependency', 'POSITIONAL_NOTATION_DEPENDENCY', 68).
test_case('testsets/pragmatic_incoherence_reading.pl', 'pragmatic_incoherence_reading', 'PRAGMATIC_INCOHERENCE_READING', 69).
test_case('testsets/prohibition_reading.pl', 'prohibition_reading', 'PROHIBITION_READING', 70).
test_case('testsets/public_health_primary.pl', 'from', 'PUBLIC_HEALTH_PRIMARY', 71).
test_case('testsets/pure_property.pl', 'pure_property', 'PURE_PROPERTY', 72).
test_case('testsets/reachability_contraction_vs_probability_drop.pl', 'reachability_contraction_vs_probability_drop', 'REACHABILITY_CONTRACTION_VS_PROBABILITY_DROP', 73).
test_case('testsets/reconstruction_reading.pl', 'reconstruction_reading', 'RECONSTRUCTION_READING', 74).
test_case('testsets/regulatory_recognition_reading.pl', 'corresponding', 'REGULATORY_RECOGNITION_READING', 75).
test_case('testsets/relational_autonomy.pl', 'relational_autonomy', 'RELATIONAL_AUTONOMY', 76).
test_case('testsets/remedial_reading.pl', 'remedial_reading', 'REMEDIAL_READING', 77).
test_case('testsets/republican_reading.pl', 'republican_reading', 'REPUBLICAN_READING', 78).
test_case('testsets/retributive_reading.pl', 'retributive_reading', 'RETRIBUTIVE_READING', 79).
test_case('testsets/revolutionary_legitimacy_scaffold.pl', 'revolutionary_legitimacy_scaffold', 'REVOLUTIONARY_LEGITIMACY_SCAFFOLD', 80).
test_case('testsets/sartorial_commitment_override.pl', 'sartorial_commitment_override', 'SARTORIAL_COMMITMENT_OVERRIDE', 81).
test_case('testsets/security_occupation_tradeoff.pl', 'security_occupation_tradeoff', 'SECURITY_OCCUPATION_TRADEOFF', 82).
test_case('testsets/simulation_as_sufficient.pl', 'simulation_as_sufficient', 'SIMULATION_AS_SUFFICIENT', 83).
test_case('testsets/sound_money_scarcity_constraint.pl', 'sound_money_scarcity_constraint', 'SOUND_MONEY_SCARCITY_CONSTRAINT', 84).
test_case('testsets/sovereignty_primary.pl', '0', 'SOVEREIGNTY_PRIMARY', 85).
test_case('testsets/sovereignty_reading.pl', 'sovereignty_reading', 'SOVEREIGNTY_READING', 86).
test_case('testsets/speculative_price_volatility_trap.pl', 'speculative_price_volatility_trap', 'SPECULATIVE_PRICE_VOLATILITY_TRAP', 87).
test_case('testsets/state_modernization_extraction.pl', 'state_modernization_extraction', 'STATE_MODERNIZATION_EXTRACTION', 88).
test_case('testsets/study_as_archiving.pl', 'study_as_archiving', 'STUDY_AS_ARCHIVING', 89).
test_case('testsets/study_as_occupation.pl', 'study_as_occupation', 'STUDY_AS_OCCUPATION', 90).
test_case('testsets/subsidy_capture_reading.pl', 'subsidy_capture_reading', 'SUBSIDY_CAPTURE_READING', 91).
test_case('testsets/survival_competence_reading.pl', 'theater_ratio', 'SURVIVAL_COMPETENCE_READING', 92).
test_case('testsets/swap_henrician_substitution.pl', 'swap_henrician_substitution', 'SWAP_HENRICIAN_SUBSTITUTION', 93).
test_case('testsets/temporal_boundary_constraint.pl', 'temporal_boundary_constraint', 'TEMPORAL_BOUNDARY_CONSTRAINT', 94).
test_case('testsets/temporal_decay_pressure.pl', 'temporal_decay_pressure', 'TEMPORAL_DECAY_PRESSURE', 95).
test_case('testsets/textualist_reading.pl', 'textualist_reading', 'TEXTUALIST_READING', 96).
test_case('testsets/transmission_as_conceptual_import.pl', 'time', 'TRANSMISSION_AS_CONCEPTUAL_IMPORT', 97).
test_case('testsets/welfare_regulated_use.pl', 'welfare_regulated_use', 'WELFARE_REGULATED_USE', 98).

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

