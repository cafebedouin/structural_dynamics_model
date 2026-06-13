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
test_case('testsets/biblical_authority__conciliar_reading.pl', 'biblical_authority__conciliar_reading', 'BIBLICAL_AUTHORITY__CONCILIAR_READING', 8).
test_case('testsets/biblical_authority__sola_scriptura_reading.pl', 'biblical_authority__sola_scriptura_reading', 'BIBLICAL_AUTHORITY__SOLA_SCRIPTURA_READING', 9).
test_case('testsets/biblical_authority__tradition_scripture_reading.pl', 'biblical_authority__tradition_scripture_reading', 'BIBLICAL_AUTHORITY__TRADITION_SCRIPTURE_READING', 10).
test_case('testsets/constitutional_text_authority__living_constitutionalist_reading.pl', 'constitutional_text_authority__living_constitutionalist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__LIVING_CONSTITUTIONALIST_READING', 11).
test_case('testsets/constitutional_text_authority__originalist_reading.pl', 'constitutional_text_authority__originalist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__ORIGINALIST_READING', 12).
test_case('testsets/constitutional_text_authority__positivist_reading.pl', 'constitutional_text_authority__positivist_reading', 'CONSTITUTIONAL_TEXT_AUTHORITY__POSITIVIST_READING', 13).
test_case('testsets/demographic_skill_mismatch_c0.pl', 'demographic_skill_mismatch_c0', 'DEMOGRAPHIC_SKILL_MISMATCH_C0', 14).
test_case('testsets/federation_membership_obligations__integration_primary.pl', 'federation_membership_obligations__integration_primary', 'FEDERATION_MEMBERSHIP_OBLIGATIONS__INTEGRATION_PRIMARY', 15).
test_case('testsets/federation_membership_obligations__member_sovereignty_primary.pl', 'federation_membership_obligations__member_sovereignty_primary', 'FEDERATION_MEMBERSHIP_OBLIGATIONS__MEMBER_SOVEREIGNTY_PRIMARY', 16).
test_case('testsets/federation_membership_obligations__selective_solidarity.pl', 'federation_membership_obligations__selective_solidarity', 'FEDERATION_MEMBERSHIP_OBLIGATIONS__SELECTIVE_SOLIDARITY', 17).
test_case('testsets/feud_obligation_kernel__christianized_pacification_reading.pl', 'feud_obligation_kernel__christianized_pacification_reading', 'FEUD_OBLIGATION_KERNEL__CHRISTIANIZED_PACIFICATION_READING', 18).
test_case('testsets/feud_obligation_kernel__extraction_cycle_reading.pl', 'feud_obligation_kernel__extraction_cycle_reading', 'FEUD_OBLIGATION_KERNEL__EXTRACTION_CYCLE_READING', 19).
test_case('testsets/feud_obligation_kernel__stateless_coordination_reading.pl', 'feud_obligation_kernel__stateless_coordination_reading', 'FEUD_OBLIGATION_KERNEL__STATELESS_COORDINATION_READING', 20).
test_case('testsets/institutional_trust_erosion_c0.pl', 'institutional_trust_erosion_c0', 'INSTITUTIONAL_TRUST_EROSION_C0', 21).
test_case('testsets/kodashim_obligation__study_as_archive.pl', 'kodashim_obligation__study_as_archive', 'KODASHIM_OBLIGATION__STUDY_AS_ARCHIVE', 22).
test_case('testsets/kodashim_obligation__study_as_performance.pl', 'kodashim_obligation__study_as_performance', 'KODASHIM_OBLIGATION__STUDY_AS_PERFORMANCE', 23).
test_case('testsets/kodashim_obligation__study_as_preparation.pl', 'kodashim_obligation__study_as_preparation', 'KODASHIM_OBLIGATION__STUDY_AS_PREPARATION', 24).
test_case('testsets/market_as_natural_default__beneficiary_maintained_reading.pl', 'market_as_natural_default__beneficiary_maintained_reading', 'MARKET_AS_NATURAL_DEFAULT__BENEFICIARY_MAINTAINED_READING', 25).
test_case('testsets/market_as_natural_default__hybrid_amnesia_reading.pl', 'market_as_natural_default__hybrid_amnesia_reading', 'MARKET_AS_NATURAL_DEFAULT__HYBRID_AMNESIA_READING', 26).
test_case('testsets/market_as_natural_default__lapsed_alternative_reading.pl', 'market_as_natural_default__lapsed_alternative_reading', 'MARKET_AS_NATURAL_DEFAULT__LAPSED_ALTERNATIVE_READING', 27).
test_case('testsets/naskh_principle__classical_abrogation.pl', 'naskh_principle__classical_abrogation', 'NASKH_PRINCIPLE__CLASSICAL_ABROGATION', 28).
test_case('testsets/naskh_principle__contextual_harmonization.pl', 'naskh_principle__contextual_harmonization', 'NASKH_PRINCIPLE__CONTEXTUAL_HARMONIZATION', 29).
test_case('testsets/naskh_principle__progressive_restriction.pl', 'naskh_principle__progressive_restriction', 'NASKH_PRINCIPLE__PROGRESSIVE_RESTRICTION', 30).
test_case('testsets/organization_floor_c0.pl', 'organization_floor_c0', 'ORGANIZATION_FLOOR_C0', 31).
test_case('testsets/provincial_sovereignty_boundary__compact_federalism.pl', 'provincial_sovereignty_boundary__compact_federalism', 'PROVINCIAL_SOVEREIGNTY_BOUNDARY__COMPACT_FEDERALISM', 32).
test_case('testsets/provincial_sovereignty_boundary__constitutional_subordination.pl', 'provincial_sovereignty_boundary__constitutional_subordination', 'PROVINCIAL_SOVEREIGNTY_BOUNDARY__CONSTITUTIONAL_SUBORDINATION', 33).
test_case('testsets/provincial_sovereignty_boundary__resource_sovereignty_primacy.pl', 'provincial_sovereignty_boundary__resource_sovereignty_primacy', 'PROVINCIAL_SOVEREIGNTY_BOUNDARY__RESOURCE_SOVEREIGNTY_PRIMACY', 34).
test_case('testsets/scale_ceiling_c0.pl', 'scale_ceiling_c0', 'SCALE_CEILING_C0', 35).

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

