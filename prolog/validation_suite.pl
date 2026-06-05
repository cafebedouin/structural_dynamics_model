:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/beautiful_reports_feedback_loop.pl', 'beautiful_reports_feedback_loop', 'BEAUTIFUL_REPORTS_FEEDBACK_LOOP', 1).
test_case('testsets/cognitive_warfare_collapse.pl', 'cognitive_warfare_collapse', 'COGNITIVE_WARFARE_COLLAPSE', 2).
test_case('testsets/compute_constraint_as_brake.pl', 'compute_constraint_as_brake', 'COMPUTE_CONSTRAINT_AS_BRAKE', 3).
test_case('testsets/control_mechanism_backfire.pl', 'control_mechanism_backfire', 'CONTROL_MECHANISM_BACKFIRE', 4).
test_case('testsets/data_overload_triage.pl', 'data_overload_triage', 'DATA_OVERLOAD_TRIAGE', 5).
test_case('testsets/deathonomics_collapse.pl', '0', 'DEATHONOMICS_COLLAPSE', 6).
test_case('testsets/elite_legitimacy_fracture.pl', '0', 'ELITE_LEGITIMACY_FRACTURE', 7).
test_case('testsets/export_control_reversibility.pl', 't4', 'EXPORT_CONTROL_REVERSIBILITY', 8).
test_case('testsets/grey_market_evasion.pl', '0', 'GREY_MARKET_EVASION', 9).
test_case('testsets/legitimacy_narrative_inversion.pl', '2014', 'LEGITIMACY_NARRATIVE_INVERSION', 10).
test_case('testsets/manpower_exhaustion_trap.pl', 'manpower_exhaustion_trap', 'MANPOWER_EXHAUSTION_TRAP', 11).
test_case('testsets/milblogger_legitimacy_erosion.pl', 'milblogger_legitimacy_erosion', 'MILBLOGGER_LEGITIMACY_EROSION', 12).
test_case('testsets/military_defeat_cascade.pl', 'military_defeat_cascade', 'MILITARY_DEFEAT_CASCADE', 13).
test_case('testsets/operational_overextension_trap.pl', 'operational_overextension_trap', 'OPERATIONAL_OVEREXTENSION_TRAP', 14).
test_case('testsets/passportization_legal_scaffolding.pl', 'passportization_legal_scaffolding', 'PASSPORTIZATION_LEGAL_SCAFFOLDING', 15).
test_case('testsets/predictive_surveillance_capability.pl', 'predictive_surveillance_capability', 'PREDICTIVE_SURVEILLANCE_CAPABILITY', 16).
test_case('testsets/public_confidence_erosion.pl', 'public_confidence_erosion', 'PUBLIC_CONFIDENCE_EROSION', 17).
test_case('testsets/surveillance_export_proliferation.pl', 'surveillance_export_proliferation', 'SURVEILLANCE_EXPORT_PROLIFERATION', 18).
test_case('testsets/technology_asymmetry_ukraine_russia.pl', 'technology_asymmetry_ukraine_russia', 'TECHNOLOGY_ASYMMETRY_UKRAINE_RUSSIA', 19).
test_case('testsets/verification_authority_fragmentation.pl', 'verification_authority_fragmentation', 'VERIFICATION_AUTHORITY_FRAGMENTATION', 20).

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

