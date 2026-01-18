:- use_module(scenario_manager).
:- dynamic test_passed/1.
:- dynamic test_failed/2.

run_dynamic_suite :-
    retractall(test_passed(_)),
    retractall(test_failed(_, _)),
    writeln('--- STARTING DYNAMIC VALIDATION ---'),
    test_file('./testsets/carbon_credit_markets_2026.pl', 'carbon_credit_markets_2026_int', 'CARBON_CREDIT_MARKETS_2026', 10),
    test_file('./testsets/deferential_realism_core.pl', 'dr_recursive_int', 'DEFERENTIAL_REALISM_CORE', 22),
    count_and_report.

test_file(Path, ID, Label, N) :-
    format('~n[~w] DOMAIN: ~w (~w)~n', [N, Label, Path]),
    (   catch(load_and_run(Path, ID), E, (assertz(test_failed(Path, E)), format('[FAIL] Exception: ~w~n', [E]), fail))
    ->  assertz(test_passed(Path)),
        report_generator:generate_llm_feedback(ID)
    ;   assertz(test_failed(Path, audit_failed)),
        report_generator:generate_llm_feedback(ID)
    ),
    !. 

count_and_report :-
    findall(P, test_passed(P), Ps), length(Ps, PC), findall(F, test_failed(F,_), Fs), length(Fs, FC),
    format('~nDONE: ~w Passed, ~w Failed~n', [PC, FC]).
