:- module(test_harness, [
    load_scenario/1,
    run_all_tests/0,    % New: One-button alias
    run_all_tests/1,
    run_all_tests/2,
    quick_check/1
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection).
:- use_module(v3_1_data_repair).
:- use_module(data_verification).
:- use_module(pattern_analysis).
:- use_module(intent_engine).
:- use_module(constraint_bridge).
:- use_module(constraint_indexing).
:- use_module(drl_core).
:- use_module(report_generator).

% Alias for quick execution using the most recent target
run_all_tests :-
    run_all_tests('tax_code_section_469'). % Targeted ID from your analysis

run_all_tests(IntervalID) :-
    constraint_indexing:default_context(Ctx),
    run_all_tests(IntervalID, Ctx).

run_all_tests(IntervalID, Context) :-
    format('~n>>> INITIATING v3.2.0 DR-AUDIT SUITE: ~w~n', [IntervalID]),
    
    % Step 1: Data Repair
    v3_1_data_repair:repair_interval(IntervalID),

    % Step 2: Verification Gate
    (   data_verification:verify_all,
        data_verification:check_paired_measurements
    ->  format('[OK] Verification passed.~n')
    ;   format('[FAIL] Verification failed for ~w.~n', [IntervalID]), fail),

    % Step 3: Indexical Audit
    forall(narrative_ontology:constraint_claim(C, _),
           constraint_indexing:compare_perspectives(C, Context)),

    % Step 4: Intent and Reporting
    intent_engine:analyze_intent(IntervalID),
    constraint_bridge:dr_diagnostic_report(IntervalID),

    % Step 5: System Insights (New)
    format('~n--- SYSTEM INSIGHTS ---~n'),
    narrative_ontology:count_unresolved_omegas(OmegaCount),
    format('  Omegas Identified: ~w~n', [OmegaCount]),
    report_generator:generate_full_report(IntervalID).

% Ensure quick_check uses the predicates from the authoritative module
quick_check(IntervalID) :-
    format('--- Diagnostic: ~w ---~n', [IntervalID]),
    (   drl_core:base_extractiveness(Name, E), E > 0.8
    ->  format('CRITICAL EXTRACTIVENESS: ~w (~2f)~n', [Name, E])
    ;   format('No critical extractiveness detected.~n')).

%% load_scenario(+Path)
load_scenario(Path) :-
    exists_file(Path),
    consult(Path),
    format('~n[SCENARIO] Successfully loaded: ~w~n', [Path]).
load_scenario(Path) :-
    \+ exists_file(Path), 
    format('~n[ERROR] Scenario file not found: ~w~n', [Path]),
    fail.
