:- module(scenario_manager, [
    clear_kb/0,
    load_and_run/2,
    list_active_intervals/0
]).

:- use_module(narrative_ontology).
:- use_module(test_harness).

/**
 * clear_kb
 * Forcefully retracts all facts.
 */
clear_kb :-
    format('~n[SCENARIO MANAGER] Clearing Knowledge Base...~n'),
    % Core Layers
    retractall(narrative_ontology:entity(_, _)),
    retractall(narrative_ontology:interval(_, _, _)),
    retractall(narrative_ontology:event(_, _, _, _)),
    retractall(narrative_ontology:measurement(_, _, _, _, _)),
    % Constraint Layer
    retractall(narrative_ontology:constraint_claim(_, _)),
    retractall(narrative_ontology:constraint_metric(_, _, _)),
    retractall(narrative_ontology:recommendation(_, _)),
    retractall(narrative_ontology:affects_constraint(_, _)),
    retractall(narrative_ontology:veto_actor(_)),
    retractall(narrative_ontology:veto_exposed(_, _)),
    % Intent Layer
    retractall(narrative_ontology:intent_viable_alternative(_, _, _)),
    retractall(narrative_ontology:intent_alternative_rejected(_, _, _)),
    retractall(narrative_ontology:intent_beneficiary_class(_, _)),
    retractall(narrative_ontology:intent_power_change(_, _, _)),
    retractall(narrative_ontology:intent_suppression_level(_, _, _, _)),
    retractall(narrative_ontology:intent_resistance_level(_, _, _, _)),
    retractall(narrative_ontology:intent_norm_strength(_, _, _)),
    format('[OK] Knowledge Base is empty.~n').

/**
 * load_and_run(+File, +IntervalID)
 * Uses the user namespace to force-load the data pack.
 */
load_and_run(File, IntervalID) :-
    clear_kb,
    (   exists_file(File)
    ->  format('[SCENARIO MANAGER] Loading: ~w...~n', [File]),
        % By using user:consult, we bypass the scenario_manager module sandbox
        user:consult(File), 
        test_harness:run_all_tests(IntervalID)
    ;   format('[ERROR] File ~w not found.~n', [File])
    ).

list_active_intervals :-
    findall(ID, narrative_ontology:interval(ID, _, _), IDs),
    format('~nActive Intervals in KB: ~w~n', [IDs]).

