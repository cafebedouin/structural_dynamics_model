:- module(v3_1_data_repair, [
    repair_interval/1
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).

% Ensure we can add facts to the ontology's measurement predicate
:- dynamic narrative_ontology:measurement/5.

/* ============================================================
   REPAIR ORCHESTRATOR
   ============================================================ */

repair_interval(IntervalID) :-
    % Check if IntervalID exists before proceeding to avoid returning false
    (   narrative_ontology:interval(IntervalID, T0, Tn)
    ->  format('~n[REPAIR] Auditing vectors for: ~w...~n', [IntervalID]),
        forall(v3_1_config:level(L), 
               ( repair_point(L, T0, IntervalID), 
                 repair_point(L, Tn, IntervalID) 
               )),
        true % Force success
    ;   format('~n[ERROR] Interval ~w not found in database.~n', [IntervalID]),
        false
    ).

repair_point(Level, Time, IntervalID) :-
    Components = [accessibility_collapse(Level), stakes_inflation(Level), 
                  suppression(Level), resistance(Level)],
    forall(member(Metric, Components), 
           ensure_metric_exists(Metric, Time, IntervalID)).

ensure_metric_exists(Metric, Time, IntervalID) :-
    % Look directly into the ontology
    narrative_ontology:measurement(_, _, Metric, Time, _)
    ->  true
    ;   (   gensym(repair_m_, SyntheticID),
            % Use assertz with explicit module prefix
            assertz(narrative_ontology:measurement(SyntheticID, IntervalID, Metric, Time, 0.5)),
            format('  [FIXED] Imputed 0.5 for ~w at T=~w~n', [Metric, Time])
        ).

