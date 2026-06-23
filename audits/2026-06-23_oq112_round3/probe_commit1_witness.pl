% OQ-112 Round 3 Commit 1 — witness (post-edit maxent_classifier.pl).
%   WA  genuine-values-unchanged: over all 86 claim constraints, NO accessor returns the
%       `unknown` sentinel on 92 (every else-branch is unreached) -> behavior byte-identical
%       to pre-edit (the only change is the else-VALUE, never taken). "live-unexercised on 92".
%   WB  baseline: json_report-mirrored sequence -> maxent_indexed_run_info present, 0 void alerts.
%   WC  END-TO-END gate witness (also the Commit-2 disproof): retract theater on one claim
%       constraint -> maxent_indexed_run throws (caught, as in json_report) -> run_info ABSENT
%       -> maxent_stage_attempted_but_void(indexed) FIRES -> indexed void alert present.
%       Item-2's gate catches the theater-throw; there is no silent findall-drop.
%   WD  boundary guard: force `unknown` through maxent_threshold_proximity -> fails closed
%       (number/1 guard), no throw (protects the unwired maxent_boundary_analysis).

:- [stack].
:- use_module(probe_harness).
:- use_module(diagnostic_summary).
:- corpus_loader:ensure_corpus_loaded.

three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).

count_unknowns(Ctx, NU) :-
    config:param(theater_metric_name, _),
    findall(1, (
        corpus_loader:corpus_constraint(C),
        narrative_ontology:constraint_claim(C, _),
        ( maxent_classifier:get_constraint_metrics(C, E, S, T),
          ( E==unknown ; S==unknown ; T==unknown )
        ; maxent_classifier:get_constraint_metrics_indexed(C, Ctx, E2, S2, T2),
          ( E2==unknown ; S2==unknown ; T2==unknown )
        ; member(M, [extractiveness,suppression,theater]),
          maxent_classifier:metric_value(C, M, V), V==unknown
        ; member(M2, [extractiveness,suppression,theater]),
          maxent_classifier:metric_value_indexed(C, Ctx, M2, V2), V2==unknown
        )
    ), L),
    length(L, NU).

run_seq(Ctx) :-
    retractall(diagnostic_summary:maxent_attempted(_)),
    assertz(diagnostic_summary:maxent_attempted(classical)),
    ( catch(maxent_classifier:maxent_run(Ctx, _), _, fail) -> true ; true ),
    assertz(diagnostic_summary:maxent_attempted(indexed)),
    ( catch(maxent_classifier:maxent_indexed_run(Ctx, _), _, fail) -> true ; true ).

probe :-
    constraint_indexing:default_context(Ctx),

    % WA — no `unknown` produced on 92
    count_unknowns(Ctx, NU),
    format('WA: unknown-sentinels produced over 86 claim constraints on 92 = ~w~n', [NU]),
    format('WA verdict: ~w~n~n',
           [(NU =:= 0 -> 'else-branches UNREACHED -> genuine values unchanged (live-unexercised on 92)' ; 'REACHED — investigate')]),

    % WB — baseline
    run_seq(Ctx),
    ( maxent_classifier:maxent_indexed_run_info(Ctx, NTot, _) -> RIB = present(NTot) ; RIB = absent ),
    diagnostic_summary:maxent_void_alerts(AlertsB),
    format('WB baseline: maxent_indexed_run_info=~w ; void_alerts=~w~n~n', [RIB, AlertsB]),

    % WC — END-TO-END: retract theater on one claim constraint, re-run, observe gate
    config:param(theater_metric_name, TN),
    once(( corpus_loader:corpus_constraint(C0),
           narrative_ontology:constraint_claim(C0, _),
           narrative_ontology:constraint_metric(C0, TN, _),
           drl_core:dr_type(C0, Ctx, _) )),
    format('WC intervention constraint = ~w (theater retracted)~n', [C0]),
    probe_harness:with_retracted(
        [narrative_ontology:constraint_metric(C0, TN, _)],
        ( three_way(maxent_classifier:maxent_indexed_run(Ctx, _), RRun),
          format('WC: maxent_indexed_run -> ~w~n', [RRun]),
          ( maxent_classifier:maxent_indexed_run_info(Ctx, _, _) -> RIC = present ; RIC = absent ),
          % mirror json_report: attempted set, run absorbed -> gate reads run_info absence
          ( catch(maxent_classifier:maxent_indexed_run(Ctx, _), _, fail) -> true ; true ),
          ( maxent_classifier:maxent_indexed_run_info(Ctx, _, _) -> RIC2 = present ; RIC2 = absent ),
          diagnostic_summary:maxent_void_alerts(AlertsC),
          format('WC: run_info after throw = ~w (re-confirm ~w) ; void_alerts=~w~n', [RIC, RIC2, AlertsC]),
          ( memberchk(alert(maxent_voided(indexed), _, _), AlertsC)
          -> format('WC verdict: item-2 gate FIRES (indexed void alert) -> verdict_join caps headline. Commit-2 silent-drop premise DISPROVEN.~n~n')
          ;  format('WC verdict: NO indexed void alert — REVISIT~n~n') )
        )),

    % restore run state for WD
    run_seq(Ctx),

    % WD — boundary guard fails closed on unknown
    ( catch(maxent_classifier:maxent_threshold_proximity(some_absent_constraint_xyz, Ctx, _, _), Eb, (RB = error(Eb)))
      -> ( var(RB) -> RB = success ; true ) ; RB = quiet_failure ),
    format('WD: maxent_threshold_proximity(absent-constraint) -> ~w (number/1 guard fails closed, no abs/2 throw)~n', [RB]).

:- (catch(probe, E, (format('PROBE ERROR: ~w~n', [E]), fail)) -> true ; format('PROBE FAILED~n')), halt.
:- halt(1).
