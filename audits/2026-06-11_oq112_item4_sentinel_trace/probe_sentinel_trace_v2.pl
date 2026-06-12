% OQ-112 item 4 — sentinel trace probe v2 (read-only).
% v1 defect (probe_output.txt): W4 ran before maxent_run, so maxent_profile/4
% (DYNAMIC, populated by precompute) was empty, Ctx bound to `none`, and the
% gaussian sink was never exercised — LL=-10.0 was prior+bool only, a
% false-clean trace. v2 runs the driver first, uses default_context/1 as the
% real callers do, and adds the constraint_claim reachability gate.
%
% Witnesses:
%   W8   reachability gate: do the absent-suppression constraints carry
%        constraint_claim (= enter maxent_run's findall)?  [v1 W6 summary said
%        60 of corpus 62]
%   W9   driver as pipeline calls it: maxent_run(DefaultCtx) three-way
%   W10  profile-present check, then the SINK: maxent_type_log_likelihood on
%        the absent-suppression constraint with profiles loaded — three-way
%        (error / success-value / quiet-failure)
%   W11  same for indexed path (metric_value_indexed has no default branch)
%   W12  threshold-proximity sink, uncaught vs caught-as-callers-wrap-it
%   W13  positive control: present-suppression constraint -> numeric LL, no error

:- [stack].
:- corpus_loader:ensure_corpus_loaded.

three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).

probe :-
    config:param(suppression_metric_name, SuppName),
    findall(C, (corpus_loader:corpus_constraint(C),
                \+ narrative_ontology:constraint_metric(C, SuppName, _)), Absent),
    format('absent-suppression constraints: ~w~n~n', [Absent]),
    Absent = [A|_],

    % W8: reachability gate into maxent_run
    forall(member(X, Absent),
        (   narrative_ontology:constraint_claim(X, Claim)
        ->  format('W8: ~w HAS constraint_claim(~w) — enters maxent_run~n', [X, Claim])
        ;   format('W8: ~w has NO constraint_claim — excluded from maxent_run findall~n', [X])
        )),
    aggregate_all(count, (narrative_ontology:constraint_claim(C2, _), atom(C2)), NClaims),
    format('W8b: constraint_claim count = ~w (corpus = 62)~n~n', [NClaims]),

    % W9: the driver, as json_report/maxent_report call it
    constraint_indexing:default_context(Ctx),
    format('W9 context: ~w~n', [Ctx]),
    three_way(maxent_classifier:maxent_run(Ctx, Summary), R9),
    format('W9: maxent_run -> ~w summary=~w~n~n', [R9, Summary]),

    % W10: profiles present? then the sink
    (   maxent_classifier:maxent_profile(snare, suppression, Ctx, P10)
    ->  format('W10 profile present: maxent_profile(snare, suppression) = ~w~n', [P10])
    ;   format('W10 profile ABSENT — sink not exercisable at this context~n')
    ),
    three_way(maxent_classifier:maxent_type_log_likelihood(A, snare, Ctx, LL10, _), R10),
    format('W10 SINK: maxent_type_log_likelihood(~w, snare) -> ~w LL=~w~n~n', [A, R10, LL10]),

    % W11: indexed path
    three_way(maxent_classifier:maxent_type_log_likelihood_indexed(A, snare, Ctx, LL11, _), R11),
    format('W11 SINK (indexed): -> ~w LL=~w~n~n', [R11, LL11]),

    % W12: threshold proximity — uncaught, then wrapped as maxent_report:211 wraps it
    three_way(maxent_classifier:maxent_threshold_proximity(A, Ctx, B12, D12), R12a),
    format('W12a uncaught: maxent_threshold_proximity(~w) -> ~w (B=~w D=~w)~n', [A, R12a, B12, D12]),
    (   catch(maxent_classifier:maxent_threshold_proximity(A, Ctx, _, _), _, fail)
    ->  format('W12b caller-wrapped: succeeded~n~n')
    ;   format('W12b caller-wrapped (catch(_,fail) as maxent_report:211): QUIET FAILURE — row silently dropped~n~n')
    ),

    % W13: positive control
    corpus_loader:corpus_constraint(PC),
    narrative_ontology:constraint_metric(PC, SuppName, _), !,
    three_way(maxent_classifier:maxent_type_log_likelihood(PC, snare, Ctx, LL13, _), R13),
    format('W13 control: maxent_type_log_likelihood(~w, snare) -> ~w LL=~w~n', [PC, R13, LL13]).

:- probe, halt.
:- halt(1).
