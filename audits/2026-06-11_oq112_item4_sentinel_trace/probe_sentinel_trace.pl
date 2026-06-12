% OQ-112 item 4 — sentinel trace probe (read-only).
% Witnesses, in order:
%   W1  absent-suppression constraints exist on the live corpus (and which)
%   W2  sentinel path taken: get_raw_suppression -> unknown (per-process positive
%       control: a present-suppression constraint returns a number in THIS session)
%   W3  dead branch: get_constraint_metrics returns Supp == unknown, i.e. the
%       "; Supp = 0.0" default did NOT fire
%   W4  sink behavior, three-way capture (error / success-value / quiet-failure):
%       maxent_type_log_likelihood on the absent-suppression constraint
%   W5  same three-way capture for the indexed path (metric_value_indexed has
%       NO default branch at all)
%   W6  whole-driver behavior: maxent_run over the full corpus
%   W7  positive control for W4: same goal on present-suppression constraint -> numeric

:- [stack].
:- corpus_loader:ensure_corpus_loaded.

three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).

probe :-
    config:param(suppression_metric_name, SuppName),
    format('suppression_metric_name = ~w~n~n', [SuppName]),

    % W1: absent-suppression constraints on the live corpus
    findall(C, (corpus_loader:corpus_constraint(C),
                \+ narrative_ontology:constraint_metric(C, SuppName, _)), Absent),
    length(Absent, NAbsent),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NCorpus),
    format('W1: corpus=~w, absent-suppression=~w: ~w~n~n', [NCorpus, NAbsent, Absent]),
    (   Absent = [A|_]
    ->  true
    ;   format('W1 EMPTY — no absent-suppression constraint; trace cannot proceed~n'),
        fail
    ),

    % W2: sentinel path + per-process positive control
    drl_core:get_raw_suppression(A, SA),
    format('W2: get_raw_suppression(~w) = ~w~n', [A, SA]),
    corpus_loader:corpus_constraint(P),
    narrative_ontology:constraint_metric(P, SuppName, _), !,
    drl_core:get_raw_suppression(P, SP),
    format('W2 control: get_raw_suppression(~w) = ~w (number: ~w)~n~n',
           [P, SP, (number(SP) -> yes ; no)]),

    % W3: dead-branch witness
    maxent_classifier:get_constraint_metrics(A, EpsA, SuppA, TheaterA),
    format('W3: get_constraint_metrics(~w): eps=~w supp=~w theater=~w~n',
           [A, EpsA, SuppA, TheaterA]),
    format('W3 verdict: "; Supp = 0.0" branch ~w~n~n',
           [(SuppA == unknown -> 'DEAD (sentinel arrived)' ; 'ALIVE (default fired)')]),

    % W4: the sink, three-way
    ( maxent_classifier:maxent_profile(snare, suppression, Ctx, _) -> true ; Ctx = none ),
    format('W4 context used: ~w~n', [Ctx]),
    three_way(maxent_classifier:maxent_type_log_likelihood(A, snare, Ctx, LL4, _), R4),
    format('W4: maxent_type_log_likelihood(~w, snare) -> ~w (LL=~w)~n~n', [A, R4, LL4]),

    % W5: indexed path (no default branch at metric_value_indexed)
    three_way(maxent_classifier:metric_value_indexed(A, Ctx, suppression, V5), R5a),
    format('W5a: metric_value_indexed suppression -> ~w (V=~w)~n', [R5a, V5]),
    three_way(maxent_classifier:maxent_type_log_likelihood_indexed(A, snare, Ctx, LL5, _), R5b),
    format('W5b: maxent_type_log_likelihood_indexed(~w, snare) -> ~w (LL=~w)~n~n', [A, R5b, LL5]),

    % W6: the whole driver, as report modules call it
    three_way(maxent_classifier:maxent_run(Ctx, Summary6), R6),
    format('W6: maxent_run(~w) -> ~w (summary=~w)~n', [Ctx, R6, Summary6]),
    (   maxent_classifier:maxent_dist(A, Ctx, DistA)
    ->  format('W6b: maxent_dist for ~w EXISTS: ~w~n~n', [A, DistA])
    ;   format('W6b: maxent_dist for ~w ABSENT after maxent_run~n~n', [A])
    ),

    % W7: positive control for the sink
    three_way(maxent_classifier:maxent_type_log_likelihood(P, snare, Ctx, LL7, _), R7),
    format('W7 control: maxent_type_log_likelihood(~w, snare) -> ~w (LL=~w)~n', [P, R7, LL7]).

:- probe, halt.
:- halt(1).
