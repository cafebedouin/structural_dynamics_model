% ============================================================================
% TRANSITION PATHS — Degradation Path Detection & Terminal State Prediction
% Split from drl_lifecycle.pl
% ============================================================================

:- module(transition_paths, [
    transition_path/4,
    repair_transition/4,
    degradation_chain/3,
    predicted_terminal_state/3
]).

:- use_module(library(lists), [last/2]).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(metric_drift_events).

/* ================================================================
   TRANSITION PATH DETECTION
   Detects the canonical degradation paths from core.md.
   ================================================================ */

%% transition_path(+ConstraintID, -FromType, -ToType, -Evidence)
%  Detects a constraint in the process of transitioning between types.
%  Uses current metrics + drift events to infer the transition.

% Rope -> Tangled Rope (extraction accumulating into coordination)
transition_path(C, rope, tangled_rope, evidence(extraction_rising, E, has_coordination, true)) :-
    drl_core:dr_type(C, Type),
    Type = rope,
    metric_drift_events:safe_metric(C, extractiveness, E),
    config:param(rope_epsilon_ceiling, Ceil),
    E > Ceil * 0.7,
    narrative_ontology:has_coordination_function(C),
    metric_drift_events:metric_trend(C, base_extractiveness, increasing).

% Tangled Rope -> Snare (coordination dying while extraction grows)
transition_path(C, tangled_rope, snare, evidence(coordination_declining, true, extraction, E)) :-
    drl_core:dr_type(C, Type),
    Type = tangled_rope,
    metric_drift_events:safe_metric(C, extractiveness, E),
    config:param(snare_epsilon_floor, Floor),
    E > Floor * 0.8,
    (   metric_drift_events:drift_event(C, coordination_loss, _)
    ;   metric_drift_events:metric_trend(C, coordination_effectiveness, decreasing)
    ).

% Rope -> Piton (direct obsolescence, no extraction phase)
transition_path(C, rope, piton, evidence(function_obsolete, true, theater_high, TR)) :-
    drl_core:dr_type(C, Type),
    Type = rope,
    metric_drift_events:safe_metric(C, theater_ratio, TR),
    TR > 0.5,
    metric_drift_events:drift_event(C, function_obsolescence, _).

% Scaffold -> Piton (sunset violation)
transition_path(C, scaffold, piton, evidence(sunset_violated, true)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    metric_drift_events:drift_event(C, sunset_violation, _).

% Scaffold -> Snare (calcification with extraction)
transition_path(C, scaffold, snare, evidence(extraction_added, E, sunset_violated, Violated)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    metric_drift_events:safe_metric(C, extractiveness, E),
    config:param(snare_epsilon_floor, Floor),
    E > Floor * 0.7,
    (   metric_drift_events:drift_event(C, sunset_violation, _)
    ->  Violated = true
    ;   Violated = false
    ).

% Scaffold -> Tangled Rope (extraction added during transition)
transition_path(C, scaffold, tangled_rope, evidence(extraction_emerging, E, coordination_intact, true)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    metric_drift_events:safe_metric(C, extractiveness, E),
    config:param(tangled_rope_epsilon_floor, Floor),
    E > Floor * 0.7,
    narrative_ontology:has_coordination_function(C).

% Snare -> I-Piton (internalization)
transition_path(C, snare, piton, evidence(internalized, true)) :-
    drl_core:dr_type(C, Type),
    Type = snare,
    metric_drift_events:drift_event(C, internalized_piton, _).

% Snare -> False Mountain (naturalization)
transition_path(C, snare, false_mountain, evidence(naturalized, true, claimed, mountain)) :-
    drl_core:dr_type(C, Type),
    Type = snare,
    narrative_ontology:constraint_claim(C, mountain).

%% degradation_chain(+ConstraintID, -Chain, -Evidence)
%  Detects multi-step degradation chains by examining measurement history.
degradation_chain(C, Chain, evidence(time_span, T1, T2)) :-
    findall(T-Type,
            (narrative_ontology:measurement(_, C, _, T, _),
             snapshot_type(C, T, Type)),
            RawPairs),
    RawPairs \= [],
    sort(RawPairs, Sorted),
    pairs_values(Sorted, TypeList),
    deduplicate_consecutive(TypeList, Chain),
    length(Chain, Len),
    Len > 1,
    Sorted = [T1-_|_],
    last(Sorted, T2-_).

%% snapshot_type(+C, +Time, -Type)
%  Classifies a constraint at a specific time using measurements available.
%  Uses sigmoid pipeline: χ = ε × f(d) × σ(S).
snapshot_type(C, Time, Type) :-
    % Determinism guard (OQ-83 close, 2026-06-11): classify_at_time/5 leaves its
    % nb-globals set, and they key on the constraint — so without this clear the
    % piton/excess gates (drl_core:effective_theater_ratio, boltzmann excess_extraction)
    % read whatever temporal state a prior same-C classify_at_time call left
    % (order-dependent output, witnessed). snapshot_type is deliberately NOT
    % threaded: static-fallback semantics, visibly distinct from classify_at_time
    % (a second, semantic divergence exists in eps-sourcing at unmeasured times).
    % Witnesses: audits/2026-06-11_oq83_close/STEP1_REPORT.md.
    nb_setval(classify_at_time_theater, none),
    nb_setval(classify_at_time_eps, none),
    (   metric_drift_events:metric_at(C, base_extractiveness, Time, E)
    ->  true
    ;   metric_drift_events:safe_metric(C, extractiveness, E)
    ->  true
    ;   config:param(default_extractiveness, E)
    ),
    (   metric_drift_events:metric_at(C, suppression_requirement, Time, S)
    ->  true
    ;   metric_drift_events:safe_metric(C, suppression_requirement, S)
    ->  true
    ;   config:param(default_suppression, S)
    ),
    constraint_indexing:default_context(Context),
    Context = context(_, _, _, spatial_scope(Scope)),
    % Time-aware d (Type-A floor): keeps snapshot_type ≡ classify_at_time under
    % the d-threading (test_snapshot_migration). Identical to the static call on
    % the current corpus (no time-indexed source). No `backed` flag here:
    % snapshot_type is default_context-only and its sole reader is
    % degradation_chain/3 (pipeline-unwired; the residual reads
    % classify_at_time/5). Full snapshot_type ≡ classify_at_time is FALSE by
    % design — see the determinism-guard note above.
    constraint_indexing:derive_directionality_at(C, Context, Time, D),
    constraint_indexing:sigmoid_f(D, PowerMod),
    constraint_indexing:scope_modifier(Scope, ScopeMod),
    Chi is E * PowerMod * ScopeMod,
    drl_core:classify_from_metrics(C, E, Chi, S, Context, Type).

/* ================================================================
   REPAIR TRANSITIONS (OQ-91) — the UPWARD dual of transition_path/4.

   COMMENTARY-GRADE, additive-only. Detects upward (repair) runs in the
   authored snapshot_type series and names the repair operation. It comments
   on the authored numbers; it does NOT reclassify. It must NEVER feed
   classify_from_metrics/6, the signature layer, or verdict_join — its sole
   consumer is the report surface (json_report -> enhanced_report.py).

   Source: reuses the direction-neutral degradation_chain/3 reporter (the
   snapshot_type series), NOT a re-derived series. "Upward" = the transitive
   closure of the 8 transition_path/4 decay edges, read backwards; `unknown`
   is off the health ordering (OQ-37) and excluded.

   Metaphors held APART (repair_dynamics.md §3, §7): the rope/rigging line
   ops maintain | splice | replace are distinct from the scaffold
   construction op scaffold_struck; the type vocabulary does not compose.
   ================================================================ */

% Decay edges = the 8 transition_path/4 heads, as bare type pairs. Used ONLY to
% define the upward direction (kept local + auditable; if a decay head is added
% to transition_path/4, mirror it here).
repair_decay_edge(rope, tangled_rope).
repair_decay_edge(tangled_rope, snare).
repair_decay_edge(rope, piton).
repair_decay_edge(scaffold, piton).
repair_decay_edge(scaffold, snare).
repair_decay_edge(scaffold, tangled_rope).
repair_decay_edge(snare, piton).
repair_decay_edge(snare, false_mountain).

repair_decays_to(X, Y) :- repair_decay_edge(X, Y).
repair_decays_to(X, Y) :- repair_decay_edge(X, Z), repair_decays_to(Z, Y).

%% repair_upward(+From, +To)
%  To is healthier than From: From can decay down to To. `unknown` excluded.
repair_upward(From, To) :-
    From \== unknown, To \== unknown,
    repair_decays_to(To, From).

%% repair_step(+Chain, -Prefix, -A, -B)
%  Consecutive pair A->B in Chain, with the prefix of types seen BEFORE A
%  (used for round-trip / "maintain" detection).
repair_step([A,B|_], [], A, B).
repair_step([H|T], [H|Pre], A, B) :- repair_step(T, Pre, A, B).

%% repair_op(+From, +To, +Prefix, -Op)
%  Names the repair operation. A lift that RESTORES a type held earlier in the
%  series is continuous upkeep -> `maintain`; otherwise by the metaphor of the
%  state being repaired: scaffold -> scaffold_struck (construction, struck on
%  success); trap/dead-anchor (snare/piton/false_mountain) -> replace (swap the
%  line); fouled-but-intact rope (tangled_rope) -> splice (local in-place mend).
% Clause selection is driven ENTIRELY by From/To/Pre (the cut fires before Op is
% unified), so repair_op is a true function of its inputs and stays correct when
% Op is QUERIED bound (e.g. a consumer filtering by operation) — not only when
% enumerated unbound. (Guarding on a bound 4th arg would let the default clause
% mislabel a snare lift as `splice`; witnessed and fixed.)
repair_op(_From, To, Pre, Op) :- memberchk(To, Pre), !, Op = maintain.
repair_op(scaffold, _, _, Op)       :- !, Op = scaffold_struck.
repair_op(snare, _, _, Op)          :- !, Op = replace.
repair_op(piton, _, _, Op)          :- !, Op = replace.
repair_op(false_mountain, _, _, Op) :- !, Op = replace.
repair_op(tangled_rope, _, _, Op)   :- !, Op = splice.
repair_op(_, _, _, splice).

%% repair_transition(+ConstraintID, -FromType, -ToType, -RepairOp)
%  Enumerates each upward (repair) step in the constraint's snapshot_type
%  series, with the named repair operation. Multiple solutions for a multi-step
%  repair (e.g. snare->tangled_rope and tangled_rope->rope). Fails (no solution)
%  for a decay-only / flat constraint — the honest empty case (commentary-grade
%  makes an empty repair section the absence-finding for free).
repair_transition(C, From, To, Op) :-
    degradation_chain(C, Chain, _),
    repair_step(Chain, Pre, From, To),
    repair_upward(From, To),
    repair_op(From, To, Pre, Op).

%% predicted_terminal_state(+ConstraintID, -State, -Confidence)
predicted_terminal_state(C, piton, high) :-
    metric_drift_events:drift_event(C, function_obsolescence, _),
    metric_drift_events:drift_event(C, extraction_dried_up, _), !.

predicted_terminal_state(C, piton, high) :-
    metric_drift_events:drift_event(C, internalized_piton, _), !.

predicted_terminal_state(C, piton, medium) :-
    metric_drift_events:drift_event(C, sunset_violation, _), !.

predicted_terminal_state(C, snare, high) :-
    transition_path(C, tangled_rope, snare, _), !.

predicted_terminal_state(C, snare, medium) :-
    metric_drift_events:drift_event(C, extraction_accumulation, _),
    metric_drift_events:drift_event(C, coordination_loss, _), !.

predicted_terminal_state(C, tangled_rope, medium) :-
    transition_path(C, rope, tangled_rope, _), !.

predicted_terminal_state(C, tangled_rope, low) :-
    metric_drift_events:drift_event(C, extraction_accumulation, _),
    narrative_ontology:has_coordination_function(C), !.

predicted_terminal_state(_, stable, low).

/* ================================================================
   HELPER PREDICATES
   ================================================================ */

%% deduplicate_consecutive(+List, -Deduped)
%  Removes consecutive duplicates: [a,a,b,b,a] -> [a,b,a]
deduplicate_consecutive([], []).
deduplicate_consecutive([X], [X]).
deduplicate_consecutive([X,X|Rest], Deduped) :-
    !, deduplicate_consecutive([X|Rest], Deduped).
deduplicate_consecutive([X,Y|Rest], [X|Deduped]) :-
    X \= Y,
    deduplicate_consecutive([Y|Rest], Deduped).

%% pairs_values(+Pairs, -Values)
pairs_values([], []).
pairs_values([_-V|Rest], [V|Vs]) :- pairs_values(Rest, Vs).
