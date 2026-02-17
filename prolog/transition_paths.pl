% ============================================================================
% TRANSITION PATHS — Degradation Path Detection & Terminal State Prediction
% Split from drl_lifecycle.pl
% ============================================================================

:- module(transition_paths, [
    transition_path/4,
    degradation_chain/3,
    predicted_terminal_state/3
]).

:- use_module(library(lists), [last/2]).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(drift_events).

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
    drift_events:safe_metric(C, extractiveness, E),
    config:param(rope_epsilon_ceiling, Ceil),
    E > Ceil * 0.7,
    narrative_ontology:has_coordination_function(C),
    drift_events:metric_trend(C, base_extractiveness, increasing).

% Tangled Rope -> Snare (coordination dying while extraction grows)
transition_path(C, tangled_rope, snare, evidence(coordination_declining, true, extraction, E)) :-
    drl_core:dr_type(C, Type),
    Type = tangled_rope,
    drift_events:safe_metric(C, extractiveness, E),
    config:param(snare_epsilon_floor, Floor),
    E > Floor * 0.8,
    (   drift_events:drift_event(C, coordination_loss, _)
    ;   drift_events:metric_trend(C, coordination_effectiveness, decreasing)
    ).

% Rope -> Piton (direct obsolescence, no extraction phase)
transition_path(C, rope, piton, evidence(function_obsolete, true, theater_high, TR)) :-
    drl_core:dr_type(C, Type),
    Type = rope,
    drift_events:safe_metric(C, theater_ratio, TR),
    TR > 0.5,
    drift_events:drift_event(C, function_obsolescence, _).

% Scaffold -> Piton (sunset violation)
transition_path(C, scaffold, piton, evidence(sunset_violated, true)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    drift_events:drift_event(C, sunset_violation, _).

% Scaffold -> Snare (calcification with extraction)
transition_path(C, scaffold, snare, evidence(extraction_added, E, sunset_violated, Violated)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    drift_events:safe_metric(C, extractiveness, E),
    config:param(snare_epsilon_floor, Floor),
    E > Floor * 0.7,
    (   drift_events:drift_event(C, sunset_violation, _)
    ->  Violated = true
    ;   Violated = false
    ).

% Scaffold -> Tangled Rope (extraction added during transition)
transition_path(C, scaffold, tangled_rope, evidence(extraction_emerging, E, coordination_intact, true)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    drift_events:safe_metric(C, extractiveness, E),
    config:param(tangled_rope_epsilon_floor, Floor),
    E > Floor * 0.7,
    narrative_ontology:has_coordination_function(C).

% Snare -> I-Piton (internalization)
transition_path(C, snare, piton, evidence(internalized, true)) :-
    drl_core:dr_type(C, Type),
    Type = snare,
    drift_events:drift_event(C, internalized_piton, _).

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
             classify_snapshot(C, T, Type)),
            RawPairs),
    RawPairs \= [],
    sort(RawPairs, Sorted),
    pairs_values(Sorted, TypeList),
    deduplicate_consecutive(TypeList, Chain),
    length(Chain, Len),
    Len > 1,
    Sorted = [T1-_|_],
    last(Sorted, T2-_).

%% classify_snapshot(+C, +Time, -Type)
%  Classifies a constraint at a specific time using measurements available.
classify_snapshot(C, Time, Type) :-
    (   drift_events:metric_at(C, base_extractiveness, Time, E)
    ->  true
    ;   drift_events:safe_metric(C, extractiveness, E)
    ->  true
    ;   config:param(default_extractiveness, E)
    ),
    (   drift_events:metric_at(C, suppression_requirement, Time, S)
    ->  true
    ;   drift_events:safe_metric(C, suppression_requirement, S)
    ->  true
    ;   config:param(default_suppression, S)
    ),
    constraint_indexing:default_context(Context),
    Context = context(agent_power(Power), _, _, _),
    constraint_indexing:power_modifier(Power, Modifier),
    Chi is E * Modifier,
    drl_core:classify_from_metrics(C, E, Chi, S, Context, Type).

%% predicted_terminal_state(+ConstraintID, -State, -Confidence)
predicted_terminal_state(C, piton, high) :-
    drift_events:drift_event(C, function_obsolescence, _),
    drift_events:drift_event(C, extraction_dried_up, _), !.

predicted_terminal_state(C, piton, high) :-
    drift_events:drift_event(C, internalized_piton, _), !.

predicted_terminal_state(C, piton, medium) :-
    drift_events:drift_event(C, sunset_violation, _), !.

predicted_terminal_state(C, snare, high) :-
    transition_path(C, tangled_rope, snare, _), !.

predicted_terminal_state(C, snare, medium) :-
    drift_events:drift_event(C, extraction_accumulation, _),
    drift_events:drift_event(C, coordination_loss, _), !.

predicted_terminal_state(C, tangled_rope, medium) :-
    transition_path(C, rope, tangled_rope, _), !.

predicted_terminal_state(C, tangled_rope, low) :-
    drift_events:drift_event(C, extraction_accumulation, _),
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
