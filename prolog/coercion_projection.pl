:- module(coercion_projection, [
    coercion_vector/4,
    coercion_magnitude/4,
    coercion_gradient/4,
    system_gradient/3,
    time_point_in_interval/2
]).

:- use_module(library(lists)).        % Required for sum_list/2
:- use_module(narrative_ontology).
:- use_module(config).

% INTERVAL SCOPING (OQ-93 ruling (b), build unit 1, 2026-06-10): coercion_vector
% and coercion_magnitude previously read measurement/5 with the INTERVAL
% ANONYMOUS — on a multi-story KB the reads leaked across constraints
% (witnessed: completeness = 312.5 on the loaded corpus, REDUNDANCY_DIFF.md).
% Single-story-safe only was never declared anywhere; it is now structural:
% every read carries the IntervalID. Arity /3 -> /4 (both external callers
% updated: pattern_analysis:compute_completeness, report_generator SECTION 6).

% Base Vector (interval-scoped)
coercion_vector(IntervalID, Level, Time, [A, S, U, R]) :-
    measurement(_, IntervalID, accessibility_collapse(Level), Time, A),
    measurement(_, IntervalID, stakes_inflation(Level),      Time, S),
    measurement(_, IntervalID, suppression(Level),           Time, U),
    measurement(_, IntervalID, resistance(Level),            Time, R).

% Magnitude logic (interval-scoped)
coercion_magnitude(IntervalID, Level, Time, Kappa) :-
    coercion_vector(IntervalID, Level, Time, [A, S, U, R]),
    aggregation_weights(Level, WA, WS, WU, WR),
    Kappa is (WA * A) + (WS * S) + (WU * U) + (WR * R).

% Time points helper.
% OQ-93 probe finding (2026-06-10): this clause ended in `!`, added as an
% "(Optimized)" change — which made the predicate FIRST-SOLUTION-ONLY (always
% T_start). Its sole caller (coercion_gradient's setof below) needs
% enumeration of future time points, so every gradient lookup failed and
% system_gradient's []-fallback emitted 0.0 — a success-shaped zero — for as
% long as the cut existed, INCLUDING the shim era. Witness: probe pre-fix run
% (hand-authored +0.588 gradient read as 0.0000),
% audits/2026-06-10_oq93_grid_viability_probe/runs/. Cut removed.
time_point_in_interval(IntervalID, Time) :-
    interval(IntervalID, T_start, T_end),
    % Find all unique times present in THIS interval's measurements
    setof(T, is_measurement_time(IntervalID, T), AllTimes),
    member(Time, AllTimes),
    Time >= T_start,
    Time =< T_end.

% Helper to isolate the cross-module dynamic call (interval-scoped: a story's
% time points are ITS measurement times, not the union across the loaded KB)
is_measurement_time(IntervalID, T) :-
    narrative_ontology:measurement(_, IntervalID, _, T, _).

% Gradient logic (Guarded)
coercion_gradient(Level, IntervalID, T_now, Grad) :-
    interval(IntervalID, _, T_end),
    T_now < T_end,
    % Use setof to find future points but wrap in a conditional to prevent looping
    (   setof(T_next, 
              (time_point_in_interval(IntervalID, T_next), T_next > T_now), 
              FuturePoints)
    ->  FuturePoints = [T_next|_], % Take the immediate next point
        coercion_magnitude(IntervalID, Level, T_now, K_now),
        coercion_magnitude(IntervalID, Level, T_next, K_next),
        Grad is K_next - K_now,
        ! % Prevent backtracking into the setof search
    ;   fail % Explicitly fail if no future points exist in the interval
    ).

% System Gradient (Safe Aggregation)
system_gradient(IntervalID, Time, SysGrad) :-
    findall(WG,
        ( level(L),
          influence_weight(L, W),
          % Ensure coercion_gradient succeeds before calculating
          coercion_gradient(L, IntervalID, Time, G),
          WG is W * G
        ),
        WGList),
    % Guard against empty lists at the end of a timeline
    (   WGList \= []
    ->  sum_list(WGList, SysGrad), !
    ;   SysGrad = 0.0 % Return neutral gradient if no changes are detected
    ).
