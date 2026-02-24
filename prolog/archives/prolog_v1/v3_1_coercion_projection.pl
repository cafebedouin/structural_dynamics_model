:- module(v3_1_coercion_projection, [
    coercion_vector/3,
    coercion_magnitude/3,
    coercion_gradient/4,
    system_gradient/3,
    time_point_in_interval/2
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).

% Base Vector
coercion_vector(Level, Time, [A, S, U, R]) :-
    measurement(_, _, accessibility_collapse(Level), Time, A),
    measurement(_, _, stakes_inflation(Level),      Time, S),
    measurement(_, _, suppression(Level),           Time, U),
    measurement(_, _, resistance(Level),            Time, R).

% Magnitude logic
coercion_magnitude(Level, Time, Kappa) :-
    coercion_vector(Level, Time, [A, S, U, R]),
    aggregation_weights(Level, WA, WS, WU, WR),
    Kappa is (WA * A) + (WS * S) + (WU * U) + (WR * R).

% Time points helper
time_point_in_interval(IntervalID, Time) :-
    interval(IntervalID, T_start, T_end),
    setof(T, (measurement(_, _, _, T, _), T >= T_start, T =< T_end), TimePoints),
    member(Time, TimePoints).

% Gradient logic
coercion_gradient(Level, IntervalID, T_now, Grad) :-
    interval(IntervalID, _, T_end),
    T_now < T_end,
    setof(T_next, (time_point_in_interval(IntervalID, T_next), T_next > T_now), FuturePoints),
    FuturePoints = [T_next|_],
    coercion_magnitude(Level, T_now, K_now),
    coercion_magnitude(Level, T_next, K_next),
    Grad is K_next - K_now.

% System Gradient
system_gradient(IntervalID, Time, SysGrad) :-
    findall(WG,
        ( level(L),
          influence_weight(L, W),
          coercion_gradient(L, IntervalID, Time, G),
          WG is W * G
        ),
        WGList),
    WGList \= [],
    sum_list(WGList, SysGrad).

