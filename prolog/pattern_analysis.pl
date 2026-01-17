:- module(pattern_analysis, [
    analyze_interval/1,
    interval_system_gradient/3,
    interval_data_completeness/2,
    interval_preliminary_pattern/2
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection).

:- dynamic interval_system_gradient/3.
:- dynamic interval_data_completeness/2.
:- dynamic interval_preliminary_pattern/2.

analyze_interval(IntervalID) :-
    interval(IntervalID, T0, _),
    retractall(interval_system_gradient(IntervalID, _, _)),
    retractall(interval_data_completeness(IntervalID, _)),
    retractall(interval_preliminary_pattern(IntervalID, _)),
    
    % Explicitly call the math module to solve the warning
    v3_1_coercion_projection:system_gradient(IntervalID, T0, Gsys),
    assertz(interval_system_gradient(IntervalID, coercion, Gsys)),
    
    compute_completeness(IntervalID, Score),
    assertz(interval_data_completeness(IntervalID, Score)),
    
    param(system_gradient_threshold, Thr),
    (Gsys > Thr -> P = increasing_coercion ; Gsys < -Thr -> P = decreasing_coercion ; P = stable),
    assertz(interval_preliminary_pattern(IntervalID, P)).

compute_completeness(ID, Score) :-
    interval(ID, T0, Tn),
    % Explicit call to solve warning
    findall((L, T), (level(L), member(T, [T0, Tn]), v3_1_coercion_projection:coercion_vector(L, T, _)), Vectors),
    length(Vectors, N),
    Score is N / 8.

