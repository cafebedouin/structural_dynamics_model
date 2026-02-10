:- module(pattern_analysis, [
    analyze_interval/1,
    analyze_interval/4,          % NEW: Pure return-value API
    interval_system_gradient/3,
    interval_data_completeness/2,
    interval_preliminary_pattern/2
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(coercion_projection).

:- dynamic interval_system_gradient/3.
:- dynamic interval_data_completeness/2.
:- dynamic interval_preliminary_pattern/2.

%% analyze_interval(+IntervalID, -Gradient, -Completeness, -Pattern)
%  Pure return-value API. Computes interval analysis results without
%  side effects. Callers can use results directly without querying
%  dynamic predicates.
analyze_interval(IntervalID, Gradient, Completeness, Pattern) :-
    narrative_ontology:interval(IntervalID, T0, _),
    coercion_projection:system_gradient(IntervalID, T0, Gradient),
    compute_completeness(IntervalID, Completeness),
    config:param(system_gradient_threshold, Thr),
    (Gradient > Thr -> Pattern = increasing_coercion
    ; Gradient < -Thr -> Pattern = decreasing_coercion
    ; Pattern = stable).

%% analyze_interval(+IntervalID)
%  Legacy API. Computes results and asserts them as dynamic facts
%  for backward compatibility with code that queries
%  interval_system_gradient/3, interval_data_completeness/2,
%  and interval_preliminary_pattern/2.
analyze_interval(IntervalID) :-
    analyze_interval(IntervalID, Gradient, Completeness, Pattern),
    retractall(interval_system_gradient(IntervalID, _, _)),
    retractall(interval_data_completeness(IntervalID, _)),
    retractall(interval_preliminary_pattern(IntervalID, _)),
    assertz(interval_system_gradient(IntervalID, coercion, Gradient)),
    assertz(interval_data_completeness(IntervalID, Completeness)),
    assertz(interval_preliminary_pattern(IntervalID, Pattern)).

compute_completeness(ID, Score) :-
    narrative_ontology:interval(ID, T0, Tn),
    findall((L, T), (config:level(L), member(T, [T0, Tn]), coercion_projection:coercion_vector(L, T, _)), Vectors),
    length(Vectors, N),
    Score is N / 8.
