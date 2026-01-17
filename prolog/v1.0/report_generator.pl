:- module(report_generator, [generate_full_report/1]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection).
:- use_module(pattern_analysis).
:- use_module(intent_engine).
:- use_module(constraint_bridge).

generate_full_report(IntervalID) :-
    interval(IntervalID, T_start, Tn),
    classify_interval(IntervalID, Pattern, Conf),
    
    format('~n~n====================================================~n'),
    format('   v3.1 STRUCTURAL ANALYSIS EXECUTIVE SUMMARY       ~n'),
    format('====================================================~n'),
    format('Timeline:       ~w to ~w~n', [T_start, Tn]),
    format('Classification: ~w (Confidence: ~w)~n', [Pattern, Conf]),
    
    % Use explicit math engine calls
    findall(Kappa, (level(L), v3_1_coercion_projection:coercion_magnitude(L, Tn, Kappa)), Kappas),
    (Kappas \= [] ->
        sum_list(Kappas, Sum), length(Kappas, N), AvgK is Sum / N,
        format('  Aggregate Magnitude (Kappa) at Tn: ~2f~n', [AvgK])
    ;   true),

    dr_diagnostic_report(IntervalID).

