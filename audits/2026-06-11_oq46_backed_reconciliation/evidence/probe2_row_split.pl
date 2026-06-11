:- [stack].
:- corpus_loader:load_all_testsets.

row(C, T) :-
    corpus_loader:corpus_constraint(C),
    temporal_residual:constraint_time_set(C, Times),
    member(T, Times).

run :-
    aggregate_all(count, row(_,_), NRows),
    format("total_rows = ~w~n", [NRows]),
    aggregate_all(count, (row(C,T),
        narrative_ontology:measurement(_, C, suppression_requirement, T, _)), NTemp),
    format("rows_temporal_supp = ~w~n", [NTemp]),
    aggregate_all(count, (row(C,T),
        \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _),
        narrative_ontology:constraint_metric(C, suppression_requirement, _)), NScal),
    format("rows_scalar_stopgap = ~w~n", [NScal]),
    aggregate_all(count, (row(C,T),
        \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _),
        \+ narrative_ontology:constraint_metric(C, suppression_requirement, _)), NUnk),
    format("rows_unknown = ~w~n", [NUnk]),
    % Split scalar rows: constraint has NO suppression series at all (static story)
    aggregate_all(count, (row(C,T),
        \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _),
        narrative_ontology:constraint_metric(C, suppression_requirement, _),
        \+ narrative_ontology:measurement(_, C, suppression_requirement, _, _)), NStatic),
    format("rows_scalar_in_seriesless_constraint = ~w~n", [NStatic]),
    % vs constraint HAS a suppression series but not at this T (misalignment)
    aggregate_all(count, (row(C,T),
        \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _),
        narrative_ontology:constraint_metric(C, suppression_requirement, _),
        once(narrative_ontology:measurement(_, C, suppression_requirement, _, _))), NMis),
    format("rows_scalar_misalignment = ~w~n", [NMis]),
    % name the constraints in each bucket
    aggregate_all(set(C), (row(C,_),
        \+ narrative_ontology:measurement(_, C, suppression_requirement, _, _),
        narrative_ontology:constraint_metric(C, suppression_requirement, _)), StaticCs),
    format("seriesless_scalar_constraints: ~w~n", [StaticCs]),
    aggregate_all(set(C), (row(C,T),
        \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _),
        narrative_ontology:constraint_metric(C, suppression_requirement, _),
        once(narrative_ontology:measurement(_, C, suppression_requirement, _, _))), MisCs),
    format("misalignment_constraints: ~w~n", [MisCs]),
    % which metrics author the extra time-points in misalignment rows
    aggregate_all(set(M), (row(C,T),
        \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _),
        narrative_ontology:constraint_metric(C, suppression_requirement, _),
        once(narrative_ontology:measurement(_, C, suppression_requirement, _, _)),
        narrative_ontology:measurement(_, C, M, T, _)), MisMetrics),
    format("metrics_authoring_offgrid_times: ~w~n", [MisMetrics]).
