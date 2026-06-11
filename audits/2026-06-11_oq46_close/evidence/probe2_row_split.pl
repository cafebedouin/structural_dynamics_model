/* OQ-46 row-level probe: scalar-STOPGAP rows split by whether the constraint
   authors a temporal suppression series at all (time-grid alignment gap) vs
   scalar-only. Run from prolog/:
   swipl -g "['<this file>'], run_probe, halt" -t "halt(1)"                   */
:- [stack].
row_time(C, T) :- narrative_ontology:measurement(_, C, _, T, _).
run_probe :-
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    findall(C-T, ( member(C, Cs), setof(Tm, row_time(C, Tm), Ts), member(T, Ts) ), Rows),
    length(Rows, NRows),
    findall(C-T, ( member(C-T, Rows),
                   narrative_ontology:measurement(_, C, suppression_requirement, T, _) ), TempRows0),
    sort(TempRows0, TempRows), length(TempRows, NTemp),
    findall(C-T, ( member(C-T, Rows),
                   \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _),
                   once(narrative_ontology:constraint_metric(C, suppression_requirement, _)),
                   once(narrative_ontology:measurement(_, C, suppression_requirement, _, _)) ), GapRows0),
    sort(GapRows0, GapRows), length(GapRows, NGap),
    findall(C-T, ( member(C-T, Rows),
                   \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _),
                   once(narrative_ontology:constraint_metric(C, suppression_requirement, _)),
                   \+ narrative_ontology:measurement(_, C, suppression_requirement, _, _) ), ScalRows0),
    sort(ScalRows0, ScalRows), length(ScalRows, NScal),
    format("rows total: ~w~n", [NRows]),
    format("temporal-branch rows: ~w~n", [NTemp]),
    format("STOPGAP rows, constraint HAS a series (alignment gap): ~w~n", [NGap]),
    format("STOPGAP rows, constraint scalar-only: ~w~n", [NScal]),
    findall(C, member(C-_, GapRows), GCs0), sort(GCs0, GCs),
    forall(member(C, GCs),
           ( findall(T, member(C-T, GapRows), Ts),
             findall(ST, narrative_ontology:measurement(_, C, suppression_requirement, ST, _), STs0),
             sort(STs0, STs),
             format("  ~w: supp series at ~w; gap rows at ~w~n", [C, STs, Ts]) )).
