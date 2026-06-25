:- initialization(main).
main :-
    [stack], corpus_loader:ensure_corpus_loaded,
    findall(C, ( corpus_loader:corpus_constraint(C),
                 ( narrative_ontology:measurement(_, C, suppression_requirement, 0, _)
                 ; narrative_ontology:constraint_metric(C, suppression_requirement, _) ),
                 \+ narrative_ontology:measurement(_, C, base_extractiveness, 0, _) ), Hits),
    format("=== classification of the 15 T=0 ε-absent constraints ===~n"),
    forall(member(C, Hits),
      ( findall(T, narrative_ontology:measurement(_, C, base_extractiveness, T, _), Ts),
        ( narrative_ontology:constraint_metric(C, base_extractiveness, _) -> Sc=scalar ; Sc=no_scalar ),
        ( Ts == [] -> Kind=GENUINELY_ABSENT ; Kind=offgrid_series ),
        format("  ~w | base_extr_times=~w | ~w | ~w~n", [C, Ts, Sc, Kind]) )),
    length(Hits, N),
    aggregate_all(count, ( member(C,Hits),
        \+ narrative_ontology:measurement(_,C,base_extractiveness,_,_),
        \+ narrative_ontology:constraint_metric(C,base_extractiveness,_) ), GenAbsent),
    format("~n total=~w  genuinely-absent (no ε anywhere)=~w  off-grid-series-or-scalar=~w~n",
           [N, GenAbsent, N-GenAbsent]),
    halt.
main :- write(FAIL),nl,halt(1).
