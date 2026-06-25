:- initialization(main).
main :-
    [stack], corpus_loader:ensure_corpus_loaded,
    forall(member(R, [jewish_sovereignty_palestine__settler_colonial_reading,
                      jewish_sovereignty_palestine__cultural_zionist_reading]),
        ( ( narrative_ontology:constraint_metric(R, base_extractiveness, BE) -> BEs=BE ; BEs=ABSENT ),
          findall(T-V, narrative_ontology:measurement(_, R, base_extractiveness, T, V), Series),
          ( narrative_ontology:constraint_metric(R, suppression_requirement, S) -> Ss=S ; Ss=ABSENT ),
          format("~w~n  scalar base_extractiveness = ~w~n  temporal base_extractiveness series = ~w~n  suppression_requirement scalar = ~w~n",
                 [R, BEs, Series, Ss]) )),
    halt.
main :- write('FAIL'), nl, halt(1).
