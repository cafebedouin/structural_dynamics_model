:- initialization(main).
main :-
    [stack],
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    format("=== Constraints hitting BaseX=0.5 default at Time=0 (live cs_kernel path) ===~n"),
    findall(C-Type,
        ( corpus_loader:corpus_constraint(C),
          % classify_at_time_with_supp is reached iff suppression authored (temporal or scalar)
          ( narrative_ontology:measurement(_, C, suppression_requirement, 0, _)
          ; narrative_ontology:constraint_metric(C, suppression_requirement, _) ),
          % base_extractiveness NOT temporally authored at T=0 -> hits 0.5 default
          \+ narrative_ontology:measurement(_, C, base_extractiveness, 0, _),
          once(drl_composition:classify_at_time(C, 0, Ctx, Type)) ),
        Hits),
    length(Hits, N),
    format("count = ~w~n", [N]),
    forall(member(C-T, Hits),
        ( ( narrative_ontology:constraint_metric(C, base_extractiveness, BE) -> BEs = BE ; BEs = none),
          ( narrative_ontology:cs_kernel_id(C, K) -> Ks = K ; Ks = no_kernel),
          format("  ~w  type@0=~w  scalar_base_extr=~w  kernel=~w~n", [C, T, BEs, Ks]) )),
    halt.
main :- format("PROBE FAILED~n"), halt(1).
