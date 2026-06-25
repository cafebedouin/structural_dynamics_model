:- initialization(main).
main :-
    [stack],
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    % POSITIVE CONTROL: constraints WITH temporal base_extractiveness at T=0
    % must NOT hit the fail-close arm -> must return a non-unknown computed type.
    findall(C, ( corpus_loader:corpus_constraint(C),
                 narrative_ontology:measurement(_, C, base_extractiveness, 0, _) ), WithEps),
    length(WithEps, NPos),
    format("POSITIVE CONTROL: ~w constraints have base_extractiveness@T=0~n", [NPos]),
    ( NPos > 0
    -> findall(C-T, ( member(C, WithEps),
                      once(drl_composition:classify_at_time(C, 0, Ctx, T)) ), Pos),
       include([_-unknown]>>true, Pos, PosUnknown),
       length(PosUnknown, NPU),
       format("  of those, returning unknown at T=0: ~w (MUST be 0 if ε authored => computed)~n", [NPU]),
       ( member(Ex-ExT, Pos), ExT \== unknown
       -> format("  example authored-ε control: ~w -> type@0=~w (computed, NOT fail-closed)~n", [Ex, ExT])
       ;  format("  (no non-unknown example)~n") )
    ;  format("  NO positive-control constraints in corpus -- control vacuous, ESCALATE~n") ),
    % NEGATIVE SIDE: the 15 ε-absent constraints must now ALL be unknown at T=0
    findall(C-T,
        ( corpus_loader:corpus_constraint(C),
          ( narrative_ontology:measurement(_, C, suppression_requirement, 0, _)
          ; narrative_ontology:constraint_metric(C, suppression_requirement, _) ),
          \+ narrative_ontology:measurement(_, C, base_extractiveness, 0, _),
          once(drl_composition:classify_at_time(C, 0, Ctx, T)) ),
        Neg),
    length(Neg, NNeg),
    include([_-unknown]>>true, Neg, NegUnknown),
    length(NegUnknown, NNU),
    format("NEGATIVE SIDE: ~w ε-absent constraints at T=0; now unknown: ~w (MUST equal ~w)~n", [NNeg, NNU, NNeg]),
    halt.
main :- format("CONTROL PROBE FAILED~n"), halt(1).
