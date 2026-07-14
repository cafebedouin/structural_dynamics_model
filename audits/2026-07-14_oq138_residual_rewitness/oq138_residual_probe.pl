:- initialization(main).

main :-
    current_prolog_flag(argv, Argv),
    ( Argv = [CorpusAtom|_] -> Corpus = CorpusAtom ; Corpus = testsets ),
    [stack],
    % Overlay corpus_path BEFORE loading (retract default then asserta — Corpus Loading rule)
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, Corpus)),
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    aggregate_all(count, corpus_loader:corpus_constraint(_), Total),
    findall(C-Sig-MT-FT,
            ( corpus_loader:corpus_constraint(C),
              drl_core:metric_based_type_indexed(C, Ctx, MT),
              drl_core:dr_type(C, Ctx, FT),
              MT \== FT,
              ( signature_detection:constraint_signature(C, Sig) -> true ; Sig = none )
            ),
            Changers),
    length(Changers, N),
    ResidualSigs = [coordination_scaffold, constructed_low_extraction,
                    constructed_constraint, constructed_high_extraction],
    findall(C2-S2-MT2-FT2,
            ( member(C2-S2-MT2-FT2, Changers),
              memberchk(S2, ResidualSigs),
              ( S2 == constructed_high_extraction -> MT2 == mountain ; true )
            ),
            Residual),
    length(Residual, RN),
    findall(x, member(_-coupling_invariant_rope-_-_, Changers), CiRope),
    length(CiRope, CN),
    format("~n########## CORPUS=~w  (N_constraints=~w) ##########~n", [Corpus, Total]),
    format("  total metric!=final changers: ~w~n", [N]),
    format("  RESIDUAL live-changers (seven unconverted clauses): ~w~n", [RN]),
    forall(member(RC-RS-RMT-RFT, Residual),
           format("     RESIDUAL >> ~w  sig=~w  ~w -> ~w~n", [RC, RS, RMT, RFT])),
    format("  [control] coupling_invariant_rope (KEEP, overwrites): ~w~n", [CN]),
    ( CN > 0 -> format("  [control OK] probe detects overwrites on this corpus~n", [])
    ; format("  [control ZERO] no CI-rope seats here; residual-zero is weaker on this leg~n", []) ),
    halt.
main :- format("PROBE FAILED~n", []), halt(1).
