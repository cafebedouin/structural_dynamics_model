% Phase B — P1, P5, P7, P8 corpus probes on a tier.

run(Tier) :-
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Tier)),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format('~n=== TIER ~w (corpus_constraint=~w) ===~n', [Tier, NC]),

    % ---- P1: is self_enforcing ruled OUT of drift/cover-story/masking? ----
    format('~n--- P1: self_enforcing constraints & their drift/cover-story exposure ---~n'),
    forall(
        ( corpus_loader:corpus_constraint(C), cs_pattern_detection:cs_pattern_is(C, natural_law_constraint) ),
        ( ( cs_pattern_detection:cs_cover_story_active(C,_) -> CS = cover_story ; CS = '-' ),
          ( cs_pattern_detection:cs_authority_masking(C,_,_) -> MK = masking ; MK = '-' ),
          ( narrative_ontology:cs_story_uid(C,U), cs_drift_mismatch:cs_drift_mismatch(U,_) -> DM = drift_mismatch ; DM = '-' ),
          format('  self_enforcing ~w | ~w ~w ~w~n', [C, CS, MK, DM])
        )),

    % ---- P5: beneficiary discriminator predicates ----
    format('~n--- P5: cs_naturalized_mountain (beneficiary+victim) firings ---~n'),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), cs_pattern_detection:cs_naturalized_mountain(C) ), NNM),
    format('  cs_naturalized_mountain: ~w~n', [NNM]),

    % ---- P7: genuine natural_law (computed signature) + concealing co-fire? ----
    format('~n--- P7: genuine natural_law SIGNATURE constraints & concealing co-fire ---~n'),
    findall(C, ( corpus_loader:corpus_constraint(C), signature_detection:constraint_signature(C, natural_law) ), GNL),
    length(GNL, NGNL),
    format('  genuine natural_law signature count: ~w~n', [NGNL]),
    forall(member(C, GNL),
        ( ( cs_pattern_detection:cs_cover_story_active(C,_) -> CS = cover_story ; CS = '-' ),
          ( cs_pattern_detection:cs_verdict(C,V) -> true ; V = no_verdict ),
          ( catch(signature_detection:false_natural_law(C,_),_,fail) -> FNL = sigFNL_fires ; FNL = '-' ),
          format('  genuine_NL ~w | cover_story=~w cs_verdict=~w ~w~n', [C, CS, V, FNL])
        )),

    % ---- P8: epistemic_consensus pattern present but NO verdict predicate ----
    format('~n--- P8: epistemic_consensus pattern count & any verdict ---~n'),
    aggregate_all(count, ( corpus_loader:corpus_constraint(C), cs_pattern_detection:cs_pattern_is(C, epistemic_consensus) ), NEC),
    format('  epistemic_consensus pattern: ~w~n', [NEC]),
    ( ( corpus_loader:corpus_constraint(C), cs_pattern_detection:cs_pattern_is(C, epistemic_consensus),
        cs_pattern_detection:cs_verdict(C, EV) )
    ->  format('  WARN: epistemic_consensus carried a cs_verdict ~w (~w)~n', [EV, C])
    ;   format('  epistemic_consensus carries NO cs_verdict (as predicted; no verdict clause exists)~n', []) ),
    format('~n=== END ~w ===~n', [Tier]).
