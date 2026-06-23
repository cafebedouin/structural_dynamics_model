% OQ-112 Round-2 — witness-truth control, refinement: WHICH phase throws, and a
% LITERAL mid-index thrower. maxent_precompute order is:
%   maxent_compute_profiles -> maxent_compute_priors -> maxent_classify_all -> assertz(run_info)
% We separate the two failure surfaces so the "constraint K of N" claim is honest.

:- [stack].
:- corpus_loader:load_all_testsets.

three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).

main :-
    constraint_indexing:default_context(Ctx),
    % use a thrower that sorts to the MIDDLE of discovery (not index 1)
    THROWER = polaris_document_status_contradictions,

    % ---- A. PROFILE-PHASE probe: does maxent_compute_profiles throw on the thrower? ----
    setup_call_cleanup(
        assertz(narrative_ontology:constraint_claim(THROWER, throw_test_claim)),
        ( maxent_classifier:maxent_cleanup,
          findall(C, (narrative_ontology:constraint_claim(C,_), \+ is_list(C), atom(C)), D0),
          sort(D0, Disc), length(Disc, Ntot),
          ( nth1(K, Disc, THROWER) -> true ; K = -1 ),
          format('PHASE: discovery N=~w ; THROWER at index K=~w (mid-list)~n', [Ntot, K]),
          three_way(maxent_classifier:maxent_compute_profiles(Disc, Ctx), RP),
          format('PHASE A maxent_compute_profiles(full list incl thrower) -> ~w~n', [RP]),

          % ---- B. CLASSIFY-PHASE probe: profiles CLEAN first, then classify_all incl thrower ----
          maxent_classifier:maxent_cleanup,
          findall(C2, (narrative_ontology:constraint_claim(C2,_), \+ is_list(C2), atom(C2),
                       C2 \== THROWER), CleanList0),
          sort(CleanList0, CleanList),
          maxent_classifier:maxent_compute_profiles(CleanList, Ctx),   % profiles from the clean 86
          maxent_classifier:maxent_compute_priors(CleanList),
          ( maxent_classifier:maxent_profile(snare, suppression, Ctx, _)
            -> format('PHASE B profiles PRESENT before classify (good setup)~n', [])
            ;  format('PHASE B profiles ABSENT (setup failed)~n', []) ),
          three_way(maxent_classifier:maxent_classify_all([THROWER], Ctx), RC),
          format('PHASE B maxent_classify_all([thrower], profiles present) -> ~w~n', [RC]),

          % ---- C. REAL driver with mid-index thrower: run_info absent + count ----
          maxent_classifier:maxent_cleanup,
          three_way(maxent_classifier:maxent_run(Ctx, _S), RR),
          aggregate_all(count, maxent_classifier:maxent_run_info(_,_,_), NRI),
          ( maxent_classifier:maxent_run_info(Ctx,_,_)
            -> format('PHASE C real maxent_run (thrower at K=~w) -> ~w ; run_info PRESENT <-- BAD~n', [K, RR])
            ;  format('PHASE C real maxent_run (thrower at K=~w) -> ~w ; run_info ABSENT, count=~w <-- GOOD~n', [K, RR, NRI]) )
        ),
        retractall(narrative_ontology:constraint_claim(THROWER, throw_test_claim))
    ),
    halt.

:- initialization(main).
