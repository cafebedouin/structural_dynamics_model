% OQ-112 Round-2 — WITNESS-TRUTH CONTROL (the premature-assertion control).
%
% The gate's entire correctness rests on maxent_run_info being asserted ONLY on
% genuine full completion. If it were asserted BEFORE the per-constraint loop can
% throw, a mid-loop throw would already have written its completion witness -> the
% gate reads it clean -> Pattern 6 reconstituted inside its own fix.
%
% This control FORCES a mid-loop throw (constraint K of N) by giving one of the
% proven-throwing unknown-suppression constraints a constraint_claim so it ENTERS
% maxent_run's discovery loop and hits the Gaussian-LL sink. Then it probes whether
% maxent_run_info(Ctx,_,_) got asserted for that Context anyway.
%
% Required pasted witness: "throw at constraint K of N -> maxent_run_info ABSENT".
% Two-sided: a CLEAN run must leave run_info PRESENT (else absence proves nothing).
%
% Read-only w.r.t. the engine: the only mutation is a test-local constraint_claim
% fact, asserted then retracted (no engine source edited).

:- [stack].
:- corpus_loader:load_all_testsets.

three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).

run_info_count(N) :-
    aggregate_all(count, maxent_classifier:maxent_run_info(_,_,_), N).

main :-
    THROWER = actinide_replenishment_mechanism_contradictions,
    constraint_indexing:default_context(Ctx),

    % ===== POSITIVE CONTROL (clean run): run_info MUST be present =====
    maxent_classifier:maxent_cleanup,
    three_way(maxent_classifier:maxent_run(Ctx, S0), R0),
    run_info_count(N0),
    format('CTRL clean: maxent_run -> ~w ; run_info_count=~w  (expect success / >=1)~n', [R0, N0]),
    format('CTRL clean: summary=~w~n~n', [S0]),

    % confirm THROWER is currently claim-less (excluded) and the engine is clean
    ( narrative_ontology:constraint_claim(THROWER, _)
      -> format('PRE: ~w already has a claim (unexpected)~n', [THROWER])
      ;  format('PRE: ~w is claim-less (excluded from discovery) -- as expected~n', [THROWER]) ),

    % ===== FORCED MID-LOOP THROW: inject a claim so THROWER enters discovery =====
    setup_call_cleanup(
        assertz(narrative_ontology:constraint_claim(THROWER, throw_test_claim)),
        ( maxent_classifier:maxent_cleanup,
          % how many constraints does discovery now see, and at what index is THROWER?
          findall(C, (narrative_ontology:constraint_claim(C,_), \+ is_list(C), atom(C)), D0),
          sort(D0, Disc), length(Disc, Ntot),
          ( nth1(K, Disc, THROWER) -> true ; K = -1 ),
          format('FORCED: discovery N=~w ; THROWER ~w at index K=~w of N~n', [Ntot, THROWER, K]),
          % run the REAL driver; capture throw
          three_way(maxent_classifier:maxent_run(Ctx, S1), R1),
          run_info_count(N1),
          ( maxent_classifier:maxent_run_info(Ctx, RN, _)
            -> format('FORCED: maxent_run -> ~w ; run_info FOR Ctx PRESENT (N=~w)  <-- BAD~n', [R1, RN])
            ;  format('FORCED: maxent_run -> ~w ; run_info FOR Ctx ABSENT  <-- GOOD~n', [R1]) ),
          format('FORCED: total run_info_count=~w (expect 0 if throw precedes the assert)~n', [N1]),
          ( R1 = error(_) -> format('FORCED: error term = ~w~n', [R1]) ; true ),

          % also exercise the per-context multi_run path (:734) under the same injection
          maxent_classifier:maxent_cleanup,
          three_way(maxent_classifier:maxent_multi_run([Ctx], _S2), R2),
          ( maxent_classifier:maxent_run_info(Ctx, RN2, _)
            -> format('FORCED multi_run -> ~w ; run_info PRESENT (N=~w) <-- BAD~n', [R2, RN2])
            ;  format('FORCED multi_run -> ~w ; run_info ABSENT <-- GOOD~n', [R2]) )
        ),
        retractall(narrative_ontology:constraint_claim(THROWER, throw_test_claim))
    ),

    % ===== confirm cleanup restored claim-less state =====
    ( narrative_ontology:constraint_claim(THROWER, _)
      -> format('~nPOST: ~w STILL has a claim <-- cleanup FAILED~n', [THROWER])
      ;  format('~nPOST: ~w claim-less again -- cleanup OK~n', [THROWER]) ),
    halt.

:- initialization(main).
