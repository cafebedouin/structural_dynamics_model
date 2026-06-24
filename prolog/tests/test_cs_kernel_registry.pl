% ============================================================================
% TEST: cs_kernel_registry.pl
% ============================================================================
% Run from prolog/ directory. NOTE: the abolition/retributive/deterrence triplet
% was removed from the live corpus in the 2026-06-05 reset; it survives in archive
% snapshots, so this test loads the triplet from one of them:
%   swipl -l stack.pl \
%         -l archives/datasets/kernel_test/abolition_reading.pl \
%         -l archives/datasets/kernel_test/retributive_reading.pl \
%         -l archives/datasets/kernel_test/deterrence_reading.pl \
%         -l tests/test_cs_kernel_registry.pl \
%         -g "run_cs_kernel_tests, halt." -t "halt(1)"
% (The divergence_fires_abolition_deterrence case is data-fragile across snapshots —
% the pair happens to agree everywhere in some draws; the structural compare_*
% invariants below are corpus-independent.)
% ============================================================================

:- use_module(cs_kernel_registry).
:- use_module(drl_composition).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).

:- dynamic cs_kernel_test_result/3.

run_cs_kernel_tests :-
    format("~n=== CS KERNEL REGISTRY TESTS ===~n", []),
    forall(cs_kernel_test(Name, Goal, Expected),
           run_kernel_test(Name, Goal, Expected)),
    aggregate_all(count, cs_kernel_test_result(_, pass, _), Passed),
    aggregate_all(count, cs_kernel_test_result(_, fail, _), Failed),
    Total is Passed + Failed,
    format("~n--- Results: ~w/~w passed ---~n", [Passed, Total]),
    (   Failed > 0
    ->  format("FAILED TESTS:~n"),
        forall(cs_kernel_test_result(N, fail, R),
               format("  [FAIL] ~w: ~w~n", [N, R]))
    ;   true
    ).

run_kernel_test(Name, Goal, Expected) :-
    (   catch(call(Goal), Err, (format(atom(ErrAtom), "~w", [Err]), fail))
    ->  Got = success
    ;   Got = fail
    ),
    (   Got == Expected
    ->  assert(cs_kernel_test_result(Name, pass, "")),
        format("  [PASS] ~w~n", [Name])
    ;   (var(Err) -> Reason = "unexpected_result" ; Reason = ErrAtom),
        assert(cs_kernel_test_result(Name, fail, Reason)),
        format("  [FAIL] ~w (expected ~w, got ~w)~n", [Name, Expected, Got])
    ).

/* ================================================================
   COVERAGE TESTS
   ================================================================ */

cs_kernel_test("triplet_coverage_is_3",
    (cs_readings_for_kernel(state_execution_authority, Rs), length(Rs, 3)),
    success).

cs_kernel_test("triplet_coverage_predicate",
    cs_kernel_coverage(state_execution_authority, 3),
    success).

cs_kernel_test("readings_contains_abolition",
    (cs_readings_for_kernel(state_execution_authority, Rs),
     member(_-abolition_reading, Rs)),
    success).

cs_kernel_test("readings_contains_retributive",
    (cs_readings_for_kernel(state_execution_authority, Rs),
     member(_-retributive_reading, Rs)),
    success).

cs_kernel_test("readings_contains_deterrence",
    (cs_readings_for_kernel(state_execution_authority, Rs),
     member(_-deterrence_reading, Rs)),
    success).

cs_kernel_test("unknown_kernel_empty",
    (cs_readings_for_kernel(nonexistent_kernel_xyz, Rs), Rs = []),
    success).

/* ================================================================
   DIVERGENCE FIRE TESTS
   Observed empirically: all three reading-pairs diverge somewhere
   across the 156-context product site (253 total diverging pairs).
   abolition type set: {naturalized, rope, unknown}
   retributive type set: {rope, tangled_rope, unknown}
   deterrence type set: {rope, tangled_rope, unknown}
   ================================================================ */

cs_kernel_test("divergence_fires_abolition_deterrence",
    cs_kernel_divergence(state_execution_authority, _, _-abolition_reading, _-deterrence_reading),
    success).

cs_kernel_test("divergence_fires_abolition_retributive",
    cs_kernel_divergence(state_execution_authority, _, _-abolition_reading, _-retributive_reading),
    success).

cs_kernel_test("divergence_fires_deterrence_retributive",
    cs_kernel_divergence(state_execution_authority, _, _-deterrence_reading, _-retributive_reading),
    success).

/* ================================================================
   SILENCE CASE + ERROR-MASKING GUARD

   Context chosen from observed divergence map (not predicted):
   deterrence and retributive both produce tangled_rope at
   context(powerless, biographical, trapped, national).

   Non-vacuity confirmed: cs_kernel_divergence DOES fire for this
   pair at context(...spatial_scope(local)), so the silence test
   below would fail if the pair diverged at the national context.
   ================================================================ */

% Test 1: the real silence case — diagnostic does not fire at the agreement context.
cs_kernel_test("divergence_silent_at_observed_agreement_context",
    \+ cs_kernel_divergence(
        state_execution_authority,
        context(agent_power(powerless),
                time_horizon(biographical),
                exit_options(trapped),
                spatial_scope(national)),
        _-deterrence_reading,
        _-retributive_reading),
    success).

% Test 2: error-masking guard — both calls succeed and agree at that same context,
% confirming the silence above is genuine and not caused by an upstream failure.
cs_kernel_test("agreement_calls_succeed_at_observed_context",
    (   AgreementCtx = context(agent_power(powerless),
                               time_horizon(biographical),
                               exit_options(trapped),
                               spatial_scope(national)),
        drl_composition:classify_at_time(deterrence_reading,  0, AgreementCtx, T1),
        drl_composition:classify_at_time(retributive_reading, 0, AgreementCtx, T2),
        T1 == T2
    ),
    success).

/* ================================================================
   READING-ROBUSTNESS TESTS (OQ-10) — compare_kernel_readings/3.
   Corpus-INDEPENDENT structural invariants (no fixed counts, so they
   do not go data-fragile across snapshots like the divergence-fire tests).
   ================================================================ */

% Profile covers every context of the product site (one verdict per context).
cs_kernel_test("compare_profile_covers_site_contexts",
    (   compare_kernel_readings(state_execution_authority, Profile, _),
        constraint_indexing:site_contexts_product(All),
        length(Profile, N), length(All, N)
    ),
    success).

% Every profile entry is a well-formed agree(_) or diverge(_) verdict (no third token).
cs_kernel_test("compare_profile_verdicts_wellformed",
    (   compare_kernel_readings(state_execution_authority, Profile, _),
        forall(member(_-V, Profile), (V = agree(_) ; V = diverge(_)))
    ),
    success).

% THE JOIN-CONSISTENCY INVARIANT: compare_kernel_readings/3 generalizes
% cs_kernel_divergence/4 without losing or inventing a divergence — the sum of
% per-pair DivergeN must equal the number of cs_kernel_divergence/4 solutions.
% This is the positive control that the agree/diverge split is faithful to the
% divergence engine (build_discipline: every probe needs a positive control).
cs_kernel_test("compare_join_consistency_with_divergence_engine",
    (   compare_kernel_readings(state_execution_authority, _, PairStats),
        aggregate_all(sum(D), member(_-stats(_,_,D), PairStats), SumDiv),
        aggregate_all(count,
            cs_kernel_divergence(state_execution_authority, _, _, _), NEngine),
        SumDiv =:= NEngine
    ),
    success).

/* ================================================================
   STRUCTURAL NOTE (not a test — for future audit reference):
   abolition_reading produces type 'naturalized' at some contexts.
   This is a real structural finding: the analytical-observer mountain
   reading of abolition triggers a false-summit classification at
   the engine level. It is NOT a test failure.

   To inspect:
     findall(Ctx-Type,
       (site_contexts_product(Cs), member(Ctx,Cs),
        classify_at_time(abolition_reading, 0, Ctx, Type),
        Type = naturalized), Pairs),
     length(Pairs, N), format('~w naturalized contexts~n',[N]).
   ================================================================ */
