% ============================================================================
% TEST: maxent_profile/4 context indexing
% ============================================================================
% Two tests encoding the spec claim that MaxEnt profiles are per-context
% independent (D-1 discipline: spec-encoding unit tests for load-bearing
% measurement primitives).
%
% Test 1 — Spec-encoding unit test (D-1 instance):
%   test_maxent_profiles_are_per_context_independent/0
%   Asserts: profiles for context_A and context_B are stored and queryable
%   independently; the findall that drives continuous_log_likelihood produces
%   exactly 3 log-likelihoods per type per context, not 6 or more.
%
% Test 2 — Regression test (Audit 3 sentinel):
%   test_audit3_sentinel_post_fix_reproduces_clean_session/0
%   Asserts: after the fix, maxent_multi_run on the four canonical contexts
%   produces the same analytical-context result as a clean single-context
%   maxent_run. Pre-fix values: H=0.000229, TopType=scaffold.
%   Post-fix target: H≈0.4456 (tolerance 0.01), TopType=tangled_rope.
%
% Run from prolog/ directory:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l tests/test_maxent_profile_indexing.pl \
%         -g "run_all_maxent_profile_tests, halt." -t "halt(1)"
% ============================================================================

:- use_module(covering_analysis).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(corpus_loader).
:- use_module(maxent_classifier).
:- use_module(library(lists)).

/* ================================================================
   ENTRY POINT
   ================================================================ */

run_all_maxent_profile_tests :-
    format('~n=== MaxEnt Profile Indexing Tests ===~n~n'),
    run_test('test_maxent_profiles_are_per_context_independent',
             test_maxent_profiles_are_per_context_independent),
    run_test('test_audit3_sentinel_post_fix_reproduces_clean_session',
             test_audit3_sentinel_post_fix_reproduces_clean_session),
    format('~n=== Done ===~n').

run_test(Name, Goal) :-
    format('--- ~w ---~n', [Name]),
    (   catch(Goal, E, (format('[FAIL] Exception: ~w~n', [E]), fail))
    ->  format('[PASS] ~w~n~n', [Name])
    ;   format('[FAIL] ~w~n~n', [Name])
    ).

/* ================================================================
   TEST 1 — Per-context profile independence (D-1 spec-encoding)
   ================================================================ */

%% Inline minimal constraint set for Test 1.
%  3 constraints with known metric values; no corpus load required.
%  We use real constraint IDs from the corpus so constraint_claim/2 and
%  constraint_metric/3 facts exist after corpus_loader:load_all_testsets.

test_maxent_profiles_are_per_context_independent :-
    corpus_loader:load_all_testsets,

    % Contexts: powerless vs analytical (different power modifiers → different χ)
    drl_core:standard_context(CtxA),  % picks first (powerless)
    constraint_indexing:default_context(CtxB), % analytical

    % Confirm the two contexts are actually different
    (CtxA \= CtxB -> true
    ;   format('[FAIL] standard_context and default_context returned the same context~n'),
        fail
    ),

    % Use a small constraint set (3 constraints) drawn from the loaded corpus
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        atom(C)
    ), RawAll),
    sort(RawAll, AllCs),
    (   AllCs = [C1, C2, C3 | _]
    ->  SmallSet = [C1, C2, C3]
    ;   format('[FAIL] Fewer than 3 constraints loaded~n'), fail
    ),

    % Clean slate
    maxent_classifier:maxent_cleanup,

    % Phase A: compute profiles for CtxA
    maxent_classifier:maxent_compute_profiles(SmallSet, CtxA),
    findall(Type-Metric-PA,
        maxent_classifier:maxent_profile(Type, Metric, CtxA, PA),
        ProfilesA),
    length(ProfilesA, NA),
    format('  Context A profiles: ~w~n', [NA]),

    % Phase B: compute profiles for CtxB (without cleanup — this is the bug condition)
    maxent_classifier:maxent_compute_profiles(SmallSet, CtxB),
    findall(Type-Metric-PB,
        maxent_classifier:maxent_profile(Type, Metric, CtxB, PB),
        ProfilesB),
    length(ProfilesB, NB),
    format('  Context B profiles: ~w~n', [NB]),

    % Assertion 1: Both A and B profiles are queryable separately.
    (   NA > 0, NB > 0
    ->  format('  [OK] Both contexts have profiles~n')
    ;   format('  [FAIL] One or both contexts have zero profiles~n'), fail
    ),

    % Assertion 2: The findall that drives continuous_log_likelihood
    %   for CtxA produces exactly 3 log-likelihoods (one per metric) per type.
    %   With the bug, it produces 6 (or more) because CtxB profiles also match.
    maxent_classifier:maxent_type(SomeType),
    findall(LL, (
        member(MN, [extractiveness, suppression, theater]),
        maxent_classifier:maxent_profile(SomeType, MN, CtxA, params(Mu, Sigma)),
        Sigma > 0,
        LL is -0.5 * log(2 * Sigma * Sigma)  % dummy computation, value irrelevant
    ), LLsA),
    length(LLsA, NLLsA),
    format('  LL count for type ~w, context A: ~w (expected 3)~n', [SomeType, NLLsA]),
    (   NLLsA =:= 3
    ->  format('  [OK] Exactly 3 log-likelihoods for context A (no accumulation)~n')
    ;   NLLsA > 3
    ->  format('  [FAIL] Profile accumulation across contexts detected — maxent_profile/4 is being read without context filter, or written without context indexing. Got ~w instead of 3.~n', [NLLsA]),
        fail
    ;   format('  [FAIL] Fewer than 3 log-likelihoods for context A (profiles missing)~n'),
        fail
    ),

    % Assertion 3: maxent_cleanup removes all profile facts regardless of context.
    maxent_classifier:maxent_cleanup,
    findall(_, maxent_classifier:maxent_profile(_, _, _, _), Remaining),
    length(Remaining, NRemaining),
    (   NRemaining =:= 0
    ->  format('  [OK] maxent_cleanup removed all profile facts~n')
    ;   format('  [FAIL] maxent_cleanup left ~w profile facts~n', [NRemaining]),
        fail
    ).

/* ================================================================
   TEST 2 — Audit 3 sentinel regression
   ================================================================ */

%% test_audit3_sentinel_post_fix_reproduces_clean_session/0
%  Post-fix: maxent_multi_run on the four canonical contexts must produce
%  the same analytical-context result as a single-context clean session.
%  Sentinel: collective_action_as_leverage_conversion
%  Pre-fix accumulated: H=0.000229, TopType=scaffold  (the bug)
%  Post-fix target:     H≈0.4456 (±0.01), TopType=tangled_rope
test_audit3_sentinel_post_fix_reproduces_clean_session :-
    corpus_loader:load_all_testsets,

    Sentinel = collective_action_as_leverage_conversion,

    % Verify sentinel is present
    (   narrative_ontology:constraint_claim(Sentinel, _)
    ->  format('  Sentinel ~w found in corpus.~n', [Sentinel])
    ;   format('  [FAIL] Sentinel constraint ~w not found — cannot run test~n', [Sentinel]),
        fail
    ),

    constraint_indexing:default_context(AnalyticalCtx),

    % Run accumulated session (the bug trigger)
    findall(Ctx, drl_core:standard_context(Ctx), FourContexts),
    length(FourContexts, NCtx),
    format('  Running maxent_multi_run on ~w contexts...~n', [NCtx]),
    maxent_classifier:maxent_multi_run(FourContexts, _Summaries),

    % Query post-fix accumulated values for the sentinel
    (   maxent_classifier:maxent_entropy(Sentinel, AnalyticalCtx, HNorm)
    ->  true
    ;   format('  [FAIL] maxent_entropy/3 failed for sentinel~n'), fail
    ),
    (   maxent_classifier:maxent_top_type(Sentinel, AnalyticalCtx, TopType)
    ->  true
    ;   format('  [FAIL] maxent_top_type/3 failed for sentinel~n'), fail
    ),

    format('  Post-fix accumulated H=~6f, TopType=~w~n', [HNorm, TopType]),
    format('  Expected:            H≈0.4456 (±0.01), TopType=tangled_rope~n'),

    % Check H within tolerance
    Tolerance = 0.01,
    Target = 0.4456,
    HDiff is abs(HNorm - Target),
    (   HDiff =< Tolerance
    ->  format('  [OK] H within tolerance (diff=~6f)~n', [HDiff])
    ;   format('  [FAIL] Post-fix accumulated session does not reproduce pre-fix clean session for sentinel constraint; profile accumulation may still be occurring. H=~6f (expected ~6f ± ~6f)~n',
               [HNorm, Target, Tolerance]),
        fail
    ),

    % Check top type
    (   TopType = tangled_rope
    ->  format('  [OK] TopType=tangled_rope~n')
    ;   format('  [FAIL] Post-fix accumulated session does not reproduce pre-fix clean session for sentinel constraint; profile accumulation may still be occurring. TopType=~w (expected tangled_rope)~n', [TopType]),
        fail
    ).
