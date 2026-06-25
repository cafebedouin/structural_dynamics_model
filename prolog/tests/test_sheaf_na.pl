% ============================================================================
% TEST: OQ-51 `unknown` is N/A on the canonical sheaf/H1 path
% ============================================================================
% Synthetic controls (corpus-free — a corpus `forall` can pass vacuously) for
% the N/A rule on grothendieck_cohomology + sheaf_analysis:
%   - obstruction_from_vector/3: real-pair counting, <2-real ⇒ null/null,
%     all-unknown ⇒ null/null (the Pattern-5 kill, NOT genuine_sheaf).
%   - count_disagreeing_pairs/2: an unknown-vs-real pair is NOT a disagreement.
%   - sheaf_status/2 + sheaf_undetermined_reason/2: both undetermined ROUTES
%     (insufficient_seats vs uncomputable_height) stay distinguishable, via a
%     cache-injected synthetic obstruction (no corpus needed).
% Plus a LIVE positive-control census: search the live corpus for each route and
% STATE dormancy out loud when a route fires on 0 live constraints this snapshot.
%
% Run:
%   cd prolog && swipl -g "[stack], [tests/test_sheaf_na], run_sheaf_na_tests, halt" -t "halt(1)"
% ============================================================================

:- use_module(grothendieck_cohomology).
:- use_module(sheaf_analysis).
:- use_module(arakelov_height).

:- dynamic sheaf_na_test_result/3.

run_sheaf_na_tests :-
    format("~n=== OQ-51 SHEAF N/A TESTS ===~n", []),
    retractall(sheaf_na_test_result(_, _, _)),
    forall(sheaf_na_test(Name, Goal),
           run_na_test(Name, Goal)),
    live_undetermined_census,            % informational — always runs, states dormancy
    aggregate_all(count, sheaf_na_test_result(_, pass, _), Passed),
    aggregate_all(count, sheaf_na_test_result(_, fail, _), Failed),
    Total is Passed + Failed,
    format("~n--- Results: ~w/~w passed ---~n", [Passed, Total]),
    (   Failed > 0
    ->  format("FAILED TESTS:~n"),
        forall(sheaf_na_test_result(N, fail, R), format("  [FAIL] ~w: ~w~n", [N, R])),
        fail
    ;   true
    ).

run_na_test(Name, Goal) :-
    (   catch(call(Goal), Err, (format(atom(EA), "~w", [Err]), fail))
    ->  assert(sheaf_na_test_result(Name, pass, "")),
        format("  [PASS] ~w~n", [Name])
    ;   (var(Err) -> Reason = "unexpected_result" ; Reason = EA),
        assert(sheaf_na_test_result(Name, fail, Reason)),
        format("  [FAIL] ~w: ~w~n", [Name, Reason])
    ).

/* ================================================================
   PURE SYNTHETIC CONTROLS — obstruction_from_vector / count pairs
   ================================================================ */

% [mountain, unknown, rope, rope] → real seats [mountain,rope,rope]; pairs
% (m,r),(m,r),(r,r) → H1 counts only the 2 real-real disagreements; H0=0.
sheaf_na_test(real_pairs_only, (
    grothendieck_cohomology:obstruction_from_vector([mountain, unknown, rope, rope], H0, H1),
    H0 == 0, H1 == 2 )).

% [unknown, unknown, unknown, snare] → NReal=1 → UNDETERMINED (null/null), not 0.
sheaf_na_test(insufficient_one_real, (
    grothendieck_cohomology:obstruction_from_vector([unknown, unknown, unknown, snare], H0, H1),
    H0 == null, H1 == null )).

% [unknown × 4] → NReal=0 → UNDETERMINED. The Pattern-5 kill: NOT genuine_sheaf.
sheaf_na_test(all_unknown_undetermined, (
    grothendieck_cohomology:obstruction_from_vector([unknown, unknown, unknown, unknown], H0, H1),
    H0 == null, H1 == null )).

% [snare × 4] → all real agree → H0=1, H1=0 (unchanged path).
sheaf_na_test(all_agree, (
    grothendieck_cohomology:obstruction_from_vector([snare, snare, snare, snare], H0, H1),
    H0 == 1, H1 == 0 )).

% [snare, unknown, unknown, snare] → 2 real agree, abstentions ignored → H0=1, H1=0.
sheaf_na_test(two_real_agree_with_abstention, (
    grothendieck_cohomology:obstruction_from_vector([snare, unknown, unknown, snare], H0, H1),
    H0 == 1, H1 == 0 )).

% count_disagreeing_pairs counts only real-real differences (=2 here).
sheaf_na_test(count_pairs_real_only, (
    grothendieck_cohomology:count_disagreeing_pairs([mountain, unknown, rope, rope], N),
    N == 2 )).

% an unknown-vs-real pair is N/A — contributes 0 disagreements.
sheaf_na_test(count_pairs_unknown_vs_real_zero, (
    grothendieck_cohomology:count_disagreeing_pairs([snare, unknown], N),
    N == 0 )).

/* ================================================================
   SYNTHETIC sheaf_status / reason — cache-injected (no corpus)
   ================================================================ */

% Route 1: <2 real seats (H1 = null) → undetermined / insufficient_seats.
sheaf_na_test(sheaf_route1_insufficient_seats, with_syn_cache(syn_undet1, null, null, (
    sheaf_analysis:sheaf_status(syn_undet1, S), S == undetermined,
    sheaf_analysis:sheaf_undetermined_reason(syn_undet1, R), R == insufficient_seats ))).

% Route 2: H1 = 0 but arakelov_height/2 fails on the fake id → undetermined /
% uncomputable_height (NOT genuine by absence, NOT insufficient_seats).
sheaf_na_test(sheaf_route2_uncomputable_height, with_syn_cache(syn_undet2, 1, 0, (
    sheaf_analysis:sheaf_status(syn_undet2, S), S == undetermined,
    sheaf_analysis:sheaf_undetermined_reason(syn_undet2, R), R == uncomputable_height ))).

% Manifest control: H1 > 0 → manifest_presheaf, and NO undetermined reason.
sheaf_na_test(sheaf_manifest_no_reason, with_syn_cache(syn_man, 0, 4, (
    sheaf_analysis:sheaf_status(syn_man, S), S == manifest_presheaf,
    \+ sheaf_analysis:sheaf_undetermined_reason(syn_man, _) ))).

%% with_syn_cache(+Id, +H0, +H1, :Goal)
%  Inject a synthetic obstruction into the grothendieck cache for Id, run Goal,
%  then retract — so sheaf_status reads our H0/H1 without needing a corpus, and
%  arakelov_height/2 legitimately FAILS on the fake (uncorpused) Id.
with_syn_cache(Id, H0, H1, Goal) :-
    setup_call_cleanup(
        assertz(grothendieck_cohomology:cached_obstruction(Id, H0, H1)),
        call(Goal),
        retract(grothendieck_cohomology:cached_obstruction(Id, H0, H1))
    ).

/* ================================================================
   LIVE POSITIVE-CONTROL CENSUS — search, state dormancy
   ================================================================ */

%% live_undetermined_census
%  Searches the live corpus for each undetermined route, with a POSITIVE CONTROL:
%  route 2 (uncomputable_height) is meaningful ONLY where arakelov_height computes,
%  and a bare [stack] load lacks the pipeline's MaxEnt, so arakelov fails
%  corpus-wide and EVERY h1=0 constraint spuriously reads route-2. Route 1
%  (insufficient_seats) is arakelov-independent and reliable in any context.
%  States dormancy / indeterminacy out loud. Informational; never fails the suite.
%  (Witnessed 2026-06-25: bare-context arakelov computes for 0/104; the AUTHORITATIVE
%  live route-2 count is in outputs/pipeline_output.json, where MaxEnt is loaded.)
live_undetermined_census :-
    format("~n--- Live undetermined census (this snapshot) ---~n", []),
    ( catch(corpus_loader:ensure_corpus_loaded, _, fail) -> true ; true ),
    ( catch(cache_registry:clear_all_caches, _, true) -> true ; true ),
    aggregate_all(count, corpus_loader:corpus_constraint(_), CTot),
    % Positive control: can arakelov_height compute for ANY constraint here?
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C0),
          catch(arakelov_height:arakelov_height(C0, _), _, fail) ), ArakN),
    % Route 1 — arakelov-independent (null H1), reliable in any context.
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C1),
          catch(sheaf_analysis:sheaf_undetermined_reason(C1, insufficient_seats), _, fail) ), N1),
    census_route_note(insufficient_seats, N1),
    (   ArakN =:= 0
    ->  format("  [INDETERMINATE] route uncomputable_height not witnessable here: arakelov_height computes for 0/~w constraints (no MaxEnt in a bare load). Route-2 liveness is pipeline-authoritative; see outputs/pipeline_output.json.~n", [CTot])
    ;   aggregate_all(count,
            ( corpus_loader:corpus_constraint(C2),
              catch(sheaf_analysis:sheaf_undetermined_reason(C2, uncomputable_height), _, fail) ), N2),
        format("  (arakelov computes for ~w/~w constraints in this context)~n", [ArakN, CTot]),
        census_route_note(uncomputable_height, N2)
    ).

census_route_note(Route, 0) :- !,
    format("  [DORMANT] route ~w fires on 0 live constraints this snapshot - witnessed synthetically only, live-dormant.~n", [Route]).
census_route_note(Route, N) :-
    format("  [LIVE] route ~w fires on ~w live constraint(s) - witnessed live.~n", [Route, N]).
