% OQ-112 Round-2 — witness-truth control for the NEW indexed completion fact
% maxent_indexed_run_info/3 (added this commit to maxent_indexed_run, after
% maxent_classify_all_indexed). The indexed assert is the highest-risk write in Round 2:
% it WRITES a completion witness rather than reading one, so it is the one place the
% premature-assert Pattern 6 can be introduced BY this fix. The two-arm control decides
% whether the line is correct (not verification-after):
%
%   CLEAN arm  -> maxent_indexed_run_info PRESENT with the correct Context, count=1
%                 (catches the "correctly-positioned-but-wrongly-guarded / wrong-Context"
%                  failure, which is INVISIBLE to the throw arm alone)
%   THROW arm  -> mid-loop throw at K of N -> maxent_indexed_run_info ABSENT, count=0
%   PHASE      -> compute_profiles_indexed succeeds, classify_all_indexed([thrower]) throws
%                 -> the throw is IN the per-constraint loop, assert is after it
%   WHY-DISTINCT -> in the THROW arm the classical maxent_run_info IS present (indexed needs
%                 a prior maxent_run for priors); a SHARED fact would read clean here. The
%                 distinct fact is what makes indexed-void detectable.
%
% Read-only w.r.t. engine source beyond this commit's edit; only mutation is a test-local
% constraint_claim/2, asserted-then-retracted (POST witness).

:- [stack].
:- corpus_loader:load_all_testsets.

three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).

idx_count(N)  :- aggregate_all(count, maxent_classifier:maxent_indexed_run_info(_,_,_), N).
cls_count(N)  :- aggregate_all(count, maxent_classifier:maxent_run_info(_,_,_), N).

main :-
    constraint_indexing:default_context(Ctx),
    THROWER = polaris_document_status_contradictions,   % sorts mid-list

    % ===================== CLEAN ARM =====================
    maxent_classifier:maxent_cleanup,
    three_way(maxent_classifier:maxent_run(Ctx, _S0), R0c),         % priors prerequisite
    three_way(maxent_classifier:maxent_indexed_run(Ctx, _Si), R0i),
    idx_count(NIdx0),
    ( maxent_classifier:maxent_indexed_run_info(ICtx, IN, _) -> true ; ICtx = absent, IN = absent ),
    format('CLEAN: maxent_run -> ~w ; maxent_indexed_run -> ~w~n', [R0c, R0i]),
    format('CLEAN: indexed_run_info PRESENT? ctx==input=~w  N=~w  count=~w  (expect yes / 86 / 1)~n',
           [ (ICtx == Ctx -> yes ; ICtx), IN, NIdx0 ]),

    % ===================== THROW ARM =====================
    maxent_classifier:maxent_cleanup,
    three_way(maxent_classifier:maxent_run(Ctx, _S1), R1c),         % priors; asserts classical run_info
    setup_call_cleanup(
        assertz(narrative_ontology:constraint_claim(THROWER, throw_test_claim)),
        ( findall(C, (narrative_ontology:constraint_claim(C,_), \+ is_list(C), atom(C)), D0),
          sort(D0, Disc), length(Disc, Ntot), ( nth1(K, Disc, THROWER) -> true ; K = -1 ),
          format('~nTHROW: priors run -> ~w ; discovery N=~w ; THROWER at K=~w~n', [R1c, Ntot, K]),
          three_way(maxent_classifier:maxent_indexed_run(Ctx, _S1i), R1i),
          idx_count(NIdx1), cls_count(NCls1),
          ( maxent_classifier:maxent_indexed_run_info(Ctx, _, _)
            -> format('THROW: indexed_run -> ~w ; indexed_run_info PRESENT  <-- BAD~n', [R1i])
            ;  format('THROW: indexed_run -> ~w ; indexed_run_info ABSENT, count=~w  <-- GOOD~n', [R1i, NIdx1]) ),
          format('THROW why-distinct: classical maxent_run_info count=~w (PRESENT) while indexed ABSENT~n',
                 [NCls1]),
          format('THROW why-distinct: a SHARED fact would read PRESENT here -> indexed void undetectable~n'),

          % ===================== PHASE RESOLUTION =====================
          maxent_classifier:maxent_cleanup,
          three_way(maxent_classifier:maxent_run(Ctx, _S2), _R2c),
          three_way(maxent_classifier:maxent_compute_profiles_indexed(Disc, Ctx), RPp),
          format('~nPHASE A compute_profiles_indexed(incl thrower) -> ~w~n', [RPp]),
          % clean profiles, then classify just the thrower
          findall(C2, (narrative_ontology:constraint_claim(C2,_), \+ is_list(C2), atom(C2), C2 \== THROWER), Clean0),
          sort(Clean0, Clean),
          maxent_classifier:maxent_compute_profiles_indexed(Clean, Ctx),
          three_way(maxent_classifier:maxent_classify_all_indexed([THROWER], Ctx), RPc),
          format('PHASE B classify_all_indexed([thrower], profiles present) -> ~w  (throw IS in the loop)~n', [RPc])
        ),
        retractall(narrative_ontology:constraint_claim(THROWER, throw_test_claim))
    ),
    ( narrative_ontology:constraint_claim(THROWER, _)
      -> format('~nPOST: ~w STILL has a claim <-- cleanup FAILED~n', [THROWER])
      ;  format('~nPOST: ~w claim-less again -- cleanup OK~n', [THROWER]) ),
    halt.

:- initialization(main).
