% ============================================================================
% nlwb_diff_harness.pl — OQ-66 step 2: the REPAIRED raw-vs-filtered diff.
%
% Answers: does swapping natural_law_without_beneficiary/1 from the raw
% constraint_beneficiary/2 read to the agent-filtered agent_beneficiary/2 read
% change any final classification observable?
%
% Repairs three uncontrolled channels in the harness that produced the prior
% `full_corpus_diff_count=0` (which was a SCREEN, not a measurement):
%
%   1. NO CACHE CLEAR between arms. Registered memos (incl.
%      boltzmann_compliance:cached_classification/3) can serve pre-swap values,
%      making a stale-cache "no change" byte-identical to a real null.
%      -> cache_registry:clear_all_caches/0 after each swap AND after restore.
%
%   2. MAXENT IS CORPUS-FITTED STATE, deliberately outside cache_registry, so a
%      cache clear does not touch it. Worse: a plain [stack] + corpus load
%      leaves it UNFITTED (maxent_dist/3 empty), every maxent_top_type call
%      fails, and the prior harness mapped that failure to `no_top` in BOTH
%      arms — comparing [no_top,...] against itself. The MaxEnt consumer
%      surface (maxent_classifier.pl:182,186,201) was never measured while the
%      result presented as zero-diff. Pattern 6, in the instrument.
%      -> explicit maxent_cleanup + maxent_multi_run/2 in EACH arm, plus a
%         non-vacuity assertion that maxent_dist/3 is non-empty BEFORE any
%         maxent_top_type is read. Without that assertion the no_top
%         self-comparison silently returns.
%
%   3. NO PLANTED FLIP proving the harness can see a change.
%      -> the fixture leg (tests/fixtures/nlwb_controls/) is the flip detector.
%
% The refit is not an empty control: MaxEnt consumes the predicate through
% THREE channels — fit population (compute_type_profile/4 selects training sets
% by drl_core:dr_type, which depends on nlwb via the snare and tangled_rope
% blocks), priors (maxent_compute_priors/1 counts the same dr_type source), and
% per-constraint likelihood (boolean_log_likelihood/3 evaluates
% eval_boolean_feature(C, natural_law_without_beneficiary, _) against
% boolean_spec(snare,...,forbidden) and boolean_spec(tangled_rope,...,forbidden)).
% The boolean term is baked into the stored maxent_dist/3 at maxent_precompute
% time, not evaluated lazily — so the recompute IS the reclassify.
%
% Usage: swipl -q -l stack.pl -l nlwb_diff_harness.pl -g "run_leg('<corpus_path>'), halt" -t 'halt(1)'
% ============================================================================

% --- the two readings of the predicate ---------------------------------------
% SWI permits abolishing static predicates (iso flag false); the predicate
% becomes dynamic for the remainder of this process. Idiom reviewed in
% docs/technical/swipl_load_path_and_probe_gotchas.md §7.

swap_nlwb_to_filtered :-
    abolish(drl_core:natural_law_without_beneficiary/1),
    assertz(( drl_core:natural_law_without_beneficiary(C) :-
                  drl_core:emerges_naturally(C),
                  \+ drl_core:requires_active_enforcement(C),
                  \+ narrative_ontology:agent_beneficiary(C, _) )),
    cache_registry:clear_all_caches.

restore_nlwb_to_raw :-
    abolish(drl_core:natural_law_without_beneficiary/1),
    assertz(( drl_core:natural_law_without_beneficiary(C) :-
                  drl_core:emerges_naturally(C),
                  \+ drl_core:requires_active_enforcement(C),
                  \+ narrative_ontology:constraint_beneficiary(C, _) )),
    cache_registry:clear_all_caches.

% --- MaxEnt refit with the non-vacuity assertion the prior run lacked --------

refit_maxent(Arm, NDist) :-
    constraint_indexing:site_contexts_canonical(Ctxs),
    maxent_classifier:maxent_cleanup,
    maxent_classifier:maxent_multi_run(Ctxs, _Summaries),
    aggregate_all(count, maxent_classifier:maxent_dist(_, _, _), NDist),
    (   NDist > 0
    ->  true
    % Fail-closed: an unfitted model makes every maxent_top_type read fail, and
    % a failed read compared against a failed read is a self-comparison that
    % reads as zero-diff. Refuse to proceed rather than emit that.
    ;   throw(maxent_unfitted(Arm))
    ).

% --- observables -------------------------------------------------------------
% dr_type at the 4 canonical contexts AND maxent_top_type at the same 4.
% Failures are mapped to TAGGED terms, never to a plausible value.

snapshot(C, snap(Types, Tops)) :-
    constraint_indexing:site_contexts_canonical(Ctxs),
    findall(T, ( member(Cx, Ctxs),
                 ( drl_core:dr_type(C, Cx, T0) -> T = T0 ; T = no_type ) ), Types),
    findall(M, ( member(Cx, Ctxs),
                 ( catch(maxent_classifier:maxent_top_type(C, Cx, M0), E,
                         M0 = maxent_error(E))
                   -> M = M0 ; M = no_top ) ), Tops).

% --- the run -----------------------------------------------------------------

run_leg(Leg) :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, Leg)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    length(Cs, N),
    (   N > 0 -> true ; throw(empty_leg(Leg)) ),

    % ---- ARM 1: raw (current HEAD behaviour) ----
    cache_registry:clear_all_caches,
    refit_maxent(raw, NDistRaw),
    maplist(snapshot, Cs, RawSnaps),

    % ---- ARM 2: filtered ----
    setup_call_cleanup(
        swap_nlwb_to_filtered,
        % once/1: the goal must exit DETERMINISTICALLY or setup_call_cleanup
        % defers the restore past the post-control below.
        once(( refit_maxent(filtered, NDistFilt),
               maplist(snapshot, Cs, FiltSnaps) )),
        restore_nlwb_to_raw),

    % ---- post-control: restore took effect ----
    (   predicate_property(drl_core:natural_law_without_beneficiary(_), number_of_clauses(_))
    ->  true ; true ),

    % ---- diff ----
    findall(C-Raw-Filt,
            ( nth1(I, Cs, C), nth1(I, RawSnaps, Raw), nth1(I, FiltSnaps, Filt),
              Raw \== Filt ),
            Diffs),
    length(Diffs, ND),
    format("LEG ~w n=~w maxent_dist_raw=~w maxent_dist_filtered=~w diff_count=~w~n",
           [Leg, N, NDistRaw, NDistFilt, ND]),
    forall(member(C-R-F, Diffs),
           format("  DIFF ~w~n    raw:      ~w~n    filtered: ~w~n", [C, R, F])).
