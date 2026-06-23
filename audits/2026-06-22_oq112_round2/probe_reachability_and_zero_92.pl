% OQ-112 Round-0 — Witness 2 (item-4 reachability on 92, profile-PRESENT) + Witness 3
% (zero-with-witness count). Read-only. Drives the real maxent path.
%
% Distinguishes the LATENT hazard from a LIVE throw:
%   - the 6 unknown-suppression constraints on 92 are claim-less -> excluded from
%     maxent_run's constraint discovery -> the driver never reaches the sink (no live throw)
%   - the sink itself DOES throw when a constraint both (a) reaches it and (b) carries
%     the `unknown` sentinel -- witnessed in isolation AND profile-present (not the
%     LL=-10.0 prior-fallback non-witness from the original 62-row trace).

:- [stack].
:- corpus_loader:load_all_testsets.

three_way(Goal, Result) :-
    (   catch(Goal, Err, (Result = error(Err)))
    ->  (var(Result) -> Result = success ; true)
    ;   Result = quiet_failure
    ).

main :-
    config:param(suppression_metric_name, Supp),
    format('suppression_metric_name = ~w~n~n', [Supp]),

    % --- (a) the absent-suppression set on 92 ---
    findall(C, (corpus_loader:corpus_constraint(C),
                \+ narrative_ontology:constraint_metric(C, Supp, _)), Absent),
    length(Absent, NA),
    format('W2a ABSENT_SUPP (~w of 92): ~w~n', [NA, Absent]),

    % --- (b) which absent-supp are claim-less (excluded from maxent discovery)? ---
    findall(C, (member(C, Absent), \+ narrative_ontology:constraint_claim(C, _)), Claimless),
    findall(C, (member(C, Absent),    narrative_ontology:constraint_claim(C, _)), ClaimBearing),
    length(Claimless, NCL), length(ClaimBearing, NCB),
    format('W2b   claim-LESS (excluded from maxent_run discovery): ~w of ~w  ~w~n', [NCL, NA, Claimless]),
    format('W2b   claim-BEARING (WOULD enter maxent -> hit sink):   ~w of ~w  ~w~n', [NCB, NA, ClaimBearing]),
    findall(C, (narrative_ontology:constraint_claim(C, _), \+ is_list(C), atom(C)), Disc0),
    sort(Disc0, Disc), length(Disc, ND),
    format('W2b   MAXENT_DISCOVERY_N = ~w  (corpus = 92; absorbed-by-exclusion = ~w)~n~n', [ND, NA]),

    % --- (c) ISOLATED sink throw (corpus-independent positive control) ---
    three_way(maxent_classifier:gaussian_log_likelihood(unknown, 0.5, 0.2, _LLi), RIso),
    format('W2c ISOLATED sink: gaussian_log_likelihood(unknown,0.5,0.2,_) -> ~w~n~n', [RIso]),

    % --- (d) PROFILE-PRESENT sink witness (run precompute first, THEN hit the sink) ---
    constraint_indexing:default_context(Ctx),
    three_way(maxent_classifier:maxent_run(Ctx, Sum), RRun),
    format('W2d maxent_run(~w) -> ~w  (summary=~w)~n', [Ctx, RRun, Sum]),
    (   maxent_classifier:maxent_profile(snare, suppression, Ctx, P)
    ->  format('W2d PROFILE PRESENT (snare,suppression,~w) = ~w~n', [Ctx, P])
    ;   format('W2d PROFILE ABSENT for context ~w  (sink result below is a NON-witness)~n', [Ctx])
    ),
    (   Absent = [A|_]
    ->  three_way(maxent_classifier:maxent_type_log_likelihood(A, snare, Ctx, LLpp, _), Rpp),
        format('W2d PROFILE-PRESENT sink on ~w (snare) -> ~w (LL=~w)~n', [A, Rpp, LLpp]),
        % positive control: a present-suppression constraint on the same goal/context = numeric
        (   corpus_loader:corpus_constraint(Pr),
            narrative_ontology:constraint_metric(Pr, Supp, _),
            narrative_ontology:constraint_claim(Pr, _), !
        ->  three_way(maxent_classifier:maxent_type_log_likelihood(Pr, snare, Ctx, LLc, _), Rc),
            format('W2d control (present-supp) ~w -> ~w (LL=~w)~n~n', [Pr, Rc, LLc])
        ;   format('W2d control: none found~n~n', [])
        )
    ;   format('W2d no absent-supp constraint~n~n', [])
    ),

    % --- Witness 3: zero-with-witness count across the run_info-asserting stages ---
    % stage 1: maxent_run (already run above, asserts via precompute :555)
    % stage 2: maxent_multi_run over the standard contexts (asserts :734 per context)
    ( catch(dirac_classification:standard_contexts(Ctxs), _, fail) -> true
    ; Ctxs = [Ctx] ),
    format('W3 maxent_multi_run contexts = ~w~n', [Ctxs]),
    three_way(maxent_classifier:maxent_multi_run(Ctxs, _Summaries), RMulti),
    format('W3 maxent_multi_run -> ~w~n', [RMulti]),
    findall(ri(RC, RN), maxent_classifier:maxent_run_info(RC, RN, _), RIs),
    format('W3 maxent_run_info facts present: ~w~n', [RIs]),
    findall(RC, maxent_classifier:maxent_run_info(RC, 0, _), Zeros),
    length(Zeros, NZ),
    format('W3 ZERO_WITH_WITNESS_COUNT = ~w   zeros=~w~n', [NZ, Zeros]),
    halt.

:- initialization(main).
