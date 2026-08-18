% ============================================================================
% AUDIT 3 — PROFILE ACCUMULATION COMPARISON
% ============================================================================
% Runs two MaxEnt sessions for every constraint and records the analytical-
% context result under each:
%
%   Clean session     — maxent_run(analytical, _)
%                       Single context; matches abductive_report's state.
%
%   Accumulated session — maxent_multi_run([powerless,moderate,institutional,
%                         analytical], _)
%                         Four contexts, no cleanup between them; matches
%                         json_report's state.
%
% IMPORTANT: MaxEnt profile computation must run on the full corpus, not
% just the sample. Profiles are empirical Gaussians fitted from all
% constraints; partial-corpus profiles differ from what the real pipeline
% produces and would invalidate the comparison. The sample only filters
% which IDs appear in the JSON output.
%
% Run from prolog/ directory:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l audit3_maxent_compare.pl \
%         -g "run_audit3, halt." -t "halt(1)" \
%         2>../outputs/audit3_prolog_log.txt
%
% Output is written directly to ../outputs/audit3_maxent_raw.json by the script
% (not via stdout redirect) to avoid mixing with stack.pl's initialization banner.
% ============================================================================

:- use_module(covering_analysis).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(corpus_loader).
:- use_module(maxent_classifier).
:- use_module(library(lists)).

:- dynamic audit3_clean_dist/2.  % audit3_clean_dist(ConstraintID, Dist)


/* ================================================================
   ENTRY POINT
   ================================================================ */

run_audit3 :-
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(AnalyticalCtx),

    %% Phase 1 — Clean session
    format(user_error, '[audit3] Phase 1: clean session (maxent_run)...~n', []),
    maxent_classifier:maxent_run(AnalyticalCtx, _CleanSummary),

    %% Save clean distributions before maxent_multi_run calls maxent_cleanup
    findall(C-Dist,
        maxent_classifier:maxent_distribution(C, AnalyticalCtx, Dist),
        CleanList),
    length(CleanList, NClean),
    format(user_error, '[audit3] Saved ~w clean distributions.~n', [NClean]),
    forall(member(C-Dist, CleanList), assertz(audit3_clean_dist(C, Dist))),

    %% Phase 2 — Accumulated session
    %% maxent_multi_run calls maxent_cleanup internally, then loops through
    %% all four contexts without cleanup between them.
    format(user_error, '[audit3] Phase 2: accumulated session (maxent_multi_run)...~n', []),
    constraint_indexing:site_contexts(WCtxs),
    maxent_classifier:maxent_multi_run(WCtxs, _AccumSummaries),
    format(user_error, '[audit3] Accumulated session done.~n', []),

    %% Discover all corpus constraints (same pattern as maxent_multi_run)
    findall(C, (
        narrative_ontology:constraint_claim(C, _),
        \+ is_list(C),
        atom(C)
    ), RawC),
    sort(RawC, AllConstraints),
    length(AllConstraints, NTotal),
    format(user_error, '[audit3] Writing output for ~w constraints...~n', [NTotal]),

    %% Write JSON directly to output file (avoids mixing with stack.pl stdout banner)
    OutPath = '../outputs/audit3_maxent_raw.json',
    setup_call_cleanup(
        open(OutPath, write, S),
        (   format(S, '{"constraints":[~n', []),
            write_rows_to(S, AllConstraints, AnalyticalCtx, true),
            format(S, '~n]}~n', [])
        ),
        close(S)
    ),
    format(user_error, '[audit3] Done. Written to ~w~n', [OutPath]).


/* ================================================================
   ROW OUTPUT
   ================================================================ */

write_rows_to(_, [], _, _).
write_rows_to(S, [C|Rest], Ctx, First) :-
    (   First = true -> Sep = '' ; Sep = ','   ),
    (   build_row(C, Ctx, Row)
    ->  format(S, '~w~n~w', [Sep, Row])
    ;   format(user_error, '[audit3] WARNING: skipped ~w (build_row failed)~n', [C])
    ),
    write_rows_to(S, Rest, Ctx, false).

build_row(C, Ctx, Row) :-
    %% Clean session data from saved facts
    (   audit3_clean_dist(C, CleanDist)
    ->  dist_top_type_prob(CleanDist, CleanTopType, CleanPTop),
        dist_entropy_norm(CleanDist, CleanH),
        clean_disagree(C, Ctx, CleanDist, CleanHardD)
    ;   CleanTopType = missing,
        CleanPTop = -1.0,
        CleanH    = -1.0,
        CleanHardD = false
    ),

    %% Accumulated session data from maxent_dist/3
    (   maxent_classifier:maxent_distribution(C, Ctx, AccumDist)
    ->  dist_top_type_prob(AccumDist, AccumTopType, AccumPTop),
        dist_entropy_norm(AccumDist, AccumH),
        (   catch(maxent_classifier:maxent_disagreement(C, Ctx, hard(_, _)), _, fail)
        ->  AccumHardD = true
        ;   AccumHardD = false
        )
    ;   AccumTopType = missing,
        AccumPTop = -1.0,
        AccumH    = -1.0,
        AccumHardD = false
    ),

    prolog_bool(CleanHardD, CleanHardStr),
    prolog_bool(AccumHardD, AccumHardStr),

    format(atom(Row),
        '{"id":"~w","clean_top_type":"~w","clean_H":~6f,"clean_P_top":~6f,\c
"clean_hard_disagree":~w,"accum_top_type":"~w","accum_H":~6f,\c
"accum_P_top":~6f,"accum_hard_disagree":~w}',
        [C,
         CleanTopType, CleanH, CleanPTop, CleanHardStr,
         AccumTopType, AccumH, AccumPTop, AccumHardStr]).


/* ================================================================
   DISTRIBUTION HELPERS
   ================================================================ */

%% dist_top_type_prob(+Dist, -TopType, -PTop)
dist_top_type_prob(Dist, TopType, PTop) :-
    foldl(update_max_pair, Dist, none-(-1.0), TopType-PTop).

update_max_pair(Type-P, _-CurP, Type-P) :- P > CurP, !.
update_max_pair(_, Cur, Cur).

%% dist_entropy_norm(+Dist, -HNorm)
%  Normalized Shannon entropy H/log(N) where N = length(Dist).
dist_entropy_norm(Dist, HNorm) :-
    foldl(add_entropy_term, Dist, 0.0, H),
    length(Dist, N),
    (   N > 1
    ->  HMax is log(float(N)),
        HNorm is H / HMax
    ;   HNorm = 0.0
    ).

add_entropy_term(_Type-P, Acc, Acc1) :-
    (   P > 0.0
    ->  Term is -P * log(P),
        Acc1 is Acc + Term
    ;   Acc1 = Acc
    ).

%% clean_disagree(+C, +Context, +CleanDist, -Hard)
%  Hard disagreement under clean session: MaxEntTop \= DetType,
%  where DetType comes from drl_core:dr_type (not affected by MaxEnt session).
clean_disagree(C, Ctx, CleanDist, Hard) :-
    dist_top_type_prob(CleanDist, ShadowType, _),
    (   catch(drl_core:dr_type(C, Ctx, DetType), _, fail)
    ->  (   ShadowType \= DetType,
            DetType \= unknown,
            DetType \= naturalized
        ->  Hard = true
        ;   Hard = false
        )
    ;   Hard = false
    ).

%% prolog_bool(+Bool, -Atom)
prolog_bool(true,  true).
prolog_bool(false, false).
