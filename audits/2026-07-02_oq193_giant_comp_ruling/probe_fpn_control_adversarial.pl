% OQ-193 follow-up: ADVERSARIAL PC_FPN_DETECTS for legs where the alphabetical
% 20-sample control failed (testsets_haiku). Selects cross-kernel edges by
% EXPECTED contamination (receiver purity - donor purity, donor type factor > 0),
% then strips the top candidates one at a time, looking for an effective_purity move.
% Restore verified per try. A pass here witnesses that the FPN read on this leg
% WOULD register an explicit-edge change — making the same-kernel NO-DIFF a
% measured-empty, not a didn't-look.
:- initialization((catch(run,E,(print_message(error,E),halt(2))),halt(0))).

:- use_module(corpus_loader).
:- use_module(cache_registry).
:- use_module(drl_purity_network).

ctx(Ctx) :- constraint_indexing:default_context(Ctx).
clear :- cache_registry:clear_all_caches.

cross_edge(A,B) :-
    narrative_ontology:affects_constraint(A,B),
    \+ ( narrative_ontology:cs_kernel_id(A,K), narrative_ontology:cs_kernel_id(B,K) ).

ep_of(C, Ctx, EPr) :-
    catch(drl_purity_network:effective_purity(C,Ctx,EP,_),_,fail),
    EPr is round(EP*10000)/10000.

% expected contamination score for edge A-B (both directions; donor must have
% typed contamination strength > 0 and lower purity than receiver)
edge_score(A, B, Ctx, Score) :-
    catch(purity_scoring:purity_score(A,PA),_,fail), PA >= 0.0,
    catch(purity_scoring:purity_score(B,PB),_,fail), PB >= 0.0,
    dir_score(A, PA, B, PB, Ctx, S1),
    dir_score(B, PB, A, PA, Ctx, S2),
    Score is max(S1, S2), Score > 0.0.

dir_score(Donor, PD, _Recv, PR, Ctx, S) :-
    (   drl_core:dr_type(Donor, Ctx, DT),
        drl_purity_network:type_contamination_strength(DT, TF),
        TF > 0.0, PR > PD
    ->  S is (PR - PD) * TF
    ;   S = 0.0 ).

try_control([], _) :- format("PC_FPN_DETECTS_ADVERSARIAL FAIL none_of_candidates_moved_purity~n").
try_control([_-(A-B)|T], Ctx) :-
    ( ep_of(A,Ctx,EA0) -> true ; EA0=none ), ( ep_of(B,Ctx,EB0) -> true ; EB0=none ),
    retract(narrative_ontology:affects_constraint(A,B)), clear,
    ( ep_of(A,Ctx,EA1) -> true ; EA1=none ), ( ep_of(B,Ctx,EB1) -> true ; EB1=none ),
    assertz(narrative_ontology:affects_constraint(A,B)), clear,
    ( ep_of(A,Ctx,EA2) -> true ; EA2=none ), ( ep_of(B,Ctx,EB2) -> true ; EB2=none ),
    ( (EA0 \== EA1 ; EB0 \== EB1)
    -> format("PC_FPN_DETECTS_ADVERSARIAL ok  edge ~w->~w  ep(~w): ~w->~w  ep(~w): ~w->~w~n",
              [A,B,A,EA0,EA1,B,EB0,EB1]),
       ( EA0==EA2, EB0==EB2 -> format("PC_RESTORE ok~n") ; format("PC_RESTORE FAIL~n") )
    ;  ( EA0==EA2, EB0==EB2 -> true ; format("PC_RESTORE FAIL(mid) ~w->~w~n",[A,B]) ),
       try_control(T, Ctx) ).

run :-
    ( getenv('CORPUS',D), D\=='' -> retractall(config:param(corpus_path,_)), assertz(config:param(corpus_path,D)) ; true ),
    corpus_loader:ensure_corpus_loaded,
    ctx(Ctx),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("~n===== ADVERSARIAL FPN CONTROL (corpus n=~w) =====~n",[NC]),
    findall(S-(A-B), ( cross_edge(A,B), edge_score(A,B,Ctx,S) ), Scored0),
    sort(0, @>=, Scored0, Scored),
    length(Scored, NS),
    format("  cross-kernel edges with expected contamination > 0: ~w~n",[NS]),
    ( Scored = [TopS-(TA-TB)|_] -> format("  top candidate: ~w->~w (score ~w)~n",[TA,TB,TopS]) ; true ),
    ( NS =:= 0
    -> format("PC_FPN_DETECTS_ADVERSARIAL VACUOUS zero_expected_contamination_cross_edges~n"),
       format("  (on this leg NO cross-kernel explicit edge carries expected contamination —~n"),
       format("   the same-kernel NO-DIFF is then trivially consistent; treat FPN-unchanged as~n"),
       format("   UNWITNESSABLE-BY-STRIP on this leg, witness rides the testsets leg control)~n")
    ;  length(Cand, M), M is min(10,NS), append(Cand,_,Scored),
       try_control(Cand, Ctx) ).
