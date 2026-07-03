% OQ-193 Campaign 3 — per-consumer price of a same-kernel guard in
% constraint_neighbors_existing/2. Strip discriminant = cs_kernel_id equality
% (the CORRECT one per HOLD_FINDINGS; the earlier typed-sibling one under-strips).
% Reversible strip with verified restore; caches cleared around every mutation.
%
% Consumers diffed (strip-incident endpoints only):
%   FPN        — drl_purity_network:effective_purity/4      (hypothesis: NO DIFF, OQ-23 guard)
%   json_report— constraint_neighbors/3 neighbor sets        (expected: DIFF)
%   net_dyn    — degree + hub status (network_hub_degree_threshold crossing)
%   net_dyn    — network_drift_severity/3
%   giant_comp — covered separately by probe_giant_ripple.pl
%
% POSITIVE CONTROLS:
%   PC_SUBSTRATE — raw affects_constraint count drops by exactly the strip count.
%   PC_FPN_DETECTS — a planted CROSS-kernel single-edge strip changes some endpoint's
%     effective_purity (proves the FPN read would register an explicit-edge change;
%     tried over a sample of existing cross-kernel edges; restore verified per try).
:- initialization((catch(run,E,(print_message(error,E),halt(2))),halt(0))).

:- use_module(corpus_loader).
:- use_module(cache_registry).
:- use_module(drl_purity_network).
:- use_module(network_dynamics).

ctx(Ctx) :- constraint_indexing:default_context(Ctx).

strip_edge(A,B) :-
    narrative_ontology:affects_constraint(A,B),
    narrative_ontology:cs_kernel_id(A,K), narrative_ontology:cs_kernel_id(B,K).

cross_edge(A,B) :-
    narrative_ontology:affects_constraint(A,B),
    \+ ( narrative_ontology:cs_kernel_id(A,K), narrative_ontology:cs_kernel_id(B,K) ).

ep_of(C, Ctx, EPr) :-
    catch(drl_purity_network:effective_purity(C,Ctx,EP,_),_,fail),
    EPr is round(EP*10000)/10000.

nbr_set(C, Ctx, Ns) :-
    catch(drl_purity_network:constraint_neighbors(C,Ctx,Nb),_,fail),
    findall(O, member(neighbor(O,_,_),Nb), Os0), sort(Os0,Ns).

snapshot(Eps, Ctx, snap(Fpn,Nbrs,Hubs,Sevs)) :-
    findall(C-E,  (member(C,Eps), ep_of(C,Ctx,E)), F0), sort(F0,Fpn),
    findall(C-Ns, (member(C,Eps), nbr_set(C,Ctx,Ns)), N0), sort(N0,Nbrs),
    config:param(network_hub_degree_threshold, HT),
    findall(C-hub(D,H), (member(C-Ns,N0), length(Ns,D), (D>=HT->H=yes;H=no)), H0), sort(H0,Hubs),
    findall(C-S, (member(C,Eps),
                  catch(network_dynamics:network_drift_severity(C,Ctx,S),_,fail)), S0), sort(S0,Sevs).

diff_count(Base, New, Label) :-
    ord_subtract(Base, New, R), ord_subtract(New, Base, A),
    length(R,NR), length(A,NA),
    findall(C, (member(C-_,R) ; member(C-_,A)), Cs0), sort(Cs0, Cs), length(Cs, NC),
    ( NR=:=0, NA=:=0
    -> format("  ~w : NO DIFF~n",[Label])
    ;  format("  ~w : CHANGED  -~w +~w  (~w endpoints)~n",[Label,NR,NA,NC]),
       ( R=[Ex|_] -> format("      e.g. removed: ~w~n",[Ex]) ; true ),
       ( A=[Ex2|_] -> format("      e.g. added  : ~w~n",[Ex2]) ; true ) ).

clear :- cache_registry:clear_all_caches.

% --- PC_FPN_DETECTS: try existing cross-kernel edges; strip one, look for an EP move ---
try_cross_control([], _) :-
    format("PC_FPN_DETECTS none_of_sample_moved_purity~n").
try_cross_control([A-B|T], Ctx) :-
    ( ep_of(A,Ctx,EA0) -> true ; EA0=none ), ( ep_of(B,Ctx,EB0) -> true ; EB0=none ),
    retract(narrative_ontology:affects_constraint(A,B)), clear,
    ( ep_of(A,Ctx,EA1) -> true ; EA1=none ), ( ep_of(B,Ctx,EB1) -> true ; EB1=none ),
    assertz(narrative_ontology:affects_constraint(A,B)), clear,
    ( ep_of(A,Ctx,EA2) -> true ; EA2=none ), ( ep_of(B,Ctx,EB2) -> true ; EB2=none ),
    ( (EA0 \== EA1 ; EB0 \== EB1)
    -> format("PC_FPN_DETECTS ok  edge ~w->~w  ep(~w): ~w->~w  ep(~w): ~w->~w~n",
              [A,B,A,EA0,EA1,B,EB0,EB1]),
       ( EA0==EA2, EB0==EB2 -> format("PC_RESTORE ok~n") ; format("PC_RESTORE FAIL ~w ~w ~w ~w~n",[EA0,EA2,EB0,EB2]) )
    ;  ( EA0==EA2, EB0==EB2 -> true ; format("PC_RESTORE FAIL(mid) ~w->~w~n",[A,B]) ),
       try_cross_control(T, Ctx) ).

run :-
    ( getenv('CORPUS',D), D\=='' -> retractall(config:param(corpus_path,_)), assertz(config:param(corpus_path,D)) ; true ),
    corpus_loader:ensure_corpus_loaded,
    ctx(Ctx),
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format("~n===== OQ-193 CONSUMER PRICE PROBE (corpus n=~w) =====~n",[NC]),

    findall(A-B, strip_edge(A,B), S0), sort(S0, StripEdges), length(StripEdges, NStrip),
    findall(X, (member(A-B,StripEdges),(X=A;X=B)), E0), sort(E0, Endpoints), length(Endpoints, NEp),
    aggregate_all(count, narrative_ontology:affects_constraint(_,_), NAff0),
    format("  same-kernel strip set: ~w edges, ~w incident endpoints; total affects=~w~n",
           [NStrip, NEp, NAff0]),

    snapshot(Endpoints, Ctx, snap(FpnB,NbrB,HubB,SevB)),

    forall(member(A-B,StripEdges), retract(narrative_ontology:affects_constraint(A,B))), clear,
    aggregate_all(count, narrative_ontology:affects_constraint(_,_), NAff1),
    Drop is NAff0-NAff1,
    ( Drop =:= NStrip -> format("PC_SUBSTRATE ok (dropped ~w == strip ~w)~n",[Drop,NStrip])
    ; format("PC_SUBSTRATE FAIL (dropped ~w != strip ~w)~n",[Drop,NStrip]) ),

    snapshot(Endpoints, Ctx, snap(FpnS,NbrS,HubS,SevS)),

    forall(member(A-B,StripEdges), assertz(narrative_ontology:affects_constraint(A,B))), clear,
    aggregate_all(count, narrative_ontology:affects_constraint(_,_), NAff2),
    ( NAff2 =:= NAff0 -> format("PC_RESTORE_SUBSTRATE ok (~w)~n",[NAff2])
    ; format("PC_RESTORE_SUBSTRATE FAIL (~w != ~w)~n",[NAff2,NAff0]) ),

    format("~n-- per-consumer diff under same-kernel strip (endpoints only) --~n"),
    diff_count(FpnB, FpnS, "FPN effective_purity              "),
    diff_count(NbrB, NbrS, "json_report neighbor sets         "),
    diff_count(HubB, HubS, "network_dynamics degree/hub status"),
    diff_count(SevB, SevS, "network_drift_severity            "),

    % hub FLIPS specifically (yes<->no), the verdict-relevant subset
    findall(C, (member(C-hub(_,H0h),HubB), member(C-hub(_,H1h),HubS), H0h\==H1h), Flips0),
    sort(Flips0,Flips), length(Flips,NFlips),
    format("  hub-status FLIPS (yes<->no): ~w~n",[NFlips]),
    ( Flips=[Fx|_] -> format("      e.g. ~w~n",[Fx]) ; true ),

    format("~n-- PC_FPN_DETECTS (planted cross-kernel single-edge strips) --~n"),
    findall(A-B, cross_edge(A,B), X0), sort(X0, Xs),
    length(Xs, NX), format("  cross-kernel edge pool: ~w (sampling up to 20)~n",[NX]),
    ( NX =:= 0 -> format("PC_FPN_DETECTS no_cross_kernel_edges_on_leg~n")
    ; length(Sample, M), M is min(20,NX), append(Sample,_,Xs),
      try_cross_control(Sample, Ctx) ).
