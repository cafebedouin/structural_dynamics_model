% One leg per PROCESS. The in-process multi-leg version was unsound: retracting
% corpus_loaded/corpus_constraint does not retract the narrative_ontology facts
% the testset files asserted, so legs accumulate and sort/2 hides it behind ID
% dedup (kimi and sonnet reported byte-identical counts — the tell).

tok(V, non_number) :- \+ number(V), !.
tok(V, negative)   :- V < 0.0, !.
tok(_, value).

main :-
    current_prolog_flag(argv, [LegAtom|_]),
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, LegAtom)),
    config:param(corpus_path, Active),
    format("leg=~w active_overlay=~w~n", [LegAtom, Active]),
    ( Active == LegAtom -> true ; format("OVERLAY FAILED~n"), halt(1) ),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NLoaded),
    format("corpus_constraint facts=~w~n", [NLoaded]),
    constraint_indexing:default_context(Ctx),
    findall(C, (narrative_ontology:constraint_claim(C, _), \+ is_list(C)), Raw),
    sort(Raw, Cs), length(Cs, NC),
    drl_fpn:fpn_run(Cs, Ctx, fpn_result(_,_,Conv,_)),
    findall(row(C,OH,FP),
        (   member(C, Cs),
            fpn_report:fpn_intrinsic_safe(C, IP), IP >= 0.0,
            fpn_report:one_hop_ep_safe(C, Ctx, OH),
            fpn_report:fpn_ep_safe(C, Ctx, FP)
        ), Rows),
    length(Rows, NR),
    findall(T,(member(row(_,OH,_),Rows),tok(OH,T)),A0), msort(A0,A1), clumped(A1,OHC),
    findall(T,(member(row(_,_,FP),Rows),tok(FP,T)),B0), msort(B0,B1), clumped(B1,FPC),
    findall(Z,(member(row(_,OH,_),Rows),fpn_report:purity_zone(OH,Z)),Z1),
    findall(Z,(member(row(_,_,FP),Rows),fpn_report:purity_zone(FP,Z)),Z2),
    append(Z1,Z2,Zs), msort(Zs,ZS), clumped(ZS,ZC),
    findall(C,(member(row(C,OH,FP),Rows),
               fpn_report:purity_zone(OH,X1), fpn_report:purity_zone(FP,X2), X1\=X2), Migs),
    length(Migs,NM),
    format("constraints=~w rows=~w converged=~w~n",[NC,NR,Conv]),
    format("one_hop_token_mix=~w~n",[OHC]),
    format("fpn_token_mix=~w~n",[FPC]),
    format("zone_counts=~w~n",[ZC]),
    format("migrations=~w~n",[NM]),
    (   ( member(non_number-_,OHC) ; member(negative-_,OHC)
        ; member(non_number-_,FPC) ; member(negative-_,FPC) )
    ->  format("VERDICT=GUARD_CHANGES_OUTPUT~n")
    ;   format("VERDICT=guard_inert~n")
    ).
