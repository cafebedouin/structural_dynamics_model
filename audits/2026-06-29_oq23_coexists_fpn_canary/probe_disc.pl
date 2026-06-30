:- initialization((catch(run,E,(print_message(error,E),halt(2))),halt(0))).
run :-
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, testsets_flash)),
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    test_coexists_fpn_canary:copresent_pairs(coexists_with, Pairs),
    % pairs that LEAK but are NOT eligible
    findall(A-B, (member(A-B,Pairs),
                  test_coexists_fpn_canary:pair_leak(A-B,Ctx,_),
                  \+ test_coexists_fpn_canary:pair_eligible(A-B)), Bad),
    length(Bad, NB),
    format("~nleak-but-ineligible pairs: ~w~n",[NB]),
    ( Bad = [A-B|_] ->
        format("example: ~w <-> ~w~n",[A,B]),
        purity_scoring:purity_score(A,PA1), purity_scoring:purity_score(A,PA2),
        purity_scoring:purity_score(B,PB1), purity_scoring:purity_score(B,PB2),
        format("purity A: ~w then ~w~n",[PA1,PA2]),
        format("purity B: ~w then ~w~n",[PB1,PB2]),
        ( test_coexists_fpn_canary:pair_leak(A-B,Ctx,Cm) -> format("leak Contam=~w~n",[Cm]) ; true ),
        ( test_coexists_fpn_canary:pair_eligible(A-B) -> format("eligible NOW: yes~n") ; format("eligible NOW: no~n") )
    ; format("none~n") ).
