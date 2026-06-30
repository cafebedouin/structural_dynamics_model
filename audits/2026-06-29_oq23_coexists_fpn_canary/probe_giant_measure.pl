:- initialization((catch(run,E,(print_message(error,E),halt(2))),halt(0))).
:- use_module(giant_component_analysis).
run :-
    ( getenv('CORPUS',D), D\=='' -> retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,D)) ; true ),
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    giant_component_analysis:all_corpus_constraints(Cs),
    retractall(giant_component_analysis:gc_edge(_,_,_,_)),
    retractall(giant_component_analysis:gc_edges_precomputed),
    retractall(adj(_,_)),
    giant_component_analysis:precompute_all_edges(Cs, Ctx),
    config:param(network_coupling_threshold, Thresh),
    giant_component_analysis:edges_at_threshold(Thresh, Edges),
    giant_component_analysis:build_adjacency_facts(Edges),
    giant_component_analysis:compute_components(Cs, Comps),
    length(Comps, NComp),
    ( Comps = [component(G,_)|_] -> true ; G=0 ),
    format("~nGIANT_COMP WITH FIX (zero-change witness): components=~w giant=~w  (pre-fix baseline was 276/334 kernel_v1, 66/12 testsets)~n",[NComp,G]).
