% Giant-component ripple witness: does stripping same-kernel sibling
% affects_constraint edges (the proposed FPN-fix discriminant) change giant_comp
% connectivity? Reversible strip; positive control = raw affects_constraint count
% must drop by the stripped count (proves the strip reached the topology layer).
:- initialization((catch(run,E,(print_message(error,E),halt(2))),halt(0))).
:- use_module(giant_component_analysis).
:- use_module(cache_registry).  % 2026-07-02: not pulled in transitively at HEAD

strip_edge(A,B) :- narrative_ontology:affects_constraint(A,B),
                   narrative_ontology:cs_kernel_id(A,K), narrative_ontology:cs_kernel_id(B,K).

measure(Cs, Ctx, NComp, GiantSize, NE, NAffects) :-
    retractall(giant_component_analysis:gc_edge(_,_,_,_)),
    retractall(giant_component_analysis:gc_edges_precomputed),
    retractall(giant_component_analysis:gc_inferred_edge(_,_,_)),
    retractall(adj(_,_)),
    cache_registry:clear_all_caches,
    giant_component_analysis:precompute_all_edges(Cs, Ctx),
    config:param(network_coupling_threshold, Thresh),
    giant_component_analysis:edges_at_threshold(Thresh, Edges),
    length(Edges, NE),
    giant_component_analysis:build_adjacency_facts(Edges),
    giant_component_analysis:compute_components(Cs, Components),
    length(Components, NComp),
    ( Components = [component(GiantSize,_)|_] -> true ; GiantSize = 0 ),
    aggregate_all(count, narrative_ontology:affects_constraint(_,_), NAffects).

run :-
    ( getenv('CORPUS',D), D\=='' -> retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path,D)) ; true ),
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:default_context(Ctx),
    giant_component_analysis:all_corpus_constraints(Cs),
    length(Cs, NC),
    findall(A-B, strip_edge(A,B), StripEdges0), sort(StripEdges0, StripEdges),
    length(StripEdges, NStrip),

    measure(Cs, Ctx, NComp0, Giant0, NE0, NAff0),
    forall(member(A-B, StripEdges), retract(narrative_ontology:affects_constraint(A,B))),
    measure(Cs, Ctx, NComp1, Giant1, NE1, NAff1),
    forall(member(A-B, StripEdges), assertz(narrative_ontology:affects_constraint(A,B))),

    format("~n===== GIANT-COMPONENT RIPPLE (corpus, ~w constraints) =====~n", [NC]),
    format("  same-kernel affects_constraint edges to strip : ~w~n", [NStrip]),
    format("  -- POSITIVE CONTROL (strip reached graph layer) --~n"),
    Drop is NAff0 - NAff1,
    format("  raw affects_constraint: ~w -> ~w  (dropped ~w; expected ~w)~n", [NAff0, NAff1, Drop, NStrip]),
    ( Drop =:= NStrip -> format("  [ok] strip applied to substrate~n") ; format("  [!!] drop != strip count — investigate~n") ),
    format("  -- giant_comp topology old -> new --~n"),
    format("  gc edges (at threshold)   : ~w -> ~w  (delta ~w)~n", [NE0, NE1, NE1-NE0]),
    format("  connected components      : ~w -> ~w  (delta ~w)~n", [NComp0, NComp1, NComp1-NComp0]),
    format("  giant component size      : ~w -> ~w  (delta ~w)~n", [Giant0, Giant1, Giant1-Giant0]),
    ( NComp0 =:= NComp1, Giant0 =:= Giant1
    ->  format("  => RIPPLE NEGLIGIBLE: component structure unchanged (siblings held by other edges)~n")
    ;   format("  => RIPPLE PRESENT: connectivity changed — correction-vs-loss call needed~n") ).
