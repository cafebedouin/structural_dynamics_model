% B1-scan: enumerate degradation_chain over the loaded corpus; flag upward (repair)
% steps = reverse of the 8 transition_path/4 decay edges.
repair_edge(tangled_rope, rope).
repair_edge(snare, tangled_rope).
repair_edge(piton, rope).
repair_edge(piton, scaffold).
repair_edge(snare, scaffold).
repair_edge(tangled_rope, scaffold).
repair_edge(piton, snare).
repair_edge(false_mountain, snare).

chain_has_repair([A,B|_], A-B) :- repair_edge(A,B), !.
chain_has_repair([_|T], Edge) :- chain_has_repair(T, Edge).

scan_b1 :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format('CORPUS_CONSTRAINT=~w~n',[NC]),
    findall(C-Chain,
            ( corpus_loader:corpus_constraint(C),
              transition_paths:degradation_chain(C, Chain, _) ),
            Chains),
    length(Chains, NChains),
    format('NONTRIVIAL_CHAINS=~w~n',[NChains]),
    ( member(C2-Ch, Chains), format('  CHAIN ~w : ~w~n',[C2,Ch]), fail ; true ),
    findall(C3-E,
            ( member(C3-Ch3, Chains), chain_has_repair(Ch3, E) ),
            Repairs),
    length(Repairs, NRep),
    format('UPWARD_RUNS=~w~n',[NRep]),
    ( member(C4-E4, Repairs), format('  REPAIR ~w : ~w~n',[C4,E4]), fail ; true ).
