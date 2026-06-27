% B1-scan v2: A->B is an upward/repair step iff B decays-to-A via transitive
% closure of the 8 transition_path/4 decay edges. `unknown` excluded (off the
% health ordering, OQ-37). Direction-neutral instrument: snapshot_type series.
decay_edge(rope, tangled_rope).
decay_edge(tangled_rope, snare).
decay_edge(rope, piton).
decay_edge(scaffold, piton).
decay_edge(scaffold, snare).
decay_edge(scaffold, tangled_rope).
decay_edge(snare, piton).
decay_edge(snare, false_mountain).
decays_to(X,Y) :- decay_edge(X,Y).
decays_to(X,Y) :- decay_edge(X,Z), decays_to(Z,Y).
% upward step: A->B where B can decay down to A (i.e. A is "better" than B)
upward(A,B) :- A \== unknown, B \== unknown, decays_to(B,A).

chain_steps([_],[]) :- !.
chain_steps([A,B|T],[A-B|R]) :- chain_steps([B|T],R).

scan_b1 :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format('CORPUS_CONSTRAINT=~w~n',[NC]),
    findall(C-Chain,
            ( corpus_loader:corpus_constraint(C),
              transition_paths:degradation_chain(C, Chain, _) ),
            Chains),
    length(Chains, NChains),
    format('NONTRIVIAL_CHAINS=~w~n',[NChains]),
    findall(C2-(A-B),
            ( member(C2-Ch, Chains), chain_steps(Ch, Steps), member(A-B, Steps), upward(A,B) ),
            Ups),
    length(Ups, NUp),
    format('UPWARD_RUNS=~w~n',[NUp]),
    ( member(C3-Step, Ups), format('  UPWARD ~w : ~w~n',[C3,Step]), fail ; true ).
