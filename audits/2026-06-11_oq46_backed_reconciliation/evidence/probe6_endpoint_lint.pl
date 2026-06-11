% W4 one-time endpoint-membership lint query (pre-registered 2026-06-11).
% For every dual-representation constraint (authored scalar suppression AND a
% suppression_requirement series), compare the scalar to the series ENDPOINT
% (value at max T). Buckets: exact match / |d| =< 0.05 / violation (|d| > 0.05).
% Pinned criterion: violations > 0 -> standing-lint question stays open;
% violations = 0 -> lint question closed-no-demonstrated-content.
:- [stack].
:- corpus_loader:load_all_testsets.

endpoint(C, V) :-
    findall(T-Val, narrative_ontology:measurement(_, C, suppression_requirement, T, Val), Pairs),
    Pairs \= [],
    sort(Pairs, Sorted),
    last(Sorted, _-V).

run :-
    findall(C-Scal-End,
        ( corpus_loader:corpus_constraint(C),
          narrative_ontology:constraint_metric(C, suppression_requirement, Scal),
          endpoint(C, End) ),
        Rows),
    length(Rows, N),
    format("dual_representation_constraints = ~w~n", [N]),
    aggregate_all(count, ( member(_-S-E, Rows), S =:= E ), NExact),
    aggregate_all(count, ( member(_-S-E, Rows), S =\= E, abs(S-E) =< 0.05 ), NNear),
    aggregate_all(count, ( member(_-S-E, Rows), abs(S-E) > 0.05 ), NViol),
    format("exact_match = ~w~nnear (0 < |d| =< 0.05) = ~w~nVIOLATIONS (|d| > 0.05) = ~w~n",
           [NExact, NNear, NViol]),
    forall(( member(C-S-E, Rows), abs(S-E) > 0.05 ),
           ( D is abs(S-E), format("  violation: ~w scalar=~2f endpoint=~2f |d|=~2f~n", [C,S,E,D]) )).
