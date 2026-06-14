% Read-only probe: per-constraint seat types + gap verdict using the CANONICAL
% seat path (stakeholder_seats:dr_type_for_stakeholder/3). No writes.
:- initialization(main).

:- [stack].
:- use_module(stakeholder_seats).
:- use_module(narrative_ontology).

functional_type(rope).
functional_type(naturalized).
functional_type(scaffold).
functional_type(mountain).
extractive_type(snare).
extractive_type(tangled_rope).

seat_readings(C, Rs) :-
    findall(D-P-T-N,
            ( narrative_ontology:constraint_stakeholder(C, N, _, P, _, _, _),
              stakeholder_seats:dr_type_for_stakeholder(C, N, T) ),
            Rs).

nonunknown_types(Rs, Types) :-
    findall(T, (member(_-_-T-_, Rs), T \= unknown), Ts),
    sort(Ts, Types).

verdict(C, Verdict, Seats, Types) :-
    seat_readings(C, Rs),
    Seats = Rs,
    nonunknown_types(Rs, Types),
    length(Rs, NSeats),
    (   NSeats < 2          -> Verdict = abstain
    ;   Types = [_,_|_]     -> Verdict = gap
    ;   Verdict = no_gap
    ).

main :-
    corpus_loader:ensure_corpus_loaded,
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    forall(member(C, Cs),
        ( verdict(C, V, Seats, Types),
          format("~w\t~w\ttypes=~w\tseats=~w~n", [V, C, Types, Seats]) )),
    findall(V, (member(C, Cs), verdict(C, V, _, _)), Vs),
    aggregate_all(count, member(gap, Vs), G),
    aggregate_all(count, member(no_gap, Vs), NG),
    aggregate_all(count, member(abstain, Vs), AB),
    length(Cs, Total),
    format("~n=== TOT:~w GAP:~w NO_GAP:~w ABSTAIN:~w ===~n", [Total, G, NG, AB]),
    halt.
