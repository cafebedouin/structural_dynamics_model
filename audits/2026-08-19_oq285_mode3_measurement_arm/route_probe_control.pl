% OQ-285 step 2 — DISCRIMINATION CONTROL for absence_route_census.pl.
% Two-sided: the census must FIRE on planted route-a and route-b seats and
% DECLINE on the unmodified corpus (and again after restore).
% Same path: plants are enumerated by census/1 itself, not by a side call.

control :-
    format("~n--- BASELINE (unmodified live leg) ---~n", []),
    coarse(B), print_counts(B),

    format("~n--- PLANT 1: route a (valid role, BOGUS exit atom) ---~n", []),
    corpus_loader:corpus_constraint(C1), !,
    assertz(narrative_ontology:constraint_stakeholder(
        C1, ctrl_bogus_exit, payer, moderate, medium_term, no_such_exit_atom, national)),
    coarse(P1), print_counts(P1),

    format("~n--- PLANT 2: route a (BOGUS role AND BOGUS power) ---~n", []),
    assertz(narrative_ontology:constraint_stakeholder(
        C1, ctrl_bogus_role, no_such_role, no_such_power, medium_term, mobile, national)),
    coarse(P2), print_counts(P2),

    format("~n--- PLANT 3: route b (well-formed seat on a constraint with NO base_extractiveness) ---~n", []),
    assertz(corpus_loader:corpus_constraint(ctrl_metricless_constraint)),
    assertz(narrative_ontology:constraint_stakeholder(
        ctrl_metricless_constraint, ctrl_ok_seat, payer, moderate, medium_term, mobile, national)),
    coarse(P3), print_counts(P3),

    format("~n--- RESTORE ---~n", []),
    retractall(narrative_ontology:constraint_stakeholder(_, ctrl_bogus_exit, _,_,_,_,_)),
    retractall(narrative_ontology:constraint_stakeholder(_, ctrl_bogus_role, _,_,_,_,_)),
    retractall(narrative_ontology:constraint_stakeholder(ctrl_metricless_constraint, _, _,_,_,_,_)),
    retractall(corpus_loader:corpus_constraint(ctrl_metricless_constraint)),
    coarse(R), print_counts(R),
    (   B == R
    ->  format("  RESTORE VERIFIED: baseline == post-restore~n", [])
    ;   format("  *** RESTORE FAILED ***~n", [])
    ).

coarse(Sums) :-
    stakeholder_seats:stakeholder_seats_cleanup,       % drop memoized obstruction
    census(Counts),
    findall(Bk-V, (member(K-V, Counts), bucket(K, Bk)), Bs),
    keysort(Bs, BsS), sum_buckets(BsS, Sums).

print_counts(S) :- forall(member(K-V, S), format("  ~w~t~45| ~d~n", [K, V])).
