% OQ-285 step 3 — do the SIBLING absence surfaces carry a blind-vs-absent
% distinction, and do their tokens have populations?

sweep :-
    % (1) seat_perceived_vs_real/4 Computed token census over ALL seats
    %     (domain = every constraint_stakeholder, incl. excluded + non_agent).
    findall(Cm, ( corpus_loader:corpus_constraint(C),
                  narrative_ontology:constraint_stakeholder(C, N, _,_,_,_,_),
                  stakeholder_seats:seat_perceived_vs_real(C, N, _, Cm) ), Cms),
    msort(Cms, S1), tally(S1, T1),
    format("~n=== (1) seat_perceived_vs_real/4 Computed tokens (ALL seats) ===~n", []),
    forall(member(K-V, T1), format("  ~w~t~40| ~d~n", [K,V])),
    ( memberchk(untyped-_, T1) -> true ; format("  untyped~t~40| 0   <-- token never fires~n", []) ),

    % (2) stakeholder_obstruction/5 coverage: how many stories are H1-null and why
    format("~n=== (2) stakeholder_obstruction/5 — null H1 and its cause ===~n", []),
    findall(Cause,
      ( corpus_loader:corpus_constraint(C2),
        stakeholder_seats:stakeholder_obstruction(C2, H0, _H1, NS, NR),
        ( H0 == null
        ->  ( NS =:= 0 -> Cause = null_zero_seats
            ; NR =:= 0 -> Cause = null_seats_present_none_real
            ; Cause = null_one_real_seat )
        ;   Cause = computed )
      ), Causes),
    msort(Causes, S2), tally(S2, T2),
    forall(member(K-V, T2), format("  ~w~t~40| ~d~n", [K,V])),

    % (3) sheaf_undetermined_reason/2 population
    format("~n=== (3) sheaf_undetermined_reason/2 ===~n", []),
    ( catch(findall(R, ( corpus_loader:corpus_constraint(C3),
                         sheaf_analysis:sheaf_undetermined_reason(C3, R) ), Rs), E, (Rs=[], print_message(error,E))) ),
    msort(Rs, S3), tally(S3, T3),
    ( T3 == [] -> format("  (no solutions)~n", []) ; true ),
    forall(member(K-V, T3), format("  ~w~t~40| ~d~n", [K,V])),

    % (4) sheaf_status/2 values
    format("~n=== (4) sheaf_status/2 ===~n", []),
    ( catch(findall(St, ( corpus_loader:corpus_constraint(C4),
                          sheaf_analysis:sheaf_status(C4, St) ), Sts), E2, (Sts=[], print_message(error,E2))) ),
    msort(Sts, S4), tally(S4, T4),
    ( T4 == [] -> format("  (no solutions)~n", []) ; true ),
    forall(member(K-V, T4), format("  ~w~t~40| ~d~n", [K,V])),

    % (5) observer-frame orbit: is the OBSERVER H1 vector's `unknown` reachable
    %     by a FAILING derivation, or only by the cascade?
    format("~n=== (5) observer-frame orbit_vector token census ===~n", []),
    findall(T, ( corpus_loader:corpus_constraint(C5),
                 grothendieck_cohomology:orbit_vector(C5, V5), member(T, V5) ), Ts5),
    msort(Ts5, S5), tally(S5, T5),
    forall(member(K-V, T5), format("  ~w~t~40| ~d~n", [K,V])).
