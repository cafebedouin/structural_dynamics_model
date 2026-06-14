% End-to-end witness: call the REAL rewired report_generator predicates.
:- initialization(main).
:- [stack].
:- use_module(report_generator).
:- use_module(stakeholder_seats).
:- use_module(narrative_ontology).

seats_dump(C, Pairs) :-
    findall(P-T,
        ( narrative_ontology:constraint_stakeholder(C, N, _, P, _, _, _),
          stakeholder_seats:dr_type_for_stakeholder(C, N, T) ),
        Pairs).

row(C) :-
    seats_dump(C, Pairs),
    aggregate_all(count, narrative_ontology:constraint_stakeholder(C,_,_,_,_,_,_), NSeats),
    (   report_generator:detect_gap_pattern(C, gap(Pat, TLo, THi))
    ->  ( report_generator:omega_from_gap(C, gap(Pat,TLo,THi), OID, _, _) -> true ; OID = '<<NO_OMEGA>>' ),
        format("GAP\t~w\tpattern=~w(~w,~w)\tomega=~w\tseats=~w~n", [C, Pat, TLo, THi, OID, Pairs])
    ;   ( NSeats < 2 -> Cov = abstain_no_seats
        ; report_generator:gap_coverage(C) -> Cov = no_gap
        ; Cov = abstain_all_unknown ),
        format("~w\t~w\tseats=~w~n", [Cov, C, Pairs])
    ).

main :-
    corpus_loader:ensure_corpus_loaded,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    forall(member(C, Cs), row(C)),
    findall(x,(member(C,Cs),report_generator:detect_gap_pattern(C,_)),G), length(G,NG),
    findall(x,(member(C,Cs),report_generator:detect_gap_pattern(C,gap(extraction_blindness,_,_))),EB), length(EB,NEB),
    findall(x,(member(C,Cs),report_generator:detect_gap_pattern(C,gap(general_type_mismatch,_,_))),GM), length(GM,NGM),
    format("~n=== GAP:~w (extraction_blindness:~w general:~w) ===~n", [NG, NEB, NGM]),
    halt.
