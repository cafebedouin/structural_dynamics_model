% Counts-only breadth check of the feeder logic against a twin corpus.
% Usage: swipl -q -g "main('testsets_flash')" probe_twin_counts.pl
:- [stack].
:- use_module(stakeholder_seats).
:- use_module(narrative_ontology).

nseats(C, N) :- aggregate_all(count, narrative_ontology:constraint_stakeholder(C,_,_,_,_,_,_), N).
types_B(C, Types) :-
    findall(Ty, ( narrative_ontology:constraint_stakeholder(C, N, _, _, _, _, _),
                  stakeholder_seats:dr_type_for_stakeholder(C, N, Ty), Ty \= unknown ), Ts),
    sort(Ts, Types).
verdict(C, V) :-
    nseats(C, NS), types_B(C, Ty),
    ( NS < 2 -> V = abstain ; Ty = [_,_|_] -> V = gap ; V = no_gap ).

main(Dir) :-
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    length(Cs, Total),
    findall(x,(member(C,Cs),verdict(C,gap)),G), length(G,NG),
    findall(x,(member(C,Cs),verdict(C,no_gap)),N), length(N,NN),
    findall(x,(member(C,Cs),verdict(C,abstain)),A), length(A,NA),
    % all-unknown-but-seated (OPEN-C population)
    findall(x,(member(C,Cs),nseats(C,S),S>=2,types_B(C,[])),U), length(U,NU),
    format("~n=== ~w  TOT:~w GAP:~w NO_GAP:~w ABSTAIN:~w (allunknown-seated:~w) ===~n",
           [Dir,Total,NG,NN,NA,NU]),
    halt.
