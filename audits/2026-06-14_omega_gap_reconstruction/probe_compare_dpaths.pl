% Compare gap verdict under two d-paths:
%   A) plan's literal proposal: drl_core:dr_type(C, Ctx, Type)  [power-atom d]
%   B) canonical seat path:     stakeholder_seats:dr_type_for_stakeholder/3 [role-d]
:- initialization(main).
:- [stack].
:- use_module(stakeholder_seats).
:- use_module(narrative_ontology).

ctx_of(P,T,E,S, context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S))).

types_A(C, Types) :-   % inline dr_type/3
    findall(Ty, ( narrative_ontology:constraint_stakeholder(C, _, _, P, T, E, S),
                  ctx_of(P,T,E,S,Ctx), drl_core:dr_type(C, Ctx, Ty), Ty \= unknown ), Ts),
    sort(Ts, Types).
types_B(C, Types) :-   % canonical seat path
    findall(Ty, ( narrative_ontology:constraint_stakeholder(C, N, _, _, _, _, _),
                  stakeholder_seats:dr_type_for_stakeholder(C, N, Ty), Ty \= unknown ), Ts),
    sort(Ts, Types).

nseats(C, N) :- aggregate_all(count, narrative_ontology:constraint_stakeholder(C,_,_,_,_,_,_), N).

verdict(C, A_or_B, V) :-
    nseats(C, NS),
    ( A_or_B == a -> types_A(C, Ty) ; types_B(C, Ty) ),
    ( NS < 2 -> V = abstain ; Ty = [_,_|_] -> V = gap ; V = no_gap ).

main :-
    corpus_loader:ensure_corpus_loaded,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    forall(member(C, Cs),
      ( verdict(C, a, VA), verdict(C, b, VB),
        ( VA \== VB -> format("DIFFER\t~w\tA=~w\tB=~w~n",[C,VA,VB]) ; true ) )),
    findall(x,(member(C,Cs),verdict(C,a,gap)),GA), length(GA,NA),
    findall(x,(member(C,Cs),verdict(C,b,gap)),GB), length(GB,NB),
    format("~n=== A(dr_type) gap=~w  B(seat) gap=~w ===~n",[NA,NB]),
    halt.
