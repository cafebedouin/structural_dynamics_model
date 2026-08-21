:- dynamic sh:p/2.
% Faithful reimplementation of the harness pipeline under the RULED order 2->3->1->4->5.
snap(M:T, Insts) :- findall(I, (copy_term(T,I), clause(M:I, true)), Insts).
rules(M:T, Hs)   :- findall(H, (copy_term(T,C), clause(M:C,B), B \== true, copy_term(C,H)), Hs).

% clause 4 at TEMPLATE shape: after retracting the snapshot, does any SURVIVING
% clause unify with the template that covers the fact being asserted?
try(Label, Templates, Facts) :-
    retractall(sh:p(_,_)),
    assertz(sh:p(a,1)), assertz(sh:p(a,9)), assertz(sh:p(b,5)),
    forall(member(M:T,Templates), ( rules(M:T,H), ( H==[] -> true ; format("  clause3 would fire on ~q~n",[M:T]) ) )),
    findall(M-I, (member(M:T,Templates), snap(M:T,Is), member(I,Is)), Snap),
    ( Snap == [] -> format("  clause1 would fire (empty snapshot)~n") ; true ),
    forall(member(Mm-I, Snap), retract(Mm:I)),
    findall(F-Cov, ( member(F,Facts), F = _:FT,
                     ( member(_:T2,Templates), \+ \+ FT = T2 -> Cov = covered(T2) ; Cov = uncovered ) ), FC),
    forall(member(F-Cov,FC),
      (  Cov = uncovered
      -> format("  clause4' would fire (undecidable) on ~q~n",[F])
      ;  Cov = covered(T2),
         findall(S, (S = p(_,_), clause(sh:S,true), \+ \+ S = T2), Surv),
         ( Surv == [] -> format("  clause4 CLEAN for ~q (no survivor at template shape ~q)~n",[F,T2])
         ; format("  *** clause4 WOULD FIRE for ~q — survivors at template shape ~q: ~q~n",[F,T2,Surv]) )
      )),
    format("~w done~n~n",[Label]).

go :-
    format("DB always: p(a,1), p(a,9), p(b,5)~n~n"),
    format("A) template p(a,_) covering fact p(a,2):~n"),
    try('A', [sh:p(a,_)], [sh:p(a,2)]),
    format("B) NARROW template p(a,1), fact p(a,2) (fact NOT covered):~n"),
    try('B', [sh:p(a,1)], [sh:p(a,2)]),
    format("C) template p(_,_) covering fact p(a,2):~n"),
    try('C', [sh:p(_,_)], [sh:p(a,2)]),
    format("D) two templates p(b,_) + p(a,_), fact p(a,2):~n"),
    try('D', [sh:p(b,_), sh:p(a,_)], [sh:p(a,2)]).
