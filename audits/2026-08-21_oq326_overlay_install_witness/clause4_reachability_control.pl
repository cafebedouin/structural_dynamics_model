:- dynamic sh:p/2.
% POSITIVE CONTROL for the clause-4 test: simulate the BINDING LEAK.
% warn_if_rule_clauses/1 binds the caller's template to a rule head; snapshot/2
% then collects only facts unifying with that NARROWED head, so other facts
% matching the ORIGINAL template survive and shadow the appended replacement.
leak_case :-
    retractall(sh:p(_,_)),
    assertz(sh:p(a,1)), assertz(sh:p(a,9)),
    Template = p(a,_),
    Narrowed = p(a,1),                       % what the leak leaves behind
    findall(I, (copy_term(Narrowed,I), clause(sh:I,true)), Snap),
    format("  snapshot under LEAK (template narrowed to ~q): ~q~n",[Narrowed,Snap]),
    forall(member(I,Snap), retract(sh:I)),
    assertz(sh:p(a,2)),                      % the replacement, appended LAST
    findall(S,(S=p(_,_),clause(sh:S,true), \+ \+ S = Template),Surv),
    format("  survivors at TEMPLATE shape ~q: ~q~n",[Template,Surv]),
    ( Surv = [First|_], First \= p(a,2)
    -> format("  *** clause4 FIRES: ~q shadows the replacement p(a,2)~n",[First]),
       once(sh:p(a,X)), format("  and once(p(a,X)) selects X=~w -- replacement UNREACHABLE~n",[X])
    ;  format("  clause4 declines~n") ).

clean_case :-
    retractall(sh:p(_,_)),
    assertz(sh:p(a,1)), assertz(sh:p(a,9)),
    Template = p(a,_),
    findall(I,(copy_term(Template,I),clause(sh:I,true)),Snap),
    format("  snapshot with NO leak (full template ~q): ~q~n",[Template,Snap]),
    forall(member(I,Snap), retract(sh:I)),
    assertz(sh:p(a,2)),
    findall(S,(S=p(_,_),clause(sh:S,true), \+ \+ S = Template),Surv),
    exclude(==(p(a,2)),Surv,Others),
    format("  survivors (excl. replacement): ~q~n",[Others]),
    ( Others == [] -> format("  clause4 DECLINES: replacement reachable~n") ; format("  *** fires~n") ),
    once(sh:p(a,Y)), format("  once(p(a,Y)) selects Y=~w -- replacement reachable~n",[Y]).

go :- format("POSITIVE CONTROL (leak present):~n"), leak_case,
      format("~nNEGATIVE (leak fixed):~n"), clean_case.
