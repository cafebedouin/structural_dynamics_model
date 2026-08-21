:- dynamic m:p/2.
m:p(a,1).
m:p(b,2).
m:p(z,9) :- true, format("").   % a RULE clause
warn(M:T) :- ( catch(clause(M:T,B),_,fail), B \== true -> format("warn on ~q~n",[M:T]) ; true ).
go :- T = m:p(_,_), maplist(warn,[T]), format("template AFTER warn: ~q~n",[T]).
