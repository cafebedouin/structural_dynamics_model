gapsweep :-
    forall(member(Src,[stakeholder,canonical]),
      ( findall(Tag, ( corpus_loader:corpus_constraint(C),
                       report_generator:gap_status(C, Src, S),
                       ( S = gap(P,_,_) -> Tag = gap(P)
                       ; S = undetermined(R) -> Tag = undetermined(R)
                       ; Tag = S ) ), Ts),
        msort(Ts, So), tally(So, T),
        format("~n=== gap_status/3 source=~w ===~n",[Src]),
        forall(member(K-V,T), format("  ~w~t~40| ~d~n",[K,V])) )).
tally([], []).
tally([X|Xs], [X-K|R]) :- cs(X,Xs,K0,T), K is K0+1, tally(T,R).
cs(X,[Y|Ys],K,T) :- X==Y, !, cs(X,Ys,K0,T), K is K0+1.
cs(_,L,0,L).
