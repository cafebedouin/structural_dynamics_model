:- [stack].
:- corpus_loader:load_all_testsets.

% NAIVE blanket reconciliation: any authored suppression (series-at-T OR scalar) = backed.
cf2_backed(C, T, true) :-
    narrative_ontology:measurement(_, C, base_extractiveness, T, _),
    (   narrative_ontology:measurement(_, C, suppression_requirement, T, _) -> true
    ;   narrative_ontology:constraint_metric(C, suppression_requirement, _) ), !.
cf2_backed(_, _, false).

scan([], 0, 0).
scan([_], 0, 0).
scan([s(_,Ty1,B1), s(T2,Ty2,B2)|R], F, FA) :-
    scan([s(T2,Ty2,B2)|R], F0, FA0),
    (   Ty1 == Ty2 -> F = F0, FA = FA0
    ;   B1 == true, B2 == true -> F is F0 + 1, FA = FA0
    ;   F = F0, FA is FA0 + 1 ).

run :-
    temporal_residual:residual_contexts(Ctxs),
    findall(F2-FA2,
        ( corpus_loader:corpus_constraint(C),
          once(narrative_ontology:measurement(_, C, _, _, _)),
          member(Ctx, Ctxs),
          temporal_residual:snapshot_seq(C, Ctx, Seq),
          findall(s(T,Ty,B2), ( member(state(T,Ty,_,_,_,_,_), Seq), cf2_backed(C,T,B2) ), CF),
          scan(CF, F2, FA2) ),
        Rows),
    aggregate_all(sum(F), member(F-_, Rows), TF),
    aggregate_all(sum(FA), member(_-FA, Rows), TFA),
    format("blanket_backed: flips=~w fab_adjacent=~w  (current: 59/20)~n", [TF, TFA]).
