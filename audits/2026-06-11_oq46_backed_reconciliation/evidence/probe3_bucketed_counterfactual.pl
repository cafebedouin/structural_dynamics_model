:- [stack].
:- corpus_loader:load_all_testsets.

% Counterfactual Backed bit: sanctioned-scalar semantics.
% Backed' = eps authored at T  AND  (supp authored at T  OR  constraint has NO supp series).
cf_backed(C, T, true) :-
    narrative_ontology:measurement(_, C, base_extractiveness, T, _),
    (   narrative_ontology:measurement(_, C, suppression_requirement, T, _) -> true
    ;   \+ narrative_ontology:measurement(_, C, suppression_requirement, _, _) ), !.
cf_backed(_, _, false).

% re-scan transitions with a supplied backed-bit list
scan([], [], 0, 0).
scan([_], [_], 0, 0).
scan([s(_,Ty1,B1), s(T2,Ty2,B2)|R], [s(T2,Ty2,B2)|R2], F, FA) :-
    scan([s(T2,Ty2,B2)|R], R2, F0, FA0),
    (   Ty1 == Ty2 -> F = F0, FA = FA0
    ;   B1 == true, B2 == true -> F is F0 + 1, FA = FA0
    ;   F = F0, FA is FA0 + 1 ).

run :-
    temporal_residual:residual_contexts(Ctxs),
    % totals: current vs counterfactual flips and fab-adjacent
    findall(c(C,Lab,F1,FA1,F2,FA2),
        ( corpus_loader:corpus_constraint(C),
          once(narrative_ontology:measurement(_, C, _, _, _)),
          member(Ctx, Ctxs), temporal_residual:context_label(Ctx, Lab),
          temporal_residual:snapshot_seq(C, Ctx, Seq),
          findall(s(T,Ty,B), member(state(T,Ty,_,_,_,_,B), Seq), Cur),
          scan(Cur, _, F1, FA1),
          findall(s(T,Ty,B2), ( member(state(T,Ty,_,_,_,_,_), Seq), cf_backed(C,T,B2) ), CF),
          scan(CF, _, F2, FA2) ),
        Rows),
    aggregate_all(sum(F1), member(c(_,_,F1,_,_,_), Rows), TF1),
    aggregate_all(sum(FA1), member(c(_,_,_,FA1,_,_), Rows), TFA1),
    aggregate_all(sum(F2), member(c(_,_,_,_,F2,_), Rows), TF2),
    aggregate_all(sum(FA2), member(c(_,_,_,_,_,FA2), Rows), TFA2),
    format("current:        flips=~w fab_adjacent=~w~n", [TF1, TFA1]),
    format("counterfactual: flips=~w fab_adjacent=~w~n", [TF2, TFA2]),
    forall(( member(c(C,Lab,F1,FA1,F2,FA2), Rows), (F1 \== F2 ; FA1 \== FA2) ),
           format("  changed: ~w @ ~w  flips ~w->~w  fabadj ~w->~w~n", [C,Lab,F1,F2,FA1,FA2])),
    % scalar-vs-series divergence on the 39 dual-representation constraints
    findall(C-Scal-Mean,
        ( corpus_loader:corpus_constraint(C),
          narrative_ontology:constraint_metric(C, suppression_requirement, Scal),
          findall(V, narrative_ontology:measurement(_, C, suppression_requirement, _, V), Vs),
          Vs \= [], sum_list(Vs, Sum), length(Vs, N), Mean is Sum / N ),
        Divs),
    length(Divs, ND),
    format("dual_rep_constraints = ~w~n", [ND]),
    aggregate_all(max(AD), ( member(_-S-M, Divs), AD is abs(S-M) ), MaxD),
    format("max_abs_scalar_minus_seriesmean = ~2f~n", [MaxD]),
    forall(( member(C-S-M, Divs), abs(S-M) > 0.1 ),
           ( D is abs(S-M), format("  divergent: ~w scalar=~2f series_mean=~2f |d|=~2f~n", [C,S,M,D]) )).
