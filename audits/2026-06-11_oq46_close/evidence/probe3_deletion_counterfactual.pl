/* OQ-46 counterfactual: timelines as-is vs with stopgap rows -> unknown. */
:- [stack].
row_time(C, T) :- narrative_ontology:measurement(_, C, _, T, _).

dedup([], []).
dedup([X], [X]) :- !.
dedup([X,X|R], Out) :- !, dedup([X|R], Out).
dedup([X,Y|R], [X|Out]) :- dedup([Y|R], Out).

run_probe :-
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Ctx),
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    findall(C, ( member(C, Cs), once(row_time(C, _)) ), MCs0), sort(MCs0, MCs),
    length(MCs, NM),
    format("measurement-bearing constraints: ~w~n", [NM]),
    findall(C, ( member(C, MCs), affected(Ctx, C) ), Aff0), sort(Aff0, Aff),
    length(Aff, NAff),
    format("constraints whose timeline CHANGES if stopgap rows -> unknown: ~w~n~n", [NAff]),
    forall(member(C, Aff),
           ( timelines(Ctx, C, Cur, CF),
             dedup(Cur, CurD), dedup(CF, CFD),
             format("  ~w~n    current      : ~w~n    counterfactual: ~w~n", [C, CurD, CFD]) )),
    % kernel-registry exposure: affected constraints that are readings (cs_kernel_id-linked)
    findall(C, ( member(C, Aff),
                 narrative_ontology:cs_kernel_id(C, _) ), KAff0), sort(KAff0, KAff),
    format("~naffected constraints carrying cs_kernel_id (kernel-registry T=0 exposure): ~w~n", [KAff]),
    % T=0 specifically: which affected constraints lack temporal supp at 0
    findall(C, ( member(C, Aff),
                 \+ narrative_ontology:measurement(_, C, suppression_requirement, 0, _) ), T0s0),
    sort(T0s0, T0s),
    format("affected constraints with NO temporal supp at T=0 (classify_at_time(C,0,..) -> unknown): ~w~n", [T0s]).

affected(Ctx, C) :-
    timelines(Ctx, C, Cur, CF),
    Cur \== CF, !.

timelines(Ctx, C, Cur, CF) :-
    setof(T, row_time(C, T), Ts),
    findall(Ty, ( member(T, Ts),
                  once(drl_composition:classify_at_time(C, T, Ctx, Ty)) ), Cur),
    findall(Ty2, ( member(T, Ts),
                   (   narrative_ontology:measurement(_, C, suppression_requirement, T, _)
                   ->  once(drl_composition:classify_at_time(C, T, Ctx, Ty2))
                   ;   Ty2 = unknown ) ), CF).
