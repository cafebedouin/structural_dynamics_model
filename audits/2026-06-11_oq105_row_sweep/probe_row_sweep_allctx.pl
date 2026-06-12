% OQ-105 per-row sweep, ALL product-site contexts: same comparison as
% probe_row_sweep.pl (scalar substitution vs local-series interpolation) but
% swept over constraint_indexing's product-site context set, so the divergence
% count holds at the same altitude as the OQ-110 context-level flip census.
% Positive control: the default-context divergent triples from the first probe
% (agenda_conditioning T=10, post_1998_convergence T=13,
% technocratic_paradigm_vs_human_primacy T=9) must reappear here.
:- [stack].
:- corpus_loader:load_all_testsets.

supp_series(C, Pairs) :-
    findall(T-V, narrative_ontology:measurement(_, C, suppression_requirement, T, V), Ps),
    Ps \= [],
    msort(Ps, Pairs).

interp_at(Pairs, T, V) :-
    Pairs = [T0-V0|_],
    last(Pairs, Tn-Vn),
    (   T =< T0 -> V = V0
    ;   T >= Tn -> V = Vn
    ;   append(_, [Ta-Va, Tb-Vb|_], Pairs),
        Ta =< T, T =< Tb
    ->  V is Va + (Vb - Va) * (T - Ta) / (Tb - Ta)
    ).

run :-
    constraint_indexing:site_contexts_product(Ctxs),
    length(Ctxs, NC),
    format("contexts=~w~n", [NC]),
    nb_setval(oq105_cells, 0), nb_setval(oq105_divcells, 0),
    forall(corpus_loader:corpus_constraint(C), sweep_constraint(C, Ctxs)),
    nb_getval(oq105_cells, Cells), nb_getval(oq105_divcells, Div),
    format("~nTOTAL cells(row x ctx)=~w divergent_cells=~w~n", [Cells, Div]).

sweep_constraint(C, Ctxs) :-
    (   supp_series(C, Pairs),
        narrative_ontology:constraint_metric(C, suppression_requirement, Scalar),
        temporal_residual:constraint_time_set(C, Times),
        findall(T, ( member(T, Times),
                     \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _) ),
                Mis),
        Mis \== []
    ->  forall(member(T, Mis),
            forall(member(Ctx, Ctxs), sweep_cell(C, T, Ctx, Scalar, Pairs)))
    ;   true
    ).

sweep_cell(C, T, Ctx, Scalar, Pairs) :-
    drl_composition:classify_at_time(C, T, Ctx, TypeS, _),
    once(interp_at(Pairs, T, VI)),
    drl_composition:classify_at_time_with_supp(C, T, Ctx, VI, false, TypeI, _),
    nb_getval(oq105_cells, N0), N1 is N0 + 1, nb_setval(oq105_cells, N1),
    (   TypeS == TypeI -> true
    ;   nb_getval(oq105_divcells, D0), D1 is D0 + 1, nb_setval(oq105_divcells, D1),
        format("DIVERGENT ~w T=~w ctx=~w sub=~2f/~w interp=~2f/~w~n",
               [C, T, Ctx, Scalar, TypeS, VI, TypeI])
    ).

:- run, halt.
:- halt(1).
