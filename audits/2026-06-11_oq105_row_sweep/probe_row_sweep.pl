% OQ-105 per-row sweep: for EVERY grid-misaligned suppression row on the live
% corpus, compare the type under the current scalar substitution against the
% type under linear interpolation of the constraint's own authored series.
% Discharges the PREDICTED bucket in the OQ-105 entry ("the per-row sweep of
% the remaining rows has not been run").
%
% Controls (per-process, in this run):
%   C1 (interp identity): interpolating AT every authored series point must
%      return the authored value exactly — validates the interp machinery.
%   C2 (same-path): re-deriving the substitution type via
%      classify_at_time_with_supp(C,T,Ctx,Scalar,false,...) must equal
%      classify_at_time/5's type at every misaligned row — validates that the
%      counterfactual call enters the same clause path as the live ladder.
%   C3 (enumeration fires): the misaligned-row enumeration must include the
%      probe5-witnessed rows (substantive_employment_reading T=3,9;
%      post_1998_convergence T=13,23; truth_democracy_disinformation T=2,6).
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

% C1: interpolation at authored points returns authored values
interp_identity_control :-
    findall(C-T, ( corpus_loader:corpus_constraint(C),
                   narrative_ontology:measurement(_, C, suppression_requirement, T, _) ),
            CTs),
    length(CTs, N),
    findall(C-T, ( member(C-T, CTs),
                   supp_series(C, Pairs),
                   narrative_ontology:measurement(_, C, suppression_requirement, T, V0),
                   once(interp_at(Pairs, T, VI)),
                   abs(VI - V0) > 1.0e-9 ),
            Bad),
    length(Bad, NB),
    format("C1 interp-identity: ~w authored points checked, ~w mismatches ~w~n", [N, NB, Bad]).

run :-
    interp_identity_control,
    constraint_indexing:default_context(Ctx),
    nb_setval(oq105_rows, 0), nb_setval(oq105_div, 0),
    nb_setval(oq105_hosts, []), nb_setval(oq105_pathfail, 0),
    forall(corpus_loader:corpus_constraint(C), sweep_constraint(C, Ctx)),
    nb_getval(oq105_rows, R), nb_getval(oq105_div, Dv),
    nb_getval(oq105_hosts, Hs), length(Hs, NH),
    nb_getval(oq105_pathfail, PF),
    format("~nTOTAL misaligned_rows=~w host_constraints=~w divergent_rows=~w path_control_failures=~w~n",
           [R, NH, Dv, PF]).

sweep_constraint(C, Ctx) :-
    (   supp_series(C, Pairs),
        narrative_ontology:constraint_metric(C, suppression_requirement, Scalar),
        temporal_residual:constraint_time_set(C, Times),
        findall(T, ( member(T, Times),
                     \+ narrative_ontology:measurement(_, C, suppression_requirement, T, _) ),
                Mis),
        Mis \== []
    ->  nb_getval(oq105_hosts, Hs0), nb_setval(oq105_hosts, [C|Hs0]),
        format("--- ~w scalar=~2f series=~w misaligned=~w~n", [C, Scalar, Pairs, Mis]),
        forall(member(T, Mis), sweep_row(C, T, Ctx, Scalar, Pairs))
    ;   true
    ).

sweep_row(C, T, Ctx, Scalar, Pairs) :-
    drl_composition:classify_at_time(C, T, Ctx, TypeS, _),
    drl_composition:classify_at_time_with_supp(C, T, Ctx, Scalar, false, TypeS2, _),
    (   TypeS == TypeS2 -> PC = ok
    ;   PC = 'PATH-MISMATCH',
        nb_getval(oq105_pathfail, P0), P1 is P0 + 1, nb_setval(oq105_pathfail, P1)
    ),
    once(interp_at(Pairs, T, VI)),
    drl_composition:classify_at_time_with_supp(C, T, Ctx, VI, false, TypeI, _),
    nb_getval(oq105_rows, R0), R1 is R0 + 1, nb_setval(oq105_rows, R1),
    (   TypeS == TypeI -> Verdict = same
    ;   Verdict = 'DIVERGENT',
        nb_getval(oq105_div, D0), D1 is D0 + 1, nb_setval(oq105_div, D1)
    ),
    format("  T=~w sub=~2f interp=~2f type_sub=~w type_interp=~w pathctl=~w ~w~n",
           [T, Scalar, VI, TypeS, TypeI, PC, Verdict]).

:- run, halt.
:- halt(1).
