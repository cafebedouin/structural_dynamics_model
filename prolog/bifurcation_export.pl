:- module(bifurcation_export, [
    export_all_classifications/0,
    export_product_classifications/0
]).

%% export_all_classifications/0
%  Prints one line per (constraint, context) classification:
%    CLASSIFY:<ConstraintID>:<PowerAtom>:<Type>
%  PowerAtom is the agent_power value from the standard context,
%  serving as a compact label (powerless, moderate, institutional, analytical).
%  Output is sorted for stable diffing between baseline and perturbed runs.

export_all_classifications :-
    corpus_loader:ensure_corpus_loaded,
    findall(
        classify(C, Power, Type),
        (   logical_fingerprint:known_constraint(C),
            drl_core:standard_context(Ctx),
            Ctx = context(agent_power(Power), _, _, _),
            drl_core:dr_type(C, Ctx, Type)
        ),
        Raw
    ),
    sort(Raw, Sorted),
    forall(
        member(classify(C, Power, Type), Sorted),
        format("CLASSIFY:~w:~w:~w~n", [C, Power, Type])
    ).

%% export_product_classifications/0
%  Prints one line per (constraint, context) classification across the full
%  product site (156 contexts). Site mode must be set to 'product' before calling.
%  Uses per-constraint streaming (not a giant findall) for efficiency.
%  Output format:
%    CLASSIFY:<ConstraintID>:<power>_<time>_<exit>_<scope>:<Type>
%  Output is sorted per constraint; constraints are emitted in corpus order.

export_product_classifications :-
    corpus_loader:ensure_corpus_loaded,
    constraint_indexing:site_contexts(Contexts),
    % Build context key list once
    maplist(product_context_key, Contexts, CtxKeys),
    pairs_keys_values(Pairs, Contexts, CtxKeys),
    % Get all constraint IDs
    findall(C, logical_fingerprint:known_constraint(C), Cs),
    sort(Cs, Sorted),
    % Process per constraint, streaming output
    maplist(export_constraint_product(Pairs), Sorted).

%% export_constraint_product(+CtxPairs, +C)
%  Prints all 156 CLASSIFY lines for one constraint.
export_constraint_product(Pairs, C) :-
    forall(
        member(Ctx-CtxKey, Pairs),
        (   (drl_core:dr_type(C, Ctx, Type) -> true ; Type = unknown),
            format("CLASSIFY:~w:~w:~w~n", [C, CtxKey, Type])
        )
    ).

%% product_context_key(+ContextTerm, -Key)
%  Converts context(...) to atom "P_T_E_S".
product_context_key(
    context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S)),
    Key
) :-
    atomic_list_concat([P, T, E, S], '_', Key).
