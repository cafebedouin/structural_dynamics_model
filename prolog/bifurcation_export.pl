:- module(bifurcation_export, [
    export_all_classifications/0
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
