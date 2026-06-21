% Dump diagnostic internals for the 3 FSM seats. Run from prolog/:
%   CORPUS_DIR=testsets swipl -q -g true -t halt ../audits/.../internals_probe.pl
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).

seat(actinide_replenishment_mechanism_flat_control).
seat(protein_anabolic_resistance).
seat(radiative_levitation_stratification).

dump(C) :-
    constraint_indexing:default_context(Ctx),
    ( drl_core:dr_type(C, Ctx, DT) -> true ; DT=err ),
    ( drl_core:metric_based_type_indexed(C, Ctx, MT) -> true ; MT=err ),
    ( catch(maxent_classifier:maxent_top_type(C, Ctx, MX),_,fail) -> true ; MX=err ),
    ( catch(maxent_classifier:maxent_disagreement(C, Ctx, MD),_,fail) -> true ; MD=err ),
    format("~n### ~w  dr_type=~w metric=~w maxent_top=~w maxent_disagree=~w~n",[C,DT,MT,MX,MD]),
    diagnostic_summary:diagnostic_summary(C, Sum),
    Sum = diagnostic_summary(Verdict, Agreements, ExpConflicts, Rejections, Tensions, _, _),
    length(Agreements,NA), length(ExpConflicts,NE), length(Tensions,NT),
    format("  Base verdict=~w | #agree=~w #expconflict=~w #tension=~w~n",[Verdict,NA,NE,NT]),
    format("  ExpConflicts: ~w~n",[ExpConflicts]),
    format("  Tensions:     ~w~n",[Tensions]),
    diagnostic_summary:verdict_join(C, Sum, VJ),
    VJ = verdict_join(J,B,Cap,Alerts,_,_,SG),
    format("  verdict_join: Joined=~w Base=~w Cap=~w SigGrade=~w~n  Alerts=~w~n",[J,B,Cap,SG,Alerts]).

main :-
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, testsets)),
    corpus_loader:load_all_testsets,
    forall(seat(C), dump(C)),
    halt.
main :- format("INTERNALS FAILED~n"), halt(1).
