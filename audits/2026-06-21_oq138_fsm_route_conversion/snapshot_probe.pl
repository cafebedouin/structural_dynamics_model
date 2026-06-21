% OQ-138 full-corpus per-seat snapshot. Run from prolog/:
%   CORPUS_DIR=testsets swipl -q -g true -t halt ../audits/.../snapshot_probe.pl > BASELINE.txt
% Emits one stable line per seat: "<id> | dr_type=<T> | <Joined>-<Base>-<SigGrade>"
% Sorted by id so pre/post diffs are line-stable. Non-FSM seats must be byte-identical
% across the FSM conversion; FSM seats are the only permitted movers.
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).

dtype(C, T) :- constraint_indexing:default_context(Ctx),
    ( catch(drl_core:dr_type(C, Ctx, T0),_,fail) -> T=T0 ; T=err ).
headline(C, Joined-Base-SG) :-
    ( catch(( diagnostic_summary:diagnostic_summary(C, Sum),
              diagnostic_summary:verdict_join(C, Sum, verdict_join(Joined,Base,_,_,_,_,SG)) ),
            _, fail) -> true ; Joined=err, Base=err, SG=err ).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    forall(member(C,Cs),
        ( dtype(C,DT), headline(C, J-B-SG),
          format("~w | dr_type=~w | ~w-~w-~w~n", [C,DT,J,B,SG]) )),
    halt.
main :- format("SNAPSHOT FAILED~n"), halt(1).
