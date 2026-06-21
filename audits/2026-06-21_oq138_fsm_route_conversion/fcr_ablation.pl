% FCR ablation via the config hook (READ pass). Run from prolog/:
%   FCR=1 swipl -q -g true -t halt ../audits/.../fcr_ablation.pl   (default override ON)
%   FCR=0 swipl -q -g true -t halt ../audits/.../fcr_ablation.pl   (ablation: preserve metric type = the ROUTED preview)
% Param set BEFORE corpus load (fresh process => no stale memo cache). Dumps every
% false_ci_rope cascade-winner seat: dr_type + base/joined/grade verdict + vic.
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).

nvic(C,N) :- findall(V, narrative_ontology:constraint_victim(C,V), L), sort(L,Ls), length(Ls,N).
dtype(C,T):- constraint_indexing:default_context(Ctx), ( catch(drl_core:dr_type(C,Ctx,T0),_,fail)->T=T0;T=err ).
verd(C,Base-Joined-SG) :-
    ( catch((diagnostic_summary:diagnostic_summary(C,Sum),
             diagnostic_summary:verdict_join(C,Sum,verdict_join(Joined,Base,_,_,_,_,SG))),_,fail)
      -> true ; Base=err,Joined=err,SG=err ).
main :-
    getenv('FCR', V), atom_number(V, FV),
    retractall(config:param(fcr_override_enabled,_)), asserta(config:param(fcr_override_enabled, FV)),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, testsets)),
    corpus_loader:load_all_testsets,
    ( config:param(fcr_override_enabled, Cur) -> true ; Cur='?' ),
    format("~n=== fcr_override_enabled=~w (witness: ~w) ===~n",[FV,Cur]),
    findall(C, (corpus_loader:corpus_constraint(C), signature_detection:constraint_signature(C, false_ci_rope)), Fs0),
    sort(Fs0,Fs),
    forall(member(C,Fs),( dtype(C,DT), nvic(C,Vv), verd(C,Vd),
        format("  ~w | dr_type=~w | vic=~w | ~w~n",[C,DT,Vv,Vd]) )),
    halt.
main :- write('FCR ABLATION FAILED'), halt(1).
