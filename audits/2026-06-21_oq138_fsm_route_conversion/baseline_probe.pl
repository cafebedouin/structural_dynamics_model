% OQ-138 baseline probe. Run from prolog/:
%   CORPUS_DIR=testsets  swipl -q -g true -t halt ../audits/2026-06-21_oq138_fsm_route_conversion/baseline_probe.pl
%   CORPUS_DIR=testsets_flash swipl -q -g true -t halt ../audits/.../baseline_probe.pl
% Reports, per FSM-firing seat: cascade signatures (determinism witness),
% metric_type vs dr_type, victim count, signature_grade/severity, verdict_join headline.
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).

nvic(C, N) :- findall(V, narrative_ontology:constraint_victim(C, V), L), sort(L,Ls), length(Ls,N).
sigs(C, Ss) :- findall(S, signature_detection:constraint_signature(C, S), Ss).
mtype(C, T) :- constraint_indexing:default_context(Ctx),
    ( catch(drl_core:metric_based_type_indexed(C, Ctx, T0),_,fail) -> T=T0 ; T=err ).
dtype(C, T) :- constraint_indexing:default_context(Ctx),
    ( catch(drl_core:dr_type(C, Ctx, T0),_,fail) -> T=T0 ; T=err ).
grade(C, G) :- ( signature_detection:signature_grade(C, G0) -> G=G0 ; G=none ).
sev(C, S)   :- ( signature_detection:signature_severity(C, S0) -> S=S0 ; S=none ).
headline(C, Joined-Base-SG) :-
    ( catch(( diagnostic_summary:diagnostic_summary(C, Sum),
              diagnostic_summary:verdict_join(C, Sum, verdict_join(Joined,Base,_,_,_,_,SG)) ),
            _, fail) -> true ; Joined=err, Base=err, SG=err ).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs), length(Cs, NC),
    format("~n=== CORPUS=~w  corpus_constraint=~w ===~n", [Dir, NC]),
    findall(C, (member(C,Cs), signature_detection:false_summit_mountain(C,_)), Fs),
    length(Fs, NF),
    format("FSM-detector fires on ~w seats~n", [NF]),
    % determinism witness: every cascade returns exactly one signature
    findall(C-L, (member(C,Fs), sigs(C,Ss), length(Ss,L), L =\= 1), MultiSig),
    length(MultiSig, NMulti),
    format("seats where constraint_signature/2 yields !=1 signature: ~w  ~w~n", [NMulti, MultiSig]),
    % per-seat baseline
    format("~nseat | cascade-sig | metric_type -> dr_type | vic | grade/sev | headline(Joined/Base/SigGrade)~n"),
    forall(member(C,Fs),
        ( sigs(C,Ss), mtype(C,MT), dtype(C,DT), nvic(C,V),
          grade(C,G), sev(C,Sv), headline(C, H),
          format("  ~w | ~w | ~w -> ~w | vic=~w | ~w/~w | ~w~n", [C,Ss,MT,DT,V,G,Sv,H]) )),
    % how many FSM-cascade seats are vic=0 vs vic>0
    findall(C, (member(C,Fs), sigs(C,[false_summit_mountain]), nvic(C,V), V=:=0), V0),
    findall(C, (member(C,Fs), sigs(C,[false_summit_mountain]), nvic(C,V), V>0),  V1),
    length(V0,NV0), length(V1,NV1),
    format("~nFSM-cascade-winner seats: vic=0 -> ~w, vic>0 -> ~w~n", [NV0, NV1]),
    halt.
main :- format("PROBE FAILED~n"), halt(1).
