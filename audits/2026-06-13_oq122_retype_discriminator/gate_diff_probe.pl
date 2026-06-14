% OQ-122 gate-diff probe. Dumps a stable per-mountain-claimer line on a given corpus.
% Run on baseline (unmodified) and on the victim-gated branch, then diff the two outputs.
%   CORPUS_DIR=testsets_flash swipl -q -g true -t halt gate_diff_probe.pl
:- initialization(main).
:- [stack].

mountain(C) :- narrative_ontology:constraint_claim(C, mountain).
nvic(C,N) :- findall(V, narrative_ontology:constraint_victim(C,V), L), sort(L,Ls), length(Ls,N).
fsmbit(C,B) :- ( signature_detection:false_summit_mountain(C,_) -> B=1 ; B=0 ).
sig(C,S) :- ( signature_detection:constraint_signature(C,S) -> true ; S=none ).
t1(C,Ctx) :- drl_core:dr_claim_mismatch(C,Ctx,type_1_false_summit,_).
t1count(C,N) :- ( setof(Ctx, t1(C,Ctx), L) -> length(L,N) ; N=0 ).
seats(C,Seq) :- findall(T, ( drl_core:standard_context(Ctx),
        ( catch(drl_core:dr_type(C,Ctx,T0),_,fail) -> T=T0 ; T='<f>' ) ), Seq).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    findall(C, (member(C,Cs), mountain(C)), Ms0), sort(Ms0, Ms),
    forall(member(C, Ms),
      ( nvic(C,V), fsmbit(C,F), t1count(C,T), sig(C,S), seats(C,Sq),
        format("~w | vic=~w | fsm=~w | t1=~w | sig=~w | seats=~w~n", [C,V,F,T,S,Sq]) )),
    halt.
main :- format("PROBE FAILED~n"), halt(1).
