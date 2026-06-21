% OQ-138 FSM verdict-decomposition sweep on the BUILT code. Run from prolog/:
%   CORPUS_DIR=testsets_flash swipl -q -g true -t halt ../audits/.../fsm_verdict_sweep.pl
% For every FSM-DETECTED seat (false_summit_mountain fires): post-revert dr_type,
% vic count, which tensions surface, Base + Joined verdict, SigGrade, sig severity.
% asserta overlay (NOT assertz). corpus_constraint count = overlay-took-effect witness.
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).

nvic(C, N) :- findall(V, narrative_ontology:constraint_victim(C, V), L), sort(L,Ls), length(Ls,N).
sev(C, S)   :- ( signature_detection:signature_severity(C, S0) -> S=S0 ; S=none ).

decomp(C, row(C,DT,V,Base,Joined,SG,Sev,TensKinds)) :-
    constraint_indexing:default_context(Ctx),
    ( drl_core:dr_type(C, Ctx, DT) -> true ; DT=err ),
    nvic(C, V), sev(C, Sev),
    ( catch(diagnostic_summary:diagnostic_summary(C, Sum),_,fail)
    -> Sum = diagnostic_summary(Base, _, _, _, Tensions, _, _),
       findall(K, (member(tension(Subsys,Detail),Tensions), functor(Detail,F,_), K=Subsys/F), TensKinds0),
       sort(TensKinds0, TensKinds),
       ( catch(diagnostic_summary:verdict_join(C,Sum,verdict_join(Joined,_,_,_,_,_,SG)),_,fail) -> true ; Joined=err, SG=err )
    ;  Base=err, Joined=err, SG=err, TensKinds=[] ).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs), length(Cs, NC),
    format("~n=== CORPUS=~w  corpus_constraint=~w ===~n", [Dir, NC]),
    findall(C, (member(C,Cs), signature_detection:false_summit_mountain(C,_),
                signature_detection:constraint_signature(C, false_summit_mountain)), Fs0),
    sort(Fs0, Fs), length(Fs, NF),
    format("FSM-cascade-winner seats: ~w~n", [NF]),
    format("seat | dr_type | vic | Base | Joined | SigGrade | sigSev | tensionKinds~n"),
    findall(Row, (member(C,Fs), decomp(C,Row)), Rows),
    forall(member(row(C,DT,V,Base,J,SG,Sev,TK), Rows),
        format("  ~w | ~w | vic=~w | ~w | ~w | ~w | ~w | ~w~n",[C,DT,V,Base,J,SG,Sev,TK])),
    % aggregate: of vic=0 seats, how many Joined=green vs yellow; same for vic>0
    aggcount(Rows, 0, green, AG0g), aggcount(Rows, 0, yellow, AG0y), aggcount(Rows, 0, red, AG0r),
    aggcountpos(Rows, green, AGpg), aggcountpos(Rows, yellow, AGpy), aggcountpos(Rows, red, AGpr),
    format("~nAGG vic=0:  Joined green=~w yellow=~w red=~w~n",[AG0g,AG0y,AG0r]),
    format("AGG vic>0:  Joined green=~w yellow=~w red=~w~n",[AGpg,AGpy,AGpr]),
    % how many FSM seats have NO dirac/cohomology tension (discriminant visible)
    findall(C, (member(row(C,_,_,_,_,_,_,TK),Rows), \+ member(dirac/_,TK), \+ member(cohomology/_,TK)), Clean),
    length(Clean, NClean),
    format("FSM seats with NO dirac/cohomology tension (discriminant would be headline-visible): ~w~n",[NClean]),
    halt.
aggcount(Rows, Vic, Verd, N) :- findall(x, (member(row(_,_,V,_,J,_,_,_),Rows), V=:=Vic, J==Verd), L), length(L,N).
aggcountpos(Rows, Verd, N) :- findall(x, (member(row(_,_,V,_,J,_,_,_),Rows), V>0, J==Verd), L), length(L,N).
main :- format("SWEEP FAILED~n"), halt(1).
