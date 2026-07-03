% OQ-138 false_natural_law census (READ pass; no engine writes). Run from prolog/:
%   CORPUS_DIR=testsets swipl ../audits/2026-07-02_oq138_fnl_evidence/fnl_census.pl
% For every FNL cascade-winner seat (UNBOUND constraint_signature — true cascade winner),
% reports metric_based_type_indexed -> dr_type (the override effect), claimed_natural source,
% boltzmann, discriminants (vic/ben/eps/supp), verdict. Plus per-corpus positive controls.
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).

nvic(C, N)  :- findall(V, narrative_ontology:constraint_victim(C, V), L), sort(L,Ls), length(Ls,N).
nben(C, N)  :- findall(B, narrative_ontology:agent_beneficiary(C, B), L), sort(L,Ls), length(Ls,N).
eps(C,E)    :- ( catch(drl_core:base_extractiveness(C,E0),_,fail), number(E0) -> E=E0 ; E=na ).
supp(C,S)   :- ( catch(drl_core:get_raw_suppression(C,S0),_,fail), number(S0) -> S=S0 ; S=na ).
mtype(C,T)  :- constraint_indexing:default_context(Ctx), ( catch(drl_core:metric_based_type_indexed(C,Ctx,T0),_,fail) -> T=T0 ; T=err ).
dtype(C,T)  :- constraint_indexing:default_context(Ctx), ( catch(drl_core:dr_type(C,Ctx,T0),_,fail) -> T=T0 ; T=err ).
fnl_src(C,Src) :- ( signature_detection:claimed_natural(C, explicit_mountain_claim) -> Src=explicit_mountain_claim
                  ; signature_detection:claimed_natural(C, natural_law_signature_match) -> Src=nl_signature_match
                  ; Src=none ).
boltz(C,B) :- ( catch(boltzmann_compliance:boltzmann_compliant(C,B0),_,fail) -> B=B0 ; B=err ).
grade(C,G)  :- ( signature_detection:signature_grade(C,G0) -> G=G0 ; G=none ).
verd(C, Base-Joined-SG) :-
    ( catch((diagnostic_summary:diagnostic_summary(C,Sum),
             diagnostic_summary:verdict_join(C,Sum,verdict_join(Joined,Base,_,_,_,_,SG))),_,fail)
      -> true ; Base=err,Joined=err,SG=err ).

% piton candidate (OQ-90 carve-out — the diff-probe planted control target)
is_piton(C) :- catch(narrative_ontology:piton_candidate(C),_,fail).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)), assertz(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0,Cs), length(Cs,NC),
    format("~n================ CORPUS=~w  corpus_constraint=~w ================~n",[Dir,NC]),

    % ---- FNL cascade-winner count (UNBOUND) ----
    findall(C,(member(C,Cs), signature_detection:constraint_signature(C,false_natural_law)),FNLs0),
    sort(FNLs0,FNLs), length(FNLs,NFNL),
    format("cascade-winner false_natural_law: ~w~n",[NFNL]),

    % ---- CENSUS POSITIVE CONTROL: on testsets, organization_floor_c0 must be an FNL winner ----
    ( Dir == testsets
    -> ( memberchk(organization_floor_c0, FNLs)
         -> format("PC_CENSUS ok  organization_floor_c0 present among FNL winners~n")
         ;  format("PC_CENSUS FAIL  organization_floor_c0 NOT an FNL winner — census broken, HALT~n"), halt(3) )
    ; true ),

    % ---- DIFF-PROBE POSITIVE CONTROL: the 3 unconverted piton (scaffold->piton) seats CHANGE ----
    findall(C-MT-DT,(member(C,Cs), is_piton(C), mtype(C,MT), dtype(C,DT), MT\==DT),PitChangers),
    length(PitChangers,NPit),
    format("PC_DIFF piton-candidate type-CHANGERS (planted known live changers): ~w~n",[NPit]),
    forall(member(C-MT-DT,PitChangers), format("    ~w : ~w->~w~n",[C,MT,DT])),
    ( Dir == testsets, NPit =:= 0
    -> format("PC_DIFF WARN  0 piton changers on testsets — diff probe may be blind~n") ; true ),

    % ---- FNL per-seat table ----
    format("~nseat | metric->dr_type (CHANGED?) | fnl_src | boltz | vic | ben | eps | supp | base-joined-grade~n"),
    ( NFNL =:= 0
    -> format("  (no FNL cascade-winners on this leg)~n")
    ;  forall(member(C,FNLs),(
        mtype(C,MT), dtype(C,DT), fnl_src(C,Src), boltz(C,BZ),
        nvic(C,V), nben(C,B), eps(C,E), supp(C,S), verd(C,Vd),
        ( MT==DT -> Ch='TYPE-INERT' ; Ch='CHANGED' ),
        format("  ~w | ~w->~w (~w) | ~w | ~w | vic=~w ben=~w | eps=~w supp=~w | ~w~n",
               [C,MT,DT,Ch,Src,BZ,V,B,E,S,Vd]) )) ),
    halt.
main :- write('CENSUS FAILED'), nl, halt(1).
