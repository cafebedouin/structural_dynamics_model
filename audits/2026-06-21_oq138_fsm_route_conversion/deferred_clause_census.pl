% OQ-138 deferred-clause evidence census (READ pass; no engine writes). Run from prolog/:
%   CORPUS_DIR=testsets        swipl -q -g true -t halt ../audits/.../deferred_clause_census.pl
%   CORPUS_DIR=testsets_flash  swipl -q -g true -t halt ../audits/.../deferred_clause_census.pl
% For every cascade-winner seat whose signature is an OVERRIDE clause still un-converted
% (FCR / CI-rope / constructed_* / coordination_scaffold), reports the per-seat
% metric_based_type_indexed -> dr_type DIFF (the override effect, computable without an
% ablation hook) + candidate discriminants (vic, agent_beneficiary, eps, supp) + verdict.
:- initialization(main).
:- [stack].
:- use_module(diagnostic_summary, []).

override_sig(false_ci_rope).
override_sig(coupling_invariant_rope).
override_sig(constructed_low_extraction).
override_sig(constructed_high_extraction).
override_sig(constructed_constraint).
override_sig(coordination_scaffold).

nvic(C, N)  :- findall(V, narrative_ontology:constraint_victim(C, V), L), sort(L,Ls), length(Ls,N).
nben(C, N)  :- findall(B, narrative_ontology:agent_beneficiary(C, B), L), sort(L,Ls), length(Ls,N).
eps(C,E)    :- ( catch(drl_core:base_extractiveness(C,E0),_,fail), number(E0) -> E=E0 ; E=na ).
supp(C,S)   :- ( catch(drl_core:get_raw_suppression(C,S0),_,fail), number(S0) -> S=S0 ; S=na ).
mtype(C,T)  :- constraint_indexing:default_context(Ctx), ( catch(drl_core:metric_based_type_indexed(C,Ctx,T0),_,fail) -> T=T0 ; T=err ).
dtype(C,T)  :- constraint_indexing:default_context(Ctx), ( catch(drl_core:dr_type(C,Ctx,T0),_,fail) -> T=T0 ; T=err ).
grade(C,G)  :- ( signature_detection:signature_grade(C,G0) -> G=G0 ; G=none ).
verd(C, Base-Joined-SG) :-
    ( catch((diagnostic_summary:diagnostic_summary(C,Sum),
             diagnostic_summary:verdict_join(C,Sum,verdict_join(Joined,Base,_,_,_,_,SG))),_,fail)
      -> true ; Base=err,Joined=err,SG=err ).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0,Cs), length(Cs,NC),
    format("~n=== CORPUS=~w  corpus_constraint=~w ===~n",[Dir,NC]),
    % per-signature cascade-winner counts
    forall(override_sig(Sig),(
        findall(C,(member(C,Cs), signature_detection:constraint_signature(C,Sig)),Ws),
        length(Ws,N), format("  cascade-winner ~w: ~w~n",[Sig,N]) )),
    format("~nseat | signature | metric->dr_type (CHANGED?) | vic | ben | eps | supp | base-joined-grade~n"),
    forall((member(C,Cs), signature_detection:constraint_signature(C,Sig), override_sig(Sig)),(
        mtype(C,MT), dtype(C,DT), nvic(C,V), nben(C,B), eps(C,E), supp(C,S), verd(C,Vd),
        ( MT==DT -> Ch='same' ; Ch='CHANGED' ),
        format("  ~w | ~w | ~w->~w (~w) | vic=~w | ben=~w | eps=~w | supp=~w | ~w~n",
               [C,Sig,MT,DT,Ch,V,B,E,S,Vd]) )),
    halt.
main :- write('CENSUS FAILED'), halt(1).
