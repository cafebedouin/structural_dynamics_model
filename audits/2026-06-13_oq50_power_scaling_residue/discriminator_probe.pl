% OQ-50 — find a discriminator that separates the physics cases (radiative/actinide)
% from social naturalization mountain-claimers that should STAY rope. Read-only.
% Dumps, for every mountain-claimer that passes ac/su/re/ts (the restoration-eligible
% set if bc/ha were cleared): eps, emerges_naturally, vic count, bc count, signature.
:- initialization(main).
:- [stack].
:- use_module(cache_registry).

mountain(C) :- narrative_ontology:constraint_claim(C, mountain).
eps(C,E) :- ( drl_core:base_extractiveness(C,E0), number(E0) -> E=E0 ; E=na ).
nvic(C,N) :- findall(V, narrative_ontology:constraint_victim(C,V), L), sort(L,Ls), length(Ls,N).
nbc(C,N) :- findall(B, narrative_ontology:agent_beneficiary(C,B), L), sort(L,Ls), length(Ls,N).
emn(C) :- ( drl_core:emerges_naturally(C) -> true ; fail ).
sig(C,S) :- ( signature_detection:constraint_signature(C,S0) -> S=S0 ; S=none ).

fld_pass(ac,AC) :- config:param(natural_law_collapse_min,M), number(AC), AC>=M.
fld_pass(su,Su) :- config:param(natural_law_suppression_max,M), number(Su), Su=<M.
fld_pass(re,Re) :- config:param(natural_law_resistance_max,M), number(Re), Re=<M.
fld_pass(ts,TS) :- TS==stable.
passes_metric_nl(C) :-
    signature_detection:get_constraint_profile(C, profile(AC,Su,Re,_,_,TS,_)),
    fld_pass(ac,AC), fld_pass(su,Su), fld_pass(re,Re), fld_pass(ts,TS).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets, cache_registry:clear_all_caches,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    findall(C, (member(C,Cs), mountain(C), passes_metric_nl(C)), Es0), sort(Es0, Es),
    format("~n==== ~w : restoration-ELIGIBLE mountain-claimers (pass ac/su/re/ts) ====~n", [Dir]),
    format("   sorted by eps; emn=emerges_naturally~n"),
    findall(E-C, (member(C,Es), eps(C,E)), Pairs0), keysort(Pairs0, Pairs),
    forall(member(E-C, Pairs),
      ( nvic(C,V), nbc(C,B), ( emn(C)->EM=yes;EM=no ), sig(C,Sg),
        format("  eps=~w~t~12|vic=~w bc=~w emn=~w~t~38|~w  ~w~n", [E,V,B,EM,Sg,C]) )),
    halt.
main :- format("DISCRIM PROBE FAILED~n"), halt(1).
