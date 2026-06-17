% OQ-50 Verification step 4 — delta-exactly-two control (read-only, no engine change).
% Computes the set of mountain-claimers that WOULD pass natural_law_signature (=> get
% restored to mountain at all seats) under each candidate metric RELAXATION, and tests
% whether any relaxation yields EXACTLY {radiative, actinide}.
%
% Relaxations (natural_law currently needs ac>=.85 & su<=.15 & re<=.15 & bc==0 & ha==false & ts==stable;
% ha is UNAUTHORABLE-false corpus-wide, bc!=0 for funded science):
%   R_ha     : drop ha==false              -> pass iff ac&su&re&ts & bc==0
%   R_ha_bc  : drop ha==false AND bc==0     -> pass iff ac&su&re&ts                (the restoration-eligible set)
%   R_scoped : R_ha_bc scoped to eps<=0.05 AND vic==0  (the "pristine physics-ish" scope)
:- initialization(main).
:- [stack].
:- use_module(cache_registry).

mountain(C) :- narrative_ontology:constraint_claim(C, mountain).
eps(C,E) :- ( drl_core:base_extractiveness(C,E0), number(E0) -> E=E0 ; E=1.0 ).
nvic(C,N) :- findall(V, narrative_ontology:constraint_victim(C,V), L), sort(L,Ls), length(Ls,N).
nbc(C,N) :- findall(B, narrative_ontology:agent_beneficiary(C,B), L), sort(L,Ls), length(Ls,N).

fld(ac,AC) :- config:param(natural_law_collapse_min,M), number(AC), AC>=M.
fld(su,Su) :- config:param(natural_law_suppression_max,M), number(Su), Su=<M.
fld(re,Re) :- config:param(natural_law_resistance_max,M), number(Re), Re=<M.
fld(ts,TS) :- TS==stable.
metric_nl(C) :-
    signature_detection:get_constraint_profile(C, profile(AC,Su,Re,_,_,TS,_)),
    fld(ac,AC), fld(su,Su), fld(re,Re), fld(ts,TS).

physics2([actinide_replenishment_mechanism_flat_control, radiative_levitation_stratification]).

report(Tag, Set) :-
    sort(Set, S), length(S, N),
    physics2(P2x), sort(P2x, P2),
    subtract(S, P2, Extra), sort(Extra, ExtraS),
    ( S == P2 -> Verdict = 'EXACTLY {radiative,actinide}  <-- delta-exactly-two HOLDS'
    ; Verdict = 'OVER-RESTORES (delta-exactly-two FAILS)' ),
    format("  ~w  n=~w  ~w~n", [Tag, N, Verdict]),
    ( ExtraS \= [] -> format("      extra (non-physics restored): ~w~n", [ExtraS]) ; true ).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets, cache_registry:clear_all_caches,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    findall(C, (member(C,Cs), mountain(C)), Ms0), sort(Ms0, Ms),
    format("~n==== ~w : delta-exactly-two control over candidate relaxations ====~n", [Dir]),
    % R_ha : metric_nl & bc==0
    findall(C, (member(C,Ms), metric_nl(C), nbc(C,B), B==0), RHa),
    report('R_ha   (drop ha)       ', RHa),
    % R_ha_bc : metric_nl
    findall(C, (member(C,Ms), metric_nl(C)), RHaBc),
    report('R_ha_bc(drop ha & bc)  ', RHaBc),
    % R_scoped : metric_nl & eps<=0.05 & vic==0
    findall(C, (member(C,Ms), metric_nl(C), eps(C,E), E=<0.05, nvic(C,V), V==0), RSc),
    report('R_scoped(+eps<=.05,vic0)', RSc),
    halt.
main :- format("DELTA CONTROL PROBE FAILED~n"), halt(1).
