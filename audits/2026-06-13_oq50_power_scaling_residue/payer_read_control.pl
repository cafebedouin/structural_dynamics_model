% OQ-50/OQ-126 — payer-read delta-exactly-two control (read-only, no engine change).
% The MISSING half: delta_control_probe.pl gave the HA/bc *metric* relaxations a
% delta-exactly-two control (it REFUTED disposition A). The payer-read leg (B'),
% the thing OQ-126 would actually rule in (swap natural_law's BeneficiaryCount==0
% for \+ constraint_victim(C,_)), never got the analog. The joint_witness GREEN
% cell FAKED the bc leg by RETRACTING constraint_beneficiary on two hand-picked
% cases -> it witnessed "zero bc on {radiative,actinide} => GREEN", NOT "read the
% payer => these two and nothing else." This probe supplies that control.
%
% natural_law gate = ac>=M & su<=M & re<=M & ts==stable & ha==false & STAKEHOLDER==0.
% Current STAKEHOLDER condition: bc==0 (agent_beneficiary count). Payer-read: vic==0.
% ha is unauthorable-false corpus-wide, so BOTH legs are 0-pass until the HA leg is
% active; this probe measures the UNGATED stakeholder set (drop ha) to characterise
% what each stakeholder-condition admits ON ITS OWN MERITS -- i.e. the blast radius
% the HA leg would have to contain.
%
% PRE-REGISTERED (written before the run):
%   R_payer (metric_nl & vic==0) == {radiative,actinide}  => victim-read isolates physics  [strengthens B']
%   R_payer ⊋ {radiative,actinide} incl. social naturalization twins
%       (price_formation__naturalist, money-emergence, zero_as_number, animal_property...)
%                                                       => victim-read LEAKS; isolation rests on HA leg only [weakens B']
% Also reports the symmetric difference vs R_ha (bc==0) so the swap's exact movement is visible.
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
    ( S == P2 -> Verdict = 'EXACTLY {radiative,actinide}  <-- isolates physics'
    ; subtract(P2, S, Miss), Miss \= [] -> Verdict = 'MISSES a physics case'
    ; Verdict = 'OVER-RESTORES beyond physics' ),
    format("  ~w  n=~w  ~w~n", [Tag, N, Verdict]),
    ( ExtraS \= [] -> format("      extra (non-physics admitted): ~w~n", [ExtraS]) ; true ).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets, cache_registry:clear_all_caches,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    findall(C, (member(C,Cs), mountain(C)), Ms0), sort(Ms0, Ms),
    length(Ms, NM),
    format("~n==== ~w : payer-read delta-exactly-two control (mountain-claimers=~w) ====~n", [Dir, NM]),
    % R_ha : current rule, ha dropped  (metric_nl & bc==0)
    findall(C, (member(C,Ms), metric_nl(C), nbc(C,B), B==0), RHa), sort(RHa, SHa),
    report('R_ha    (bc==0, current) ', SHa),
    % R_payer : payer-read, ha dropped (metric_nl & vic==0)
    findall(C, (member(C,Ms), metric_nl(C), nvic(C,V), V==0), RPa), sort(RPa, SPa),
    report('R_payer (vic==0, B-prime)', SPa),
    % symmetric difference: what the swap moves
    subtract(SPa, SHa, NewlyAdmitted), sort(NewlyAdmitted, NA),
    subtract(SHa, SPa, NewlyDropped),  sort(NewlyDropped, ND),
    format("  swap bc==0 -> vic==0 : +newly admitted (vic0 & bc>0) = ~w~n", [NA]),
    format("                         -newly dropped  (bc0 & vic>0) = ~w~n", [ND]),
    halt.
main :- format("PAYER-READ CONTROL PROBE FAILED~n"), halt(1).
