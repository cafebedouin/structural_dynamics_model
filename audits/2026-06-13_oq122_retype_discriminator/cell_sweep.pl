% OQ-122 cell sweep: enumerate {claim=mountain, eps<=0.25, agent_beneficiary != []}
% across the whole corpus, report what fires false_summit_mountain, with the
% metric vector needed to hand-label false-positive vs genuine-concealment.
:- initialization(main).
:- [stack].

mval(P, C, V) :- ( call(P, C, V) -> true ; V = na ).

emn(C, Y) :- ( domain_priors:emerges_naturally(C) -> Y = yes ; Y = no ).

fsm(C, F) :- ( signature_detection:false_summit_mountain(C, _) -> F = 'FSM-FIRES' ; F = '.' ).

benefs(C, N, Bs) :-
    findall(B, narrative_ontology:agent_beneficiary(C, B), Bs0),
    sort(Bs0, Bs), length(Bs, N).

in_cell(C) :-
    narrative_ontology:constraint_claim(C, mountain),
    domain_priors:base_extractiveness(C, E), number(E), E =< 0.25,
    narrative_ontology:agent_beneficiary(C, _).

main :-
    corpus_loader:ensure_corpus_loaded,
    format("~n=== CELL: claim=mountain & base_extractiveness<=0.25 & agent_beneficiary!=[] ===~n"),
    format("~w~t~40|  eps  supp  thtr  accC  resist  emrg  ~w  nB beneficiaries~n", ['constraint', 'fsm']),
    findall(C, in_cell(C), Cs0), sort(Cs0, Cs),
    ( Cs == [] -> format("  (cell empty)~n") ; true ),
    forall(member(C, Cs),
      ( mval(domain_priors:base_extractiveness, C, E),
        ( domain_priors:suppression_score(C, Su) -> true ; Su = na ),
        ( domain_priors:theater_ratio(C, Th) -> true ; Th = na ),
        ( narrative_ontology:constraint_metric(C, accessibility_collapse, AC) -> true ; AC = na ),
        ( narrative_ontology:constraint_metric(C, resistance, Rs) -> true ; Rs = na ),
        emn(C, Em), fsm(C, F), benefs(C, NB, Bs),
        format("~w~t~40|  ~w  ~w  ~w  ~w  ~w  ~w  ~w  ~w  ~w~n",
               [C, E, Su, Th, AC, Rs, Em, F, NB, Bs]) )),
    length(Cs, NCell), format("~n  cell size = ~w~n", [NCell]),

    % Positive control: confirm the sweep can SEE a non-member (a mountain-claimer
    % with high eps, or a non-mountain claim) — so 'cell membership' isn't vacuous.
    format("~n=== positive control: mountain-claimers OUTSIDE the cell (eps>0.25 OR no agent benef) ===~n"),
    findall(C2-E2-Why,
      ( narrative_ontology:constraint_claim(C2, mountain),
        \+ in_cell(C2),
        ( domain_priors:base_extractiveness(C2, E2) -> true ; E2 = na ),
        ( (number(E2), E2 > 0.25) -> Why = 'eps>0.25'
        ; \+ narrative_ontology:agent_beneficiary(C2, _) -> Why = 'no_agent_benef'
        ; Why = 'other' ) ),
      Outs0),
    sort(Outs0, Outs),
    ( Outs == [] -> format("  (none — every mountain-claimer is in the cell)~n")
    ; forall(member(C2-E2-Why, Outs), format("  ~w  eps=~w  (~w)~n", [C2, E2, Why])) ),
    halt.
main :- format("SWEEP FAILED~n"), halt(1).
