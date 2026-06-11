% OQ-90 Phase 1 capture-disposition sweep.
% Enumerates the authoritative corpus membership (corpus_loader:corpus_constraint/1)
% and reports, per constraint: gain_flow, fixing_cost_class, signature, capture
% disposition (read from the WIRED fcr_evidence/7 term for FCR-reaching constraints;
% from narrative_ontology predicates directly otherwise), and the analytical-perspective
% dr_type. The per-item trace for ruling 4 ("never read 'piton sparse' without the
% upstream-shadow caveat").
%
% Run from prolog/:  swipl -q -g run_sweep -t "halt(1)" ../audits/2026-06-11_oq90_piton_refinement/capture_disposition_sweep.pl

:- initialization(true).

run_sweep :-
    use_module(stack),
    corpus_loader:ensure_corpus_loaded,
    format('~n=== OQ-90 capture-disposition sweep (~w constraints) ===~n', [_]),
    format('~w~t~40|~w~t~54|~w~t~68|~w~t~92|~w~n',
           ['constraint','gain_flow','fixing_cost','signature','disposition[source]/dr_type']),
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    forall(member(C, Cs), report_one(C)),
    nl,
    summary(Cs),
    halt.

gain_flow_of(C, GF) :- ( narrative_ontology:stakeholder_gain_flow(C, GF) -> true ; GF = '(absent)' ).
fixing_cost_of(C, FC) :- ( narrative_ontology:fixing_cost_class(C, FC) -> true ; FC = '(absent)' ).

signature_of(C, Sig) :-
    ( signature_detection:constraint_signature(C, Sig) -> true ; Sig = '(none)' ).

% Disposition: prefer the wired fcr_evidence/7 term (proves the field is populated
% on the live path); fall back to the predicates directly for non-FCR constraints.
disposition_of(C, Disp-Source) :-
    ( signature_detection:false_ci_rope(C, fcr_evidence(_,_,_,_,_,_,D))
    -> Disp = D, Source = fcr_evidence
    ;  signature_detection:capture_disposition(C, D), Disp = D, Source = predicate
    ).

drtype_analytical(C, T) :-
    ( drl_core:dr_type(C, context(agent_power(analytical), time_horizon(civilizational),
                                  exit_options(analytical), spatial_scope(universal)), T)
    -> true ; T = '(n/a)' ).

report_one(C) :-
    gain_flow_of(C, GF), fixing_cost_of(C, FC), signature_of(C, Sig),
    disposition_of(C, Disp-Src), drtype_analytical(C, T),
    ( GF == diffuse
    -> format('~w~t~40|~w~t~54|~w~t~68|~w~t~92|~w[~w]/~w  <== DIFFUSE~n',
              [C, GF, FC, Sig, Disp, Src, T])
    ;  true ).  % only diffuse rows printed in full; summary covers the rest

summary(Cs) :-
    findall(C, (member(C,Cs), narrative_ontology:stakeholder_gain_flow(C,diffuse)), Diff),
    findall(C, (member(C,Cs), narrative_ontology:piton_candidate(C)), Pit),
    findall(C, (member(C,Cs), narrative_ontology:transient_neglect(C)), TN),
    findall(C, (member(C,Cs), narrative_ontology:constraint_captured(C)), Cap),
    length(Cs,N), length(Diff,ND), length(Pit,NP), length(TN,NT), length(Cap,NC),
    format('--- summary ---~n', []),
    format('corpus N=~w | diffuse=~w | piton_candidate=~w | transient_neglect=~w | captured=~w~n',
           [N, ND, NP, NT, NC]),
    format('diffuse claims: ~q~n', [Diff]),
    format('piton_candidates: ~q~n', [Pit]),
    format('transient_neglect: ~q (corpus-empty expected)~n', [TN]).
