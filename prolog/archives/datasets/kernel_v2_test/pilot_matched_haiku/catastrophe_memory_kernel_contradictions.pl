% Axiom contradictions for kernel: catastrophe_memory_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% symbol_continuity_reading↔survival_competence_reading: Symbol-continuity reading treats operational survival-yield as epiphenomenal (ritual persists because it is tradition, not because it confers adaptive advantage). Survival-competence reading treats symbolic content as instrumental (ritual persists because it encodes survival-relevant information). No single framework holds both: either tradition-continuity is the terminal value or survival-competence is.

narrative_ontology:cs_axiom_contradiction(symbolic_continuity_primary_function, ritual_transmits_survival_competence).
narrative_ontology:cs_axiom_contradiction(ritual_transmits_survival_competence, symbolic_continuity_primary_function).
