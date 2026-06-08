% Axiom contradictions for kernel: catastrophe_memory_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% symbol_continuity_reading↔survival_competence_reading: Symbol-continuity reading treats operational survival-yield as epiphenomenal (ritual persists because it is tradition, not because it confers adaptive advantage). Survival-competence reading treats symbolic content as instrumental (ritual persists because it encodes survival-relevant information). No single framework holds both: either tradition-continuity is the terminal value or survival-competence is.
% trauma_encoding_reading↔symbol_continuity_reading: Trauma-encoding reading treats ritual as mechanism imposing psychological costs on descendants for collective threat-detection (extractive). Symbol-continuity reading treats ritual as cost-free identity-marker (non-extractive symbolic transmission). No single framework holds both: either the ritual extracts from future generations or it does not.

narrative_ontology:cs_axiom_contradiction(symbol_transmission_preserves_identity, ritual_encodes_practical_competence).
narrative_ontology:cs_axiom_contradiction(ritual_encodes_practical_competence, symbol_transmission_preserves_identity).
narrative_ontology:cs_axiom_contradiction(trauma_is_constitutive_intergenerational_gift, symbol_transmission_preserves_identity).
narrative_ontology:cs_axiom_contradiction(symbol_transmission_preserves_identity, trauma_is_constitutive_intergenerational_gift).
