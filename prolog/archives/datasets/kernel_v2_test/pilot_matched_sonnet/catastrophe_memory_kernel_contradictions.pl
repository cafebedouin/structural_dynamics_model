% Axiom contradictions for kernel: catastrophe_memory_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% trauma_encoding_reading↔symbol_continuity_reading: Trauma-encoding reading treats ritual as mechanism imposing psychological costs on descendants for collective threat-detection (extractive). Symbol-continuity reading treats ritual as cost-free identity-marker (non-extractive symbolic transmission). No single framework holds both: either the ritual extracts from future generations or it does not.

narrative_ontology:cs_axiom_contradiction(trauma_transmission_preserves_vigilance, symbolic_continuity_primacy).
narrative_ontology:cs_axiom_contradiction(symbolic_continuity_primacy, trauma_transmission_preserves_vigilance).
