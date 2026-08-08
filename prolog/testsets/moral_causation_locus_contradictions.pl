% Axiom contradictions for kernel: moral_causation_locus
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% dispositional_reading↔situational_reading: Dispositional reading holds that stable character traits are the primary causal factor and persist across situations; situational reading holds that circumstances override dispositions and most people lack stable character. No single framework can hold both that character is causally primary and stable AND that circumstances override character and stability is absent.

narrative_ontology:cs_axiom_contradiction(character_cross_situational_stability, situational_primacy_over_disposition).
narrative_ontology:cs_axiom_contradiction(situational_primacy_over_disposition, character_cross_situational_stability).
