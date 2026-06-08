% Axiom contradictions for kernel: orthographic_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% continuity_reading↔rupture_reading: Continuity reading holds that orthographic stability is necessary for cultural transmission; rupture reading holds that orthographic discontinuity is necessary for cultural transformation. No coherent framework can simultaneously require stability and discontinuity of the same substrate.
% continuity_reading↔modernization_reading: Continuity reading grounds legitimacy in historical precedent and religious tradition; modernization reading grounds legitimacy in technological efficiency and European alignment. These are mutually exclusive sources of orthographic authority.

narrative_ontology:cs_axiom_contradiction(living_transmission_preserves_legitimacy, cultural_rupture_prerequisite_for_modernization).
narrative_ontology:cs_axiom_contradiction(cultural_rupture_prerequisite_for_modernization, living_transmission_preserves_legitimacy).
narrative_ontology:cs_axiom_contradiction(living_transmission_preserves_legitimacy, script_determines_modernization_capacity).
narrative_ontology:cs_axiom_contradiction(script_determines_modernization_capacity, living_transmission_preserves_legitimacy).
