% Axiom contradictions for kernel: orthographic_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% continuity_reading↔rupture_reading: Continuity reading holds that orthographic stability is necessary for cultural transmission; rupture reading holds that orthographic discontinuity is necessary for cultural transformation. No coherent framework can simultaneously require stability and discontinuity of the same substrate.
% continuity_reading↔modernization_reading: Continuity reading grounds legitimacy in historical precedent and religious tradition; modernization reading grounds legitimacy in technological efficiency and European alignment. These are mutually exclusive sources of orthographic authority.

narrative_ontology:cs_axiom_contradiction(unbroken_community_entails_linguistic_continuity, script_change_is_deliberate_rupture).
narrative_ontology:cs_axiom_contradiction(script_change_is_deliberate_rupture, unbroken_community_entails_linguistic_continuity).
narrative_ontology:cs_axiom_contradiction(unbroken_community_entails_linguistic_continuity, script_language_separability).
narrative_ontology:cs_axiom_contradiction(script_language_separability, unbroken_community_entails_linguistic_continuity).
