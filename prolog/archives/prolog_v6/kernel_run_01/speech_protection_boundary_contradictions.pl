% Axiom contradictions for kernel: speech_protection_boundary
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% near_absolutist_reading↔dignitary_harm_reading: Absolutist axiom holds that content-based harm (offense, dignitary injury) cannot justify restriction because all ideas must compete freely; dignitary harm axiom holds that speech constructing subordination is itself a liberty violation requiring state intervention. No single framework can hold both: accepting dignitary harm as restrictable requires rejecting absolutist immunity for harmful ideas.

narrative_ontology:cs_axiom_contradiction(speech_protection_categorically_prior, dignitary_harm_justifies_restriction).
narrative_ontology:cs_axiom_contradiction(dignitary_harm_justifies_restriction, speech_protection_categorically_prior).
narrative_ontology:cs_story_uid(speech_protection_boundary_contradictions, '9294dc47-9fd4-4752-aefc-4f9fde68bced').
narrative_ontology:cs_created_at('9294dc47-9fd4-4752-aefc-4f9fde68bced', '').
