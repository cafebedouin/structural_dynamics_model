% Axiom contradictions for kernel: speech_protection_boundary
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.
:- multifile narrative_ontology:cs_contradiction_of/2.

% absolutist_reading↔harm_limited_reading: Absolutist axiom holds that no competing value can override speech protection except imminent violence; harm-limited axiom holds that dignity and equality are co-equal constitutional values that can override speech. No single framework can simultaneously hold both that speech is categorically superior to dignity and that dignity can categorically override speech.

narrative_ontology:cs_axiom_contradiction(speech_protection_categorically_prior_to_harm_regulation, equal_dignity_foundational).
narrative_ontology:cs_axiom_contradiction(equal_dignity_foundational, speech_protection_categorically_prior_to_harm_regulation).
narrative_ontology:cs_story_uid(speech_protection_boundary_contradictions, 'e2a6216a-8d1b-4533-806e-e66a75c380e1').
narrative_ontology:cs_contradiction_of(speech_protection_boundary_contradictions, speech_protection_boundary).
narrative_ontology:cs_created_at('e2a6216a-8d1b-4533-806e-e66a75c380e1', '').
