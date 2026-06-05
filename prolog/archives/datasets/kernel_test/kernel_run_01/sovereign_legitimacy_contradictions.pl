% Axiom contradictions for kernel: sovereign_legitimacy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% monarchical_reading↔republican_reading: No single coherent framework can hold both 'authority derives from God/inheritance' and 'authority derives from popular consent' as simultaneously true foundations. Accepting divine right as the true ground of legitimacy requires rejecting popular sovereignty as the true ground, and vice versa.

narrative_ontology:cs_axiom_contradiction(inheritance_legitimacy_foundational, popular_consent_necessary_for_legitimacy).
narrative_ontology:cs_axiom_contradiction(popular_consent_necessary_for_legitimacy, inheritance_legitimacy_foundational).
narrative_ontology:cs_story_uid(sovereign_legitimacy_contradictions, '046ee596-af4e-4563-85dc-6dfac4468cfa').
narrative_ontology:cs_created_at('046ee596-af4e-4563-85dc-6dfac4468cfa', '').
