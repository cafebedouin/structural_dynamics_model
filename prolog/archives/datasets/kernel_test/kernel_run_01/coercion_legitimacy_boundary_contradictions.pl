% Axiom contradictions for kernel: coercion_legitimacy_boundary
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% public_health_primary↔bodily_autonomy_primary: Public-health-primary accepts utilitarian override of consent when collective benefit suffices; bodily-autonomy-primary treats consent as non-overridable regardless of collective benefit. No single framework holds both: accepting utilitarian override as legitimate requires rejecting consent as absolute, and vice versa.

narrative_ontology:cs_axiom_contradiction(collective_harm_prevention_primacy, bodily_autonomy_categorically_protected).
narrative_ontology:cs_axiom_contradiction(bodily_autonomy_categorically_protected, collective_harm_prevention_primacy).
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary_contradictions, '662cd7ba-9507-40bf-89b2-fc7a844ce0e7').
narrative_ontology:cs_created_at('662cd7ba-9507-40bf-89b2-fc7a844ce0e7', '').
