% Axiom contradictions for kernel: knowledge_legitimacy_biomedicine
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% institutional_validation_reading↔pragmatic_action_reading: Institutional reading requires proof before action (Type I error minimization); pragmatic reading accepts action under uncertainty when delay costs exceed error costs (Type II error minimization). No single framework simultaneously minimizes both error types—they represent incompatible risk philosophies.

narrative_ontology:cs_axiom_contradiction(institutional_validation_necessity, action_guidance_suffices_for_legitimacy).
narrative_ontology:cs_axiom_contradiction(action_guidance_suffices_for_legitimacy, institutional_validation_necessity).
