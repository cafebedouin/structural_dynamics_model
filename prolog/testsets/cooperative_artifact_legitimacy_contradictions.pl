% Axiom contradictions for kernel: cooperative_artifact_legitimacy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% legibility_primacy_reading↔authorial_primacy_reading: Legibility-primacy holds that the collective enterprise, not the individual hand, is the proper unit of ownership over the joint artifact; authorial-primacy holds that traceable individual origination is the non-waivable ground of legitimacy. Accepting that erasing authorial trace is legitimate (legibility-primacy's axiom) requires denying that traceable origination is a non-waivable legitimacy condition (authorial-primacy's axiom) — no single framework can hold both as foundational simultaneously.

narrative_ontology:cs_axiom_contradiction(artifact_belongs_to_enterprise_not_hand, traceable_origination_is_the_legitimacy_criterion).
narrative_ontology:cs_axiom_contradiction(traceable_origination_is_the_legitimacy_criterion, artifact_belongs_to_enterprise_not_hand).
