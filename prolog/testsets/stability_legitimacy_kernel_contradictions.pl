% Axiom contradictions for kernel: stability_legitimacy_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% redistributive_stabilization_reading↔democratic_legitimacy_reading: Redistributive stabilization locates legitimacy in material outcomes achievable by a benevolent (even undemocratic) technocratic elite dispensing wealth taxes; democratic legitimacy locates it exclusively in procedural accountability regardless of material outcome — accepting that material sufficiency alone can legitimate rule forecloses the claim that only removability can.

narrative_ontology:cs_axiom_contradiction(redistribution_produces_mutual_benefit_legitimacy, legitimacy_requires_binding_recallable_voice).
narrative_ontology:cs_axiom_contradiction(legitimacy_requires_binding_recallable_voice, redistribution_produces_mutual_benefit_legitimacy).
