% Axiom contradictions for kernel: end_of_life_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% autonomy_reading↔sanctity_reading: Autonomy reading holds that individual choice is the sole legitimate authority over one's death; sanctity reading holds that life preservation is a categorical constraint that overrides individual choice. No single coherent framework can simultaneously hold both that individual autonomy is supreme in end-of-life decisions AND that life preservation categorically prohibits those decisions.

narrative_ontology:cs_axiom_contradiction(respect_for_autonomous_choice_over_unbearable_suffering, life_intrinsic_dignity_inviolable).
narrative_ontology:cs_axiom_contradiction(life_intrinsic_dignity_inviolable, respect_for_autonomous_choice_over_unbearable_suffering).
narrative_ontology:cs_story_uid(end_of_life_authority_contradictions, 'f2c7981e-aaec-4898-b5f8-6c5ccc6511cb').
narrative_ontology:cs_created_at('f2c7981e-aaec-4898-b5f8-6c5ccc6511cb', '').
