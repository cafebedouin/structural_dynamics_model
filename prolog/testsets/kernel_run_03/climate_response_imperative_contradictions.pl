% Axiom contradictions for kernel: climate_response_imperative
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% mitigation_priority_reading↔degrowth_reading: Mitigation-priority assumes continued GDP growth as compatible with emissions reduction (via decoupling and CDR); degrowth asserts GDP growth is structurally incompatible with adequate climate response. No single framework can hold both 'growth is necessary for climate finance' and 'growth must be abandoned for climate stability' simultaneously.
% adaptation_priority_reading↔mitigation_priority_reading: Adaptation-priority treats mitigation as aspirational and prioritizes immediate resilience investment; mitigation-priority treats adaptation as residual and prioritizes emissions reduction. A framework cannot simultaneously hold 'adaptation is the primary response' and 'mitigation is the primary response' as foundational commitments, though both can coexist as complementary tactics within a hybrid framework.

narrative_ontology:cs_axiom_contradiction(emissions_reduction_primary_imperative, global_north_consumption_reduction_mandatory).
narrative_ontology:cs_axiom_contradiction(global_north_consumption_reduction_mandatory, emissions_reduction_primary_imperative).
narrative_ontology:cs_axiom_contradiction(immediate_suffering_priority, emissions_reduction_primary_imperative).
narrative_ontology:cs_axiom_contradiction(emissions_reduction_primary_imperative, immediate_suffering_priority).
narrative_ontology:cs_story_uid(climate_response_imperative_contradictions, '1c82e520-4381-460e-86b4-58518f5ae285').
narrative_ontology:cs_created_at('1c82e520-4381-460e-86b4-58518f5ae285', '').
