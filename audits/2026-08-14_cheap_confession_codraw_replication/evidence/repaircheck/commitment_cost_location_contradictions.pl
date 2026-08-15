% Axiom contradictions for kernel: commitment_cost_location
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% legibility_reading↔enforcement_deflation_reading: legibility_reading holds that visibility-without-enforcement is a genuine, sufficient achievement (the essay's stated thesis); enforcement_deflation_reading holds that visibility-without-enforcement is not an achievement at all because it changes no incentive. A framework cannot simultaneously treat mere visibility as sufficient success and as null success — accepting one requires rejecting the other's evaluative standard for what 'the machinery secures' means.
% legibility_reading↔temporal_identity_reading: legibility_reading grounds the entire phenomenon in third-party observability (the cost is real because and only because it can now be seen by others); temporal_identity_reading grounds it in agent-internal continuity that holds independently of any observer. A framework cannot hold both that observability is constitutive of the cost and that the cost is fully present in a zero-observer scenario — one must give priority to either the social or the internal ground.

narrative_ontology:cs_axiom_contradiction(confession_cost_invariance, visibility_without_sanction_is_not_a_price).
narrative_ontology:cs_axiom_contradiction(visibility_without_sanction_is_not_a_price, confession_cost_invariance).
narrative_ontology:cs_axiom_contradiction(confession_cost_invariance, cost_located_in_intrapersonal_continuity_not_observability).
narrative_ontology:cs_axiom_contradiction(cost_located_in_intrapersonal_continuity_not_observability, confession_cost_invariance).
