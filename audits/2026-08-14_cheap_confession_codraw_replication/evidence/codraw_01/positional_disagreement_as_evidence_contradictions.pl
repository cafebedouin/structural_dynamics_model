% Axiom contradictions for kernel: positional_disagreement_as_evidence
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% standpoint_reading↔pragmatist_reading: Standpoint reading holds that the marginalized position has a principled, asymmetric epistemic advantage that must be weighted, not merely pooled toward convergence; pragmatist reading holds that no position has a priori epistemic privilege and that resolution comes only through extended, symmetric inquiry. Accepting standpoint's axiom (structural credibility asymmetry is itself evidence-grade) requires rejecting pragmatist neutrality about positions as a starting point, and vice versa.
% proceduralist_reading↔instrumentalist_reading: Proceduralist reading locates evidentiary force in the cost and design of the procedure (expense IS the signal); instrumentalist reading explicitly argues cheap, model-assisted generation can produce legitimate omegas despite trivial production cost, relocating the cost elsewhere (abiding, not producing). A framework cannot simultaneously hold that evidentiary force requires production-cost expense AND that cheap production of the same artifact is legitimate — one must reject the other's cost-location claim.

narrative_ontology:cs_axiom_contradiction(positional_asymmetry_grounds_corrective_weighting, no_a_priori_standing_advantage).
narrative_ontology:cs_axiom_contradiction(no_a_priori_standing_advantage, positional_asymmetry_grounds_corrective_weighting).
narrative_ontology:cs_axiom_contradiction(evidentiary_force_derives_from_procedural_cost_not_position, tractability_is_a_capacity_fact_not_a_standpoint_fact).
narrative_ontology:cs_axiom_contradiction(tractability_is_a_capacity_fact_not_a_standpoint_fact, evidentiary_force_derives_from_procedural_cost_not_position).
