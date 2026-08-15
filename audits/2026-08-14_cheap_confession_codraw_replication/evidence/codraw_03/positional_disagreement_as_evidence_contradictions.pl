% Axiom contradictions for kernel: positional_disagreement_as_evidence
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% standpoint_reading↔pragmatist_reading: Standpoint reading holds that the marginalized position has a principled, asymmetric epistemic advantage that must be weighted, not merely pooled toward convergence; pragmatist reading holds that no position has a priori epistemic privilege and that resolution comes only through extended, symmetric inquiry. Accepting standpoint's axiom (structural credibility asymmetry is itself evidence-grade) requires rejecting pragmatist neutrality about positions as a starting point, and vice versa.

narrative_ontology:cs_axiom_contradiction(asymmetric_epistemic_access_by_position, no_position_has_a_priori_standing_advantage).
narrative_ontology:cs_axiom_contradiction(no_position_has_a_priori_standing_advantage, asymmetric_epistemic_access_by_position).
