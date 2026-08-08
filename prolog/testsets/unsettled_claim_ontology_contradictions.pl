% Axiom contradictions for kernel: unsettled_claim_ontology
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% stance_reading↔drift_reading: Drift_reading requires that belief-forming machinery originally existed (or exists) and was reshaped by the register; stance_reading requires that such machinery never formed at all in this domain. Both cannot be true of the same claim-domain simultaneously.
% filter_reading↔impression_management_reading: Filter_reading's axiom is that escalation under challenge is the mechanism's success condition (challenge is the informative event); impression_management_reading's axiom is that retreat under challenge is the rational response to preserve audience approval. The same behavioral observation (escalate vs. retreat under expert challenge) cannot be explained by both axioms as the mechanism's intended function simultaneously — the essay's own worked example treats them as mutually falsifying.

narrative_ontology:cs_axiom_contradiction(no_calibration_target_beneath_assertion, sustained_performance_reshapes_self_model).
narrative_ontology:cs_axiom_contradiction(sustained_performance_reshapes_self_model, no_calibration_target_beneath_assertion).
narrative_ontology:cs_axiom_contradiction(truth_indifference_is_instrumental_not_diagnostic, claim_strength_tracks_audience_composition_not_evidence).
narrative_ontology:cs_axiom_contradiction(claim_strength_tracks_audience_composition_not_evidence, truth_indifference_is_instrumental_not_diagnostic).
