% Axiom contradictions for kernel: persona_as_valid_proxy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% representational_correspondence_reading↔sociotechnical_risk_reading: The correspondence reading treats the central failure as epistemic (the tool doesn't yet match reality, so improve the calibration). The risk reading treats improved correspondence as making the tool more dangerous, not more legitimate (a more accurate targeting/profiling engine is a worse externality, not a better one). One framework cannot hold both 'closing the correspondence gap is the path to legitimacy' and 'closing the correspondence gap increases the harm surface' as its organizing principle without incoherence.

narrative_ontology:cs_axiom_contradiction(legitimacy_requires_joint_distributional_fidelity, artifact_existence_is_the_hazard).
narrative_ontology:cs_axiom_contradiction(artifact_existence_is_the_hazard, legitimacy_requires_joint_distributional_fidelity).
