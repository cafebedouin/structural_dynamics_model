% Axiom contradictions for kernel: authentic_preference_boundary
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% behaviorist_counterfactual_reading↔genealogical_origin_reading: The counterfactual reading holds that there is no fact about authenticity beyond what the re-exposure test would reveal (verificationist axiom); the genealogical reading holds that authenticity is a determinate historical fact independent of any test's ability to recover it (realist axiom). A framework cannot simultaneously assert that authenticity just is dispositional-testability and that it is a fact that can outrun all possible tests.
% behaviorist_counterfactual_reading↔phenomenological_endorsement_reading: The counterfactual reading treats introspective report as categorically inadmissible ('reports are the thing under suspicion'); the endorsement reading treats a disciplined introspective state as constitutive of authenticity itself. One cannot hold both that first-person access is definitionally disqualified from bearing on the fact and that it is the very locus where the fact consists.

narrative_ontology:cs_axiom_contradiction(authenticity_exhausted_by_disposition, determinacy_without_recoverability).
narrative_ontology:cs_axiom_contradiction(determinacy_without_recoverability, authenticity_exhausted_by_disposition).
narrative_ontology:cs_axiom_contradiction(authenticity_exhausted_by_disposition, endorsement_is_genuine_detectable_mental_state).
narrative_ontology:cs_axiom_contradiction(endorsement_is_genuine_detectable_mental_state, authenticity_exhausted_by_disposition).
