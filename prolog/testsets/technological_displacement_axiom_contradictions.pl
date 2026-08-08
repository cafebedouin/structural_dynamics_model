% Axiom contradictions for kernel: technological_displacement_axiom
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% temporal_equivalence_reading↔clock_incompatibility_reading: Temporal equivalence holds that the historical offsetting mechanism operates on the relevant timescale for this transition; clock incompatibility holds that it structurally cannot given the depreciation clock. Both cannot be true of the same transition — one asserts the mechanism completes in time, the other asserts it is foreclosed by time. No single framework can hold 'the ladder survives' and 'the ladder is dismantled' simultaneously for the same historical episode.
% clock_incompatibility_reading↔skills_mismatch_reading: Clock incompatibility asserts the training-pipeline collapse is structural and irreversible by policy (the model itself is broken); skills mismatch asserts the same phenomenon is a remediable allocation problem solvable by retraining. Accepting that the pipeline is structurally severed requires rejecting that retraining can restore it, and vice versa — these are mutually exclusive diagnoses of the same observable, not complementary partial explanations.

narrative_ontology:cs_axiom_contradiction(displacement_horizon_is_temporary_by_default, absorption_mechanism_is_real_but_rate_limited).
narrative_ontology:cs_axiom_contradiction(absorption_mechanism_is_real_but_rate_limited, displacement_horizon_is_temporary_by_default).
narrative_ontology:cs_axiom_contradiction(absorption_mechanism_is_real_but_rate_limited, displacement_population_is_remediable).
narrative_ontology:cs_axiom_contradiction(displacement_population_is_remediable, absorption_mechanism_is_real_but_rate_limited).
