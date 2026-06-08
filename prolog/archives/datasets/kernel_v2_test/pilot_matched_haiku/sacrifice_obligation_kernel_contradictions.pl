% Axiom contradictions for kernel: sacrifice_obligation_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% study_as_exercise_reading↔performance_only_reading: Study-as-exercise holds that rabbinic authority can redefine mitzvah modality (study = performance); performance-only holds that physical enactment is non-negotiable. No framework can simultaneously accept both: either rabbinic authority has this transformative power or it does not.
% study_as_exercise_reading↔symbolic_archive_reading: Study-as-exercise holds sacrifice law as binding halakhic obligation currently fulfilled through study; symbolic-archive holds it as non-binding cultural heritage. No framework can hold both: either the law binds or it does not.
% performance_only_reading↔messianic_suspension_reading: Performance-only holds the obligation remains active and unfulfilled (creating ongoing violation); messianic-suspension holds the obligation is divinely placed in abeyance (no violation during suspension). No framework can hold both: either the obligation is currently binding or it is suspended.
% messianic_suspension_reading↔symbolic_archive_reading: Messianic-suspension holds sacrifice will be restored as binding divine command; symbolic-archive holds it as historical artifact with no future binding force. No framework can hold both: either messianic restoration reactivates the obligation or the obligation is permanently historical.

narrative_ontology:cs_axiom_contradiction(intellectual_engagement_fulfills_obligation, physical_performance_required).
narrative_ontology:cs_axiom_contradiction(physical_performance_required, intellectual_engagement_fulfills_obligation).
narrative_ontology:cs_axiom_contradiction(intellectual_engagement_fulfills_obligation, no_binding_obligation_without_temple).
narrative_ontology:cs_axiom_contradiction(no_binding_obligation_without_temple, intellectual_engagement_fulfills_obligation).
narrative_ontology:cs_axiom_contradiction(physical_performance_required, obligation_suspended_not_transformed).
narrative_ontology:cs_axiom_contradiction(obligation_suspended_not_transformed, physical_performance_required).
narrative_ontology:cs_axiom_contradiction(obligation_suspended_not_transformed, no_binding_obligation_without_temple).
narrative_ontology:cs_axiom_contradiction(no_binding_obligation_without_temple, obligation_suspended_not_transformed).
