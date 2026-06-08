% Axiom contradictions for kernel: sacrifice_obligation_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% study_as_exercise_reading↔performance_only_reading: Study-as-exercise holds that rabbinic authority can redefine mitzvah modality (study = performance); performance-only holds that physical enactment is non-negotiable. No framework can simultaneously accept both: either rabbinic authority has this transformative power or it does not.
% study_as_exercise_reading↔symbolic_archive_reading: Study-as-exercise holds sacrifice law as binding halakhic obligation currently fulfilled through study; symbolic-archive holds it as non-binding cultural heritage. No framework can hold both: either the law binds or it does not.

narrative_ontology:cs_axiom_contradiction(obligation_persists_post_temple, physical_performance_irreplaceable).
narrative_ontology:cs_axiom_contradiction(physical_performance_irreplaceable, obligation_persists_post_temple).
narrative_ontology:cs_axiom_contradiction(obligation_persists_post_temple, study_preserves_tradition_without_obligation).
narrative_ontology:cs_axiom_contradiction(study_preserves_tradition_without_obligation, obligation_persists_post_temple).
