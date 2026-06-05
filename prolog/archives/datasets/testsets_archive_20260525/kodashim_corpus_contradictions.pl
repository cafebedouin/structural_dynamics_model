% Axiom contradictions for kernel: kodashim_corpus
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% study_as_exercise↔performance_only: Study-as-exercise holds that intellectual engagement IS the complete fulfillment of the mitzvah (no future performance needed). Performance-only holds that study is preparation for future physical restoration (current engagement is incomplete). No single framework can hold both: either study completes the obligation or it does not.
% study_as_exercise↔substitution_archive: Study-as-exercise holds the kernel is occupied (sacrifice law remains living divine command). Substitution-archive holds the kernel was superseded (sacrifice replaced by prayer/study, now archived as memorial). No single framework can hold both: either the original commandment persists or it was replaced.

narrative_ontology:cs_axiom_contradiction(study_is_performance_not_substitute, performance_validates_law).
narrative_ontology:cs_axiom_contradiction(performance_validates_law, study_is_performance_not_substitute).
narrative_ontology:cs_axiom_contradiction(study_is_performance_not_substitute, substitution_is_complete_and_sufficient).
narrative_ontology:cs_axiom_contradiction(substitution_is_complete_and_sufficient, study_is_performance_not_substitute).
