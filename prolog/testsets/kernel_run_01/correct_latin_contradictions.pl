% Axiom contradictions for kernel: correct_latin
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% living_drift_reading↔prescriptive_ideal_reading: Living drift treats language as naturally evolving through use (Dante's 'natural' vernacular principle); prescriptive ideal treats language as artificial construct bound by textual rules. No single framework can hold both that linguistic correctness emerges from living practice AND that it requires rigid conformity to historical textual precedent.
% textual_recovery_reading↔living_drift_reading: Textual recovery assumes Classical Latin is a recoverable historical form accessible through philological reconstruction; living drift assumes correctness is defined by current practice. No single framework can hold both that the correct form is what ancient texts preserve AND that the correct form is what living users produce.

narrative_ontology:cs_axiom_contradiction(change_through_use_is_evolution_not_corruption, cicero_defines_correctness).
narrative_ontology:cs_axiom_contradiction(cicero_defines_correctness, change_through_use_is_evolution_not_corruption).
narrative_ontology:cs_axiom_contradiction(classical_texts_are_historically_determinate, change_through_use_is_evolution_not_corruption).
narrative_ontology:cs_axiom_contradiction(change_through_use_is_evolution_not_corruption, classical_texts_are_historically_determinate).
