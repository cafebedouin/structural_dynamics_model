% Axiom contradictions for kernel: latin_correctness
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% living_drift_reading↔textual_recovery_reading: Living drift treats continuous use as constitutive of correctness (language is what competent speakers do); textual recovery treats ancient texts as normative regardless of living practice (language is what the texts say). No single framework can hold both: either living practice or textual authority is the ultimate arbiter of correctness.

narrative_ontology:cs_axiom_contradiction(usage_is_norma_loquendi, classical_texts_authentically_recoverable).
narrative_ontology:cs_axiom_contradiction(classical_texts_authentically_recoverable, usage_is_norma_loquendi).
