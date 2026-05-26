% Axiom contradictions for kernel: zero_mathematical_status
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% parmenidean_rejection↔number_reading: Parmenidean axiom (nothing cannot exist as entity) and number-reading axiom (zero exists as mathematical object with properties) are mutually exclusive—no single coherent framework can hold both that zero is ontologically impossible and that zero is a legitimate number

narrative_ontology:cs_axiom_contradiction(non_being_cannot_exist_as_mathematical_entity, zero_is_numerical_entity).
narrative_ontology:cs_axiom_contradiction(zero_is_numerical_entity, non_being_cannot_exist_as_mathematical_entity).
