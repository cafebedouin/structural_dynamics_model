% Axiom contradictions for kernel: employment_boundary
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% formalist_employment_reading↔substantive_employment_reading: Formalist reading holds that contractual form determines employment status (no written employment contract = no employment relationship); substantive reading holds that economic reality determines status regardless of contract form (algorithmic control + income dependence = employment). No single coherent framework can hold both: accepting that contract form is dispositive requires rejecting that economic substance overrides form, and vice versa.

narrative_ontology:cs_axiom_contradiction(contract_label_determinative, economic_reality_primacy).
narrative_ontology:cs_axiom_contradiction(economic_reality_primacy, contract_label_determinative).
