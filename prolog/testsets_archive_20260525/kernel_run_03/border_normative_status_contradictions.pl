% Axiom contradictions for kernel: border_normative_status
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% sovereignty_primary↔freedom_primary: Sovereignty-primary holds that states have foundational authority to exclude (exclusion requires no justification beyond state decision); freedom-primary holds that exclusion is a rights violation requiring extraordinary justification. No single framework can hold both: either exclusion is presumptively legitimate or presumptively illegitimate.

narrative_ontology:cs_axiom_contradiction(territorial_closure_prerequisite_for_self_determination, movement_fundamental_right).
narrative_ontology:cs_axiom_contradiction(movement_fundamental_right, territorial_closure_prerequisite_for_self_determination).
