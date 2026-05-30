% Axiom contradictions for kernel: border_normative_status
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.
:- multifile narrative_ontology:cs_contradiction_of/2.

% sovereignty_primary↔freedom_primary: Sovereignty-primary holds that states have foundational authority to exclude (exclusion requires no justification beyond state decision); freedom-primary holds that exclusion is a rights violation requiring extraordinary justification. No single framework can hold both: either exclusion is presumptively legitimate or presumptively illegitimate.

narrative_ontology:cs_axiom_contradiction(territorial_closure_prerequisite_for_self_determination, movement_fundamental_right).
narrative_ontology:cs_axiom_contradiction(movement_fundamental_right, territorial_closure_prerequisite_for_self_determination).
narrative_ontology:cs_story_uid(border_normative_status_contradictions, 'a1373c9e-15eb-41c1-9f7a-13e3d56fa270').
narrative_ontology:cs_contradiction_of(border_normative_status_contradictions, border_normative_status).
narrative_ontology:cs_created_at('a1373c9e-15eb-41c1-9f7a-13e3d56fa270', '').
