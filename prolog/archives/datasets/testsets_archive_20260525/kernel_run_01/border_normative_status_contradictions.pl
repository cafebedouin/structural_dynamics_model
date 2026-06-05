% Axiom contradictions for kernel: border_normative_status
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% sovereignty_primary↔freedom_of_movement_primary: Sovereignty-primary holds that the political community's right to self-determination includes categorical admission control; freedom-of-movement-primary holds that no such categorical right exists because territorial jurisdiction does not entail exclusion authority. No single coherent framework can hold both: either borders are presumptively legitimate filters (sovereignty) or presumptively illegitimate barriers (freedom). The hybrid reading attempts synthesis but does not resolve the foundational axiom conflict.

narrative_ontology:cs_axiom_contradiction(political_community_self_determination_via_membership, freedom_of_movement_presumptive).
narrative_ontology:cs_axiom_contradiction(freedom_of_movement_presumptive, political_community_self_determination_via_membership).
