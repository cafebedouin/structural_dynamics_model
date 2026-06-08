% Axiom contradictions for kernel: imposition_mechanism_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% endogenous_climb_reading↔exogenous_override_reading: Endogenous climb axiom (legitimacy derives from cultural acceptance) and exogenous override axiom (legitimacy derives from state coercion) cannot coexist in a single framework. Accepting climb as true requires rejecting override as the legitimacy source, and vice versa. A framework cannot simultaneously hold that the same norm achieved legitimacy both through voluntary adoption and through coercive imposition.

narrative_ontology:cs_axiom_contradiction(organic_coordination_legitimacy, legitimacy_derives_from_coercion).
narrative_ontology:cs_axiom_contradiction(legitimacy_derives_from_coercion, organic_coordination_legitimacy).
