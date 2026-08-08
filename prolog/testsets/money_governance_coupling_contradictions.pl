% Axiom contradictions for kernel: money_governance_coupling
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% fusion_reading↔adjacency_reading: Fusion's founding axiom (capital contribution is a legitimate and sufficient basis for governance weight) is directly negated by adjacency's founding axiom (wealth held on the chain must confer zero weight over the chain). No single coherent constitutional design can simultaneously let stake buy voice and structurally forbid stake from buying voice.
% exile_reading↔adjacency_reading: Exile's axiom holds that necessary monetary judgment (crisis response, lender-of-last-resort action) requires an office with discretion unconstrained by pre-written rules. Adjacency's axiom holds that all governance-relevant monetary contact must be enumerated and capped in advance, with no standing discretionary office. A framework cannot both require an unconstrained discretionary monetary authority and forbid the existence of unconstrained discretionary authority over money-governance contact points.
% fusion_reading↔exile_reading: Fusion's axiom locates ultimate authority over monetary/governance rules IN the same body that holds the money (stake = law). Exile's axiom locates that authority OUTSIDE the body accountable to the governed, in a separated discretionary institution. A single framework cannot ground legitimacy simultaneously in capital-holder self-rule and in externalized technocratic separation from capital-holder rule — they answer 'who decides' with mutually exclusive bodies.

narrative_ontology:cs_axiom_contradiction(capital_risk_bearing_grounds_permanent_voice, wealth_confers_no_direct_vote_weight).
narrative_ontology:cs_axiom_contradiction(wealth_confers_no_direct_vote_weight, capital_risk_bearing_grounds_permanent_voice).
narrative_ontology:cs_axiom_contradiction(discretion_irreducible_to_rule, wealth_confers_no_direct_vote_weight).
narrative_ontology:cs_axiom_contradiction(wealth_confers_no_direct_vote_weight, discretion_irreducible_to_rule).
narrative_ontology:cs_axiom_contradiction(capital_risk_bearing_grounds_permanent_voice, discretion_irreducible_to_rule).
narrative_ontology:cs_axiom_contradiction(discretion_irreducible_to_rule, capital_risk_bearing_grounds_permanent_voice).
