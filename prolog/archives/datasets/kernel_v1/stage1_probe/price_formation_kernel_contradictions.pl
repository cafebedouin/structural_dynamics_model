% Axiom contradictions for kernel: price_formation_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% naturalist_reading↔georgist_reading: Naturalist treats all price components as earned returns to scarcity/productivity. Georgist axiom that land rent is unearned contradicts this — no framework holds both 'all returns are earned' and 'land returns are unearned' simultaneously.
% naturalist_reading↔financialization_reading: Naturalist axiom (price reflects real scarcity/preference) contradicts financialization axiom (price reflects credit availability and speculative feedback). If price is credit-driven, it does not reflect real scarcity; if it reflects real scarcity, credit is neutral.

narrative_ontology:cs_axiom_contradiction(prices_discovered_from_objective_scarcity, land_rent_separable_analytically).
narrative_ontology:cs_axiom_contradiction(land_rent_separable_analytically, prices_discovered_from_objective_scarcity).
narrative_ontology:cs_axiom_contradiction(prices_discovered_from_objective_scarcity, credit_expansion_price_driver).
narrative_ontology:cs_axiom_contradiction(credit_expansion_price_driver, prices_discovered_from_objective_scarcity).
