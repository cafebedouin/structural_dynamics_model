% Axiom contradictions for kernel: curb_space_legitimacy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% property_tax_entitlement↔public_resource_pricing: Entitlement reading treats curb access as a purchased right (property tax already paid); pricing reading treats it as ongoing consumption of scarce public good requiring marginal cost payment. No framework holds both: either the tax purchased the right or it didn't.
% property_tax_entitlement↔equity_redistribution: Entitlement reading grounds legitimacy in homeowner tax contribution; equity reading grounds legitimacy in compensating those excluded from the subsidy. No framework simultaneously validates homeowner priority and non-car-owner compensation claims.

narrative_ontology:cs_axiom_contradiction(property_tax_purchases_public_goods_access, scarcity_requires_price_rationing).
narrative_ontology:cs_axiom_contradiction(scarcity_requires_price_rationing, property_tax_purchases_public_goods_access).
narrative_ontology:cs_axiom_contradiction(property_tax_purchases_public_goods_access, subsidy_skew_requires_compensation).
narrative_ontology:cs_axiom_contradiction(subsidy_skew_requires_compensation, property_tax_purchases_public_goods_access).
