% Axiom contradictions for kernel: future_claims_present_resources
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% issuance_as_physical_backing↔issuance_as_market_discovered_confidence: The physicalist reading holds that a claim's legitimacy is an objective fact about real resource answerability, verifiable in principle by ex ante modeling independent of what any market currently prices; the catallactic reading holds that no such independent verification is possible or meaningful — legitimacy IS whatever price discovery reveals, and there is no fact about 'true' physical backing standing apart from and correcting market judgment. A framework cannot hold both that markets can be substantively wrong about physical answerability (physicalist) and that market price is the exhaustive and only test of legitimacy with no external corrective (catallactic).
% issuance_as_deliberative_judgment↔issuance_as_market_discovered_confidence: The deliberative reading's axiom is that monetary quantity is a matter for accountable collective judgment precisely BECAUSE unmanaged market/credit processes produce social costs (boom-bust, concentration) that only visible constituted authority can dampen; the catallactic reading's axiom is that no constituted body can out-perform or legitimately override the informational content of decentralized price discovery, and that attempts to do so necessarily substitute someone's interested judgment for genuine discovery. Accepting that constituted deliberation is the appropriate legitimating mechanism requires denying that market discovery is self-sufficient, and vice versa.

narrative_ontology:cs_axiom_contradiction(legitimacy_tracks_physical_redeemability_not_procedure, price_discovery_is_the_legitimating_test).
narrative_ontology:cs_axiom_contradiction(price_discovery_is_the_legitimating_test, legitimacy_tracks_physical_redeemability_not_procedure).
narrative_ontology:cs_axiom_contradiction(legitimacy_from_deciding_body_constitution, price_discovery_is_the_legitimating_test).
narrative_ontology:cs_axiom_contradiction(price_discovery_is_the_legitimating_test, legitimacy_from_deciding_body_constitution).
