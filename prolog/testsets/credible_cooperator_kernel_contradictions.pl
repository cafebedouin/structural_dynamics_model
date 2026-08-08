% Axiom contradictions for kernel: credible_cooperator_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% audit_reading↔commitment_reading: The audit reading holds that trust must be perpetually re-earned through observable behavior (no act is ever fully binding); the commitment reading holds that trust is precisely constituted by an act that forecloses future re-evaluation. A framework that treats all trust as perpetually revisable cannot simultaneously treat any trust as irreversible by design — accepting one axiom as true requires rejecting the other.
% commitment_reading↔exit_option_reading: The commitment reading treats binding/exit-foreclosure as the mechanism that manufactures credibility. The exit-option reading treats exit-foreclosure as corrosive to genuine cooperation, producing strategic pretense rather than real cooperative intent once exit is priced out. One framework cannot hold that removing exit both creates credible cooperation and destroys it.
% audit_reading↔signaling_market_reading: The audit reading locates the enforcement mechanism in behavioral verification over time (a relational, dyadic monitoring cost). The signaling-market reading locates it in a market-clearing cost differential that requires no monitoring at all — signals separate types without anyone observing subsequent behavior. A single framework cannot hold that verification is necessary AND that self-selecting cost alone is sufficient, since the market reading explicitly substitutes for the audit reading's core mechanism.

narrative_ontology:cs_axiom_contradiction(trust_requires_continuous_reverification, irreversibility_constitutes_credibility).
narrative_ontology:cs_axiom_contradiction(irreversibility_constitutes_credibility, trust_requires_continuous_reverification).
narrative_ontology:cs_axiom_contradiction(irreversibility_constitutes_credibility, exit_payoff_dominance_determines_cooperation).
narrative_ontology:cs_axiom_contradiction(exit_payoff_dominance_determines_cooperation, irreversibility_constitutes_credibility).
narrative_ontology:cs_axiom_contradiction(trust_requires_continuous_reverification, trust_emerges_from_signal_cost_not_verification).
narrative_ontology:cs_axiom_contradiction(trust_emerges_from_signal_cost_not_verification, trust_requires_continuous_reverification).
