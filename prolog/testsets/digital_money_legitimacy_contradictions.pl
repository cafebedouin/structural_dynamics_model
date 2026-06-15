% Axiom contradictions for kernel: digital_money_legitimacy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% sovereign_cbdc_reading↔crypto_permissionless_reading: Sovereign CBDC axiom requires state monopoly on legitimate money issuance; crypto axiom requires money legitimacy independent of state permission. No coherent framework holds both simultaneously.

narrative_ontology:cs_axiom_contradiction(state_monopoly_on_legitimate_issuance, consensus_suffices_for_legitimacy).
narrative_ontology:cs_axiom_contradiction(consensus_suffices_for_legitimacy, state_monopoly_on_legitimate_issuance).
