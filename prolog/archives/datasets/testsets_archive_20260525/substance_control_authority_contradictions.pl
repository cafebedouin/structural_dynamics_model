% Axiom contradictions for kernel: substance_control_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% prohibition_reading↔harm_reduction_reading: Prohibition axiom: drug use itself is the harm to be prevented. Harm reduction axiom: unsafe use practices are the harm; use itself is accepted. No framework can simultaneously hold 'use must be prevented' and 'use is acceptable if made safer.'
% prohibition_reading↔legalization_reading: Prohibition axiom: state has duty to criminalize use to protect society. Legalization axiom: state criminalization causes greater harm than use itself. No framework can simultaneously hold 'criminalization protects' and 'criminalization harms.'
% harm_reduction_reading↔legalization_reading: Harm reduction axiom: continued criminalization is compatible with health interventions (decriminalization of possession, not supply). Legalization axiom: only full market legalization eliminates systemic violence and enforcement harms. These can coexist in hybrid frameworks (Portugal model), so NOT axiom-contradictory despite different scopes.

narrative_ontology:cs_axiom_contradiction(intrinsic_use_harm_primary, continued_use_inevitable_medium_term).
narrative_ontology:cs_axiom_contradiction(continued_use_inevitable_medium_term, intrinsic_use_harm_primary).
narrative_ontology:cs_axiom_contradiction(intrinsic_use_harm_primary, drug_use_outside_state_constraint_scope).
narrative_ontology:cs_axiom_contradiction(drug_use_outside_state_constraint_scope, intrinsic_use_harm_primary).
narrative_ontology:cs_axiom_contradiction(continued_use_inevitable_medium_term, drug_use_outside_state_constraint_scope).
narrative_ontology:cs_axiom_contradiction(drug_use_outside_state_constraint_scope, continued_use_inevitable_medium_term).
