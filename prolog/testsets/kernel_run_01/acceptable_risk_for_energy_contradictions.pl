% Axiom contradictions for kernel: acceptable_risk_for_energy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% expected_value_reading↔catastrophic_tail_reading: Expected-value reading treats all harms as commensurable via probability weighting (1000 deaths at 0.1% probability = 1 expected death). Catastrophic-tail reading treats spatially-concentrated irreversible harms as categorically different from diffuse harms regardless of probability weighting. No single coherent framework can simultaneously hold that catastrophic events are reducible to expected values AND that they constitute a categorically distinct harm type.
% expected_value_reading↔precautionary_reading: Expected-value reading places burden of proof on demonstrating harm exceeds benefits via quantified risk assessment. Precautionary reading reverses burden of proof under irreducible uncertainty, requiring proponents demonstrate safety. These are mutually exclusive epistemic stances on where default presumption lies when uncertainty cannot be eliminated.

narrative_ontology:cs_axiom_contradiction(risk_is_commensurable, irreversibility_tail_dominance).
narrative_ontology:cs_axiom_contradiction(irreversibility_tail_dominance, risk_is_commensurable).
narrative_ontology:cs_axiom_contradiction(risk_is_commensurable, irreducible_uncertainty_burden_shift).
narrative_ontology:cs_axiom_contradiction(irreducible_uncertainty_burden_shift, risk_is_commensurable).
