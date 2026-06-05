% Axiom contradictions for kernel: end_of_life_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% autonomy_reading↔sanctity_reading: Autonomy-primary holds that competent individual choice is sufficient moral authority to end life; sanctity-primary holds that intrinsic life value prohibits intentional killing regardless of consent. No single coherent framework can simultaneously hold both: accepting autonomy as sufficient authority requires rejecting sanctity's prohibition as overriding, and vice versa.

narrative_ontology:cs_axiom_contradiction(competent_autonomy_foundational_authority, intrinsic_value_inviolable).
narrative_ontology:cs_axiom_contradiction(intrinsic_value_inviolable, competent_autonomy_foundational_authority).
