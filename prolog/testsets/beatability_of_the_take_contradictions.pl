% Axiom contradictions for kernel: beatability_of_the_take
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% folk_mountain_reading↔meta_prediction_reading: Folk_mountain holds that no model can systematically exceed the market's implied probabilities net of take; meta_prediction holds that the market's implied probabilities are demonstrably and exploitably biased. A framework cannot simultaneously treat the take-adjusted market as efficient (folk_mountain's implicit axiom) and as containing extractable inefficiency (meta_prediction's foundational axiom) — accepting one requires rejecting the other's premise about what the market price represents.
% flow_extraction_reading↔meta_prediction_reading: Flow_extraction's axiom is that house revenue and profitability questions are category-separate from outcome-prediction accuracy (the house doesn't care who wins). Meta_prediction's axiom is that the entire game is defined by outcome-prediction accuracy relative to the public. These are not merely different emphases: flow_extraction denies that 'beatability' is even a coherent question about the house's position, while meta_prediction treats beatability as the only question that matters. No single framework can hold that beatability is both a category error and the central fact.

narrative_ontology:cs_axiom_contradiction(take_plus_variance_forecloses_all_edge, actionable_target_is_prediction_error_not_outcome).
narrative_ontology:cs_axiom_contradiction(actionable_target_is_prediction_error_not_outcome, take_plus_variance_forecloses_all_edge).
narrative_ontology:cs_axiom_contradiction(house_indifference_to_outcome_distribution, actionable_target_is_prediction_error_not_outcome).
narrative_ontology:cs_axiom_contradiction(actionable_target_is_prediction_error_not_outcome, house_indifference_to_outcome_distribution).
