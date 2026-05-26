% Axiom contradictions for kernel: nuclear_impossibility_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% structural_contraction_reading↔rational_dropout_reading: Contraction reading holds that no coherent strategic framework can include 'winnable nuclear war' because victory is physically impossible; dropout reading holds that victory is possible but irrational. No single framework can hold both 'victory is impossible' and 'victory is possible but too costly' as simultaneously true.
% structural_contraction_reading↔credibility_paradox_reading: Contraction reading holds that use is impossible (guaranteed mutual annihilation removes it from possibility space); paradox reading holds that use must remain possible for deterrence to function. No framework can hold both 'use is impossible' and 'use must be credibly possible' simultaneously.

narrative_ontology:cs_axiom_contradiction(war_exits_reachable_set, cost_benefit_rationality_excludes_war).
narrative_ontology:cs_axiom_contradiction(cost_benefit_rationality_excludes_war, war_exits_reachable_set).
narrative_ontology:cs_axiom_contradiction(war_exits_reachable_set, credibility_paradox_is_insoluble).
narrative_ontology:cs_axiom_contradiction(credibility_paradox_is_insoluble, war_exits_reachable_set).
