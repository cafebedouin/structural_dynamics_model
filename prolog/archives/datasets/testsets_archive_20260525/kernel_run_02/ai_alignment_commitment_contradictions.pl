% Axiom contradictions for kernel: ai_alignment_commitment
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% safety_control_reading↔ethics_justice_reading: Safety reading holds that existential risk is the tractable high-impact cause (effective altruism axiom); ethics reading holds that speculative future harms divert from demonstrated present injustice (precautionary axiom). No single framework can simultaneously hold 'prioritize tractable high-impact future risk' and 'prioritize demonstrated present harm' as foundational without one subordinating the other.

narrative_ontology:cs_axiom_contradiction(catastrophic_loss_of_control_is_highest_priority_harm, present_day_bias_harm_immediate_priority).
narrative_ontology:cs_axiom_contradiction(present_day_bias_harm_immediate_priority, catastrophic_loss_of_control_is_highest_priority_harm).
