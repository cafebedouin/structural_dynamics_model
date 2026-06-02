% Axiom contradictions for kernel: federation_membership_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.
:- multifile narrative_ontology:cs_contradiction_of/2.

% integration_reading↔sovereignty_reading: Integration_reading holds that legitimate authority over labor mobility resides at supranational level (CJEU/Commission) as necessary for single market completion. Sovereignty_reading holds that legitimate authority over labor mobility resides at national level (member state legislatures) as core democratic sovereignty. No single coherent framework can hold both axioms simultaneously - accepting supranational authority as legitimate requires rejecting national authority as ultimate, and vice versa.

narrative_ontology:cs_axiom_contradiction(complete_labor_market_integration_technically_necessary, member_state_welfare_control_essential).
narrative_ontology:cs_axiom_contradiction(member_state_welfare_control_essential, complete_labor_market_integration_technically_necessary).
narrative_ontology:cs_story_uid(federation_membership_kernel_contradictions, '00dd4a58-e245-416c-a8aa-8c3a4adef7f8').
narrative_ontology:cs_contradiction_of(federation_membership_kernel_contradictions, federation_membership_kernel).
narrative_ontology:cs_created_at('00dd4a58-e245-416c-a8aa-8c3a4adef7f8', '').
