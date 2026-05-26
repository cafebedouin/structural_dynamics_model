% Axiom contradictions for kernel: substance_control_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% prohibition_reading↔legalization_reading: Prohibition axiom: state must prevent access to protect users. Legalization axiom: state-enforced scarcity creates the primary harm (violence, corruption). No framework can hold both 'access prevention is protective' and 'access prevention is the harm source' simultaneously.

narrative_ontology:cs_axiom_contradiction(drug_use_as_moral_failure_requiring_constraint, regulation_superior_to_prohibition).
narrative_ontology:cs_axiom_contradiction(regulation_superior_to_prohibition, drug_use_as_moral_failure_requiring_constraint).
