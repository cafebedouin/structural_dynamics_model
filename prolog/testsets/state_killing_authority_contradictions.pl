% Axiom contradictions for kernel: state_killing_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% retributive_desert↔categorical_abolition: Retributive reading holds that murderers forfeit their right to life via their crime (rights are conditional on desert); abolition reading holds that life is inalienable and cannot be forfeited (rights are unconditional). No single coherent framework can hold both that a right is forfeitable and that the same right is inalienable.
% deterrence_instrument↔categorical_abolition: Deterrence reading treats the condemned person as an instrumental cost in a utilitarian calculus (permissible to kill if net lives saved); abolition reading treats persons as ends-in-themselves whose lives cannot be traded off for aggregate benefit. No single coherent framework can hold both that persons may be instrumentalized for collective benefit and that persons are categorically non-instrumentalizable.

narrative_ontology:cs_axiom_contradiction(murderers_forfeit_rights, inalienable_life).
narrative_ontology:cs_axiom_contradiction(inalienable_life, murderers_forfeit_rights).
narrative_ontology:cs_axiom_contradiction(deterrence_empirical_efficacy, inalienable_life).
narrative_ontology:cs_axiom_contradiction(inalienable_life, deterrence_empirical_efficacy).
narrative_ontology:cs_story_uid(state_killing_authority_contradictions, 'd53aa3c4-a3ca-48ac-9784-2f16eb5260e9').
narrative_ontology:cs_created_at('d53aa3c4-a3ca-48ac-9784-2f16eb5260e9', '').
