% Axiom contradictions for kernel: state_killing_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% retributive_desert↔categorical_impermissibility: Retributive reading holds that moral desert can require death (murderer forfeits right to life); abolitionist reading holds right to life is inalienable (cannot be forfeited). No coherent framework can hold both that a right is inalienable AND that it can be forfeited through wrongdoing.
% deterrence_instrument↔categorical_impermissibility: Deterrence reading treats persons as instrumentally sacrificeable for aggregate welfare; abolitionist reading treats persons as ends-in-themselves with inviolable dignity. Kantian deontology (which grounds abolition here) categorically rejects treating persons as mere means.

narrative_ontology:cs_axiom_contradiction(murder_forfeits_moral_status, inalienability_categorical).
narrative_ontology:cs_axiom_contradiction(inalienability_categorical, murder_forfeits_moral_status).
narrative_ontology:cs_axiom_contradiction(deterrence_empirically_contingent_legitimacy, inalienability_categorical).
narrative_ontology:cs_axiom_contradiction(inalienability_categorical, deterrence_empirically_contingent_legitimacy).
narrative_ontology:cs_story_uid(state_killing_authority_contradictions, '48c48b97-f129-44d4-a12f-683298d52f90').
narrative_ontology:cs_created_at('48c48b97-f129-44d4-a12f-683298d52f90', '').
