% Axiom contradictions for kernel: legitimacy_of_imposed_practice
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% exogenous_override_reading↔endogenous_climb_reading: Exogenous reading holds that state decree authority is sufficient for practice displacement regardless of internalization; endogenous reading holds that displacement requires bottom-up adoption and internalization. No single framework can hold both: either decree suffices (exogenous) or it does not (endogenous).

narrative_ontology:cs_axiom_contradiction(decree_sufficiency_without_internalization, endogenous_adoption_pathway_sufficient).
narrative_ontology:cs_axiom_contradiction(endogenous_adoption_pathway_sufficient, decree_sufficiency_without_internalization).
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice_contradictions, 'daa6fd73-16d8-4b28-b775-98bd54439ce9').
narrative_ontology:cs_created_at('daa6fd73-16d8-4b28-b775-98bd54439ce9', '').
