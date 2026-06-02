% Axiom contradictions for kernel: legitimacy_of_imposed_practice
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.
:- multifile narrative_ontology:cs_contradiction_of/2.

% exogenous_override_reading↔endogenous_climb_reading: Exogenous reading holds that state decree authority is sufficient for practice displacement regardless of internalization; endogenous reading holds that displacement requires bottom-up adoption and internalization. No single framework can hold both: either decree suffices (exogenous) or it does not (endogenous).

narrative_ontology:cs_axiom_contradiction(decree_sufficiency_without_internalization, endogenous_adoption_pathway_sufficient).
narrative_ontology:cs_axiom_contradiction(endogenous_adoption_pathway_sufficient, decree_sufficiency_without_internalization).
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice_contradictions, 'b9d92948-6cb9-4f8f-86fa-6754e082d271').
narrative_ontology:cs_contradiction_of(legitimacy_of_imposed_practice_contradictions, legitimacy_of_imposed_practice).
narrative_ontology:cs_created_at('b9d92948-6cb9-4f8f-86fa-6754e082d271', '').
