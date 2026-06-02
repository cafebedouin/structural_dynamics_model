% Axiom contradictions for kernel: end_of_life_decision_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.
:- multifile narrative_ontology:cs_contradiction_of/2.

% autonomy_reading↔sanctity_reading: Autonomy reading holds individual will as sovereign over life/death decisions; sanctity reading holds life value as independent of and superior to individual will. No coherent framework can simultaneously hold both that individual consent makes intentional killing permissible AND that life's intrinsic value makes intentional killing impermissible regardless of consent.

narrative_ontology:cs_axiom_contradiction(rational_agency_sovereignty_principle, human_life_intrinsic_value).
narrative_ontology:cs_axiom_contradiction(human_life_intrinsic_value, rational_agency_sovereignty_principle).
narrative_ontology:cs_story_uid(end_of_life_decision_authority_contradictions, '92952971-efef-46fd-9d00-c43d082eef9e').
narrative_ontology:cs_contradiction_of(end_of_life_decision_authority_contradictions, end_of_life_decision_authority).
narrative_ontology:cs_created_at('92952971-efef-46fd-9d00-c43d082eef9e', '').
