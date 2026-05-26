% Axiom contradictions for kernel: preparedness_retention
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% husk_reading↔competence_reading: Husk reading holds that ritual performance cannot preserve tacit operational knowledge across non-catastrophe generations; competence reading holds that structured practice does preserve such knowledge. No single framework can hold both—either drills work or they don't.

narrative_ontology:cs_axiom_contradiction(ceremony_displaces_competence_under_resource_constraint, live_exercise_retains_competence).
narrative_ontology:cs_axiom_contradiction(live_exercise_retains_competence, ceremony_displaces_competence_under_resource_constraint).
