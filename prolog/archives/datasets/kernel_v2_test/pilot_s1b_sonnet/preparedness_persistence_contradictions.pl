% Axiom contradictions for kernel: preparedness_persistence
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% husk_reading↔competence_reading: Husk reading holds that symbolic continuity with 1953 trauma is sufficient for preparedness (ritual performance equals readiness). Competence reading holds that only exercised capability constitutes preparedness (performance without skill is hollow). No single framework can hold both: either the ritual is the substance or the ritual is merely a vehicle for skill maintenance.

narrative_ontology:cs_axiom_contradiction(memorial_sufficiency_for_preparedness, preparedness_requires_exercised_competence).
narrative_ontology:cs_axiom_contradiction(preparedness_requires_exercised_competence, memorial_sufficiency_for_preparedness).
