% Axiom contradictions for kernel: preparedness_retention
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% husk_reading↔competence_reading: Husk reading holds that institutional continuity IS preparedness (structure equals function); competence reading holds that preparedness requires active maintenance separate from structure (function requires exercise). No single framework can hold both: either the institution's existence proves preparedness, or preparedness must be continuously demonstrated through performance.

narrative_ontology:cs_axiom_contradiction(competence_locus_is_institutional_infrastructure, embodied_knowledge_non_substitutable).
narrative_ontology:cs_axiom_contradiction(embodied_knowledge_non_substitutable, competence_locus_is_institutional_infrastructure).
