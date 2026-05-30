% Axiom contradictions for kernel: preparedness_retention
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.
:- multifile narrative_ontology:cs_contradiction_of/2.

% husk_reading↔competence_reading: Husk reading holds that symbolic continuity (memorial, routine) is sufficient for preparedness retention; competence reading holds that preparedness requires active validation and knowledge refresh independent of symbolic practice. No framework can hold both: either symbolic fidelity preserves capability (husk) or capability requires continuous active testing (competence). The 1953 case ('warnings mistaken for routine') suggests husk failure, but post-1953 regime institutionalized both readings simultaneously without resolving which mechanism actually retains preparedness.

narrative_ontology:cs_axiom_contradiction(compliance_substitutes_for_competence, institutional_knowledge_transfer_succeeds).
narrative_ontology:cs_axiom_contradiction(institutional_knowledge_transfer_succeeds, compliance_substitutes_for_competence).
narrative_ontology:cs_story_uid(preparedness_retention_contradictions, '1c31ed6e-26ef-4b0a-9555-b3fe93dd3ecc').
narrative_ontology:cs_contradiction_of(preparedness_retention_contradictions, preparedness_retention).
narrative_ontology:cs_created_at('1c31ed6e-26ef-4b0a-9555-b3fe93dd3ecc', '').
