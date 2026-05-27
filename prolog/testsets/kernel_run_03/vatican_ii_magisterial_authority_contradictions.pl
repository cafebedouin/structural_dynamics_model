% Axiom contradictions for kernel: vatican_ii_magisterial_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% continuity_reading↔rupture_reading: Continuity reading requires that Dignitatis Humanae is reconcilable with Syllabus of Errors (no doctrinal contradiction); rupture reading requires acknowledging the contradiction as doctrinal progress. No single coherent framework can hold both 'DH contradicts prior teaching AND this is legitimate development' and 'DH does not contradict prior teaching because development preserves continuity.'

narrative_ontology:cs_axiom_contradiction(medieval_change_is_natural_evolution, classical_purity_as_sole_standard).
narrative_ontology:cs_axiom_contradiction(classical_purity_as_sole_standard, medieval_change_is_natural_evolution).
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority_contradictions, 'c6300181-e352-4d22-99da-9517a690470e').
narrative_ontology:cs_created_at('c6300181-e352-4d22-99da-9517a690470e', '').
