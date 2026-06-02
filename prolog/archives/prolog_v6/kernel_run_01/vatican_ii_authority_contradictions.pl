% Axiom contradictions for kernel: vatican_ii_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% composite_overdetermination_reading↔continuity_reading: Composite reading requires accepting that Vatican II's ambiguities encode irresolvable theological contradictions from factional compromise; continuity reading requires accepting that all ambiguities are resolvable through traditional hermeneutics under Holy Spirit guidance. No single framework holds both: either the Council produced genuinely contradictory outputs (composite) or all outputs are harmonizable (continuity).

narrative_ontology:cs_axiom_contradiction(council_texts_encode_factional_compromise, development_preserves_substance).
narrative_ontology:cs_axiom_contradiction(development_preserves_substance, council_texts_encode_factional_compromise).
narrative_ontology:cs_story_uid(vatican_ii_authority_contradictions, '2d1ed570-9a2d-4b2e-b5d5-16b360ca4597').
narrative_ontology:cs_created_at('2d1ed570-9a2d-4b2e-b5d5-16b360ca4597', '').
