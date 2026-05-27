% Axiom contradictions for kernel: woman_female_category
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% sex_biology_reading↔gender_identity_reading: Sex-biology reading holds category membership is observer-independent biological fact; gender-identity reading holds category membership is subject-dependent self-identification. No single framework can hold both 'membership is independent of subject's claim' and 'membership is constituted by subject's claim' as simultaneously true for the same category.

narrative_ontology:cs_axiom_contradiction(biological_sex_constitutes_woman_category, gender_identity_constitutive_of_personhood).
narrative_ontology:cs_axiom_contradiction(gender_identity_constitutive_of_personhood, biological_sex_constitutes_woman_category).
narrative_ontology:cs_story_uid(woman_female_category_contradictions, '3af3539e-11a0-4041-9fa0-e094a0746022').
narrative_ontology:cs_created_at('3af3539e-11a0-4041-9fa0-e094a0746022', '').
