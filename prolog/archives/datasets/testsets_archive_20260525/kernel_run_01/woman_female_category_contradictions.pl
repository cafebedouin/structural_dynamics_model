% Axiom contradictions for kernel: woman_female_category
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% sex_biology_reading↔gender_identity_reading: No single coherent framework can hold both 'category membership is determined by chromosomal/anatomical observables' and 'category membership is determined by internal self-identification independent of observables' as simultaneously true for the same category in the same context. Accepting one as the category-defining mechanism requires rejecting the other as the category-defining mechanism.

narrative_ontology:cs_axiom_contradiction(chromosomal_sex_determines_legal_category, gender_identity_ontologically_primary).
narrative_ontology:cs_axiom_contradiction(gender_identity_ontologically_primary, chromosomal_sex_determines_legal_category).
