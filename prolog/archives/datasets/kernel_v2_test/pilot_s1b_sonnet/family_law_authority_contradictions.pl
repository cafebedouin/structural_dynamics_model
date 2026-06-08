% Axiom contradictions for kernel: family_law_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% hindu_dharmashastra_reading↔secular_contractual_reading: Sacramental ontology (marriage as cosmic/ritual bond transcending individual will) cannot coexist with contractual ontology (marriage as revocable agreement between autonomous parties) in a single coherent framework
% muslim_shariat_reading↔christian_canonical_reading: Contractual dissolution as divinely permitted (Quranic talaq) contradicts sacramental indissolubility (Catholic permanence) — no framework holds both as simultaneously valid
% christian_canonical_reading↔secular_contractual_reading: Ecclesiastical authority as necessary for validity (sacrament requires church) contradicts state-only authority (civil registration sufficient) — mutually exclusive sovereignty claims
% parsi_zoroastrian_reading↔secular_contractual_reading: Community-boundary preservation (endogamy as religious duty) contradicts individual autonomy (interfaith marriage as protected right) — incompatible conceptions of legitimate choice

narrative_ontology:cs_axiom_contradiction(marriage_as_sacramental_samskara, individual_autonomy_in_contract).
narrative_ontology:cs_axiom_contradiction(individual_autonomy_in_contract, marriage_as_sacramental_samskara).
narrative_ontology:cs_axiom_contradiction(divine_law_gender_asymmetry, marriage_indissoluble_sacrament).
narrative_ontology:cs_axiom_contradiction(marriage_indissoluble_sacrament, divine_law_gender_asymmetry).
narrative_ontology:cs_axiom_contradiction(marriage_indissoluble_sacrament, individual_autonomy_in_contract).
narrative_ontology:cs_axiom_contradiction(individual_autonomy_in_contract, marriage_indissoluble_sacrament).
narrative_ontology:cs_axiom_contradiction(zoroastrian_identity_by_birth_only, individual_autonomy_in_contract).
narrative_ontology:cs_axiom_contradiction(individual_autonomy_in_contract, zoroastrian_identity_by_birth_only).
