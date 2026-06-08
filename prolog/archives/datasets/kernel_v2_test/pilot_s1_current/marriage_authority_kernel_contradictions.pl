% Axiom contradictions for kernel: marriage_authority_kernel
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% muslim_shariat_reading↔secular_civil_reading: Shariat reading grounds authority in divine law interpreted by religious scholars (no single coherent framework can hold both 'divine law is supreme' and 'individual constitutional rights are supreme' as simultaneously true foundational axioms)
% hindu_codified_reading↔muslim_shariat_reading: Hindu reading accepts state codification and parliamentary amendment of religious law; Muslim reading rejects state authority to modify Shariat (no framework holds both 'state can revise religious law' and 'religious law is immune to state revision' as simultaneously true)
% christian_canonical_reading↔secular_civil_reading: Canonical reading grounds marriage indissolubility in sacramental theology; secular reading grounds marriage as civil contract dissolvable by mutual consent (no framework holds both 'marriage is ontologically indissoluble' and 'marriage is a revocable contract' as simultaneously true)

narrative_ontology:cs_axiom_contradiction(religious_community_autonomy_in_personal_law, individual_rights_prior_to_community).
narrative_ontology:cs_axiom_contradiction(individual_rights_prior_to_community, religious_community_autonomy_in_personal_law).
narrative_ontology:cs_axiom_contradiction(codified_brahmanical_norm_as_hindu_universal, religious_community_autonomy_in_personal_law).
narrative_ontology:cs_axiom_contradiction(religious_community_autonomy_in_personal_law, codified_brahmanical_norm_as_hindu_universal).
narrative_ontology:cs_axiom_contradiction(marriage_is_sacrament_indissoluble, individual_rights_prior_to_community).
narrative_ontology:cs_axiom_contradiction(individual_rights_prior_to_community, marriage_is_sacrament_indissoluble).
