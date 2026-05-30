% Axiom contradictions for kernel: kami_buddha_ontology
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.
:- multifile narrative_ontology:cs_contradiction_of/2.

% unified_manifestation_reading↔domain_partition_reading: Unified manifestation requires ontological dependence (kami cannot exist without buddha ground); domain partition requires ontological independence (each system operates in its domain regardless of the other). No single framework can hold both kami-as-dependent-trace AND kami-as-independent-domain-entity simultaneously.

narrative_ontology:cs_axiom_contradiction(buddha_nature_ontological_priority, purity_impurity_ontological_incommensurability).
narrative_ontology:cs_axiom_contradiction(purity_impurity_ontological_incommensurability, buddha_nature_ontological_priority).
narrative_ontology:cs_story_uid(kami_buddha_ontology_contradictions, '8be502f9-6953-4976-9b78-8493d403424a').
narrative_ontology:cs_contradiction_of(kami_buddha_ontology_contradictions, kami_buddha_ontology).
narrative_ontology:cs_created_at('8be502f9-6953-4976-9b78-8493d403424a', '').
