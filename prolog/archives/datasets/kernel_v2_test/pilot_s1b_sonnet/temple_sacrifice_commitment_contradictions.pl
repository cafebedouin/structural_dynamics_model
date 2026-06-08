% Axiom contradictions for kernel: temple_sacrifice_commitment
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% study_as_exercise↔performance_only: Study-as-exercise holds that intellectual engagement alone can occupy a divine command; performance-only holds that material instantiation is definitional to the command. No single framework can hold both: either study suffices or it does not.
% symbolic_transformation↔performance_only: Symbolic transformation claims rabbinic authority to redefine the content of divine commands; performance-only denies such authority exists. These are mutually exclusive claims about interpretive legitimacy.

narrative_ontology:cs_axiom_contradiction(study_intrinsically_fulfills_commandment, material_instantiation_required).
narrative_ontology:cs_axiom_contradiction(material_instantiation_required, study_intrinsically_fulfills_commandment).
narrative_ontology:cs_axiom_contradiction(prayer_study_equivalence_to_sacrifice, material_instantiation_required).
narrative_ontology:cs_axiom_contradiction(material_instantiation_required, prayer_study_equivalence_to_sacrifice).
