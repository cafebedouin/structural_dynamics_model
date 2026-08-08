% Axiom contradictions for kernel: polaris_document_status
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% authoritative_specification_reading↔fictional_construct_reading: One reading treats the document as binding technical reality requiring organizational capacity and operational infrastructure; the other treats it as constructed artifact where organizational claims are narrative elements. No single framework holds both: either Polaris exists and specifications bind, or it is fiction and specifications are worldbuilding.
% conceptual_framework_reading↔authoritative_specification_reading: One reading brackets organizational reality to extract architectural patterns as epistemic utility independent of implementation; the other requires organizational reality as the ground of specification authority. A framework treating the document as valuable-regardless-of-implementation cannot simultaneously treat organizational existence as necessary for specification legitimacy.

narrative_ontology:cs_axiom_contradiction(operational_standards_body_exists, specification_format_as_narrative_device).
narrative_ontology:cs_axiom_contradiction(specification_format_as_narrative_device, operational_standards_body_exists).
narrative_ontology:cs_axiom_contradiction(pattern_validity_independent_of_instantiation, operational_standards_body_exists).
narrative_ontology:cs_axiom_contradiction(operational_standards_body_exists, pattern_validity_independent_of_instantiation).
