% Axiom contradictions for kernel: sacrifice_obligation_continuity
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% study_as_performance↔performance_only: Study-as-performance holds that textual engagement satisfies the divine commandment; performance-only holds that only physical sacrifice satisfies it. No single framework can hold both: accepting one requires rejecting the other's account of what constitutes fulfillment.
% performance_only↔archival_preservation: Performance-only holds sacrifice remains a binding divine obligation; archival-preservation holds it is no longer normatively binding. No coherent framework can hold both: accepting archival reading requires rejecting the claim that the obligation persists.
% study_as_performance↔archival_preservation: Study-as-performance holds study fulfills a binding religious obligation; archival-preservation holds there is no binding obligation to fulfill. No single framework can hold both: one requires normative force, the other denies it.

narrative_ontology:cs_axiom_contradiction(study_constitutes_fulfillment, performance_uniquely_constitutive).
narrative_ontology:cs_axiom_contradiction(performance_uniquely_constitutive, study_constitutes_fulfillment).
narrative_ontology:cs_axiom_contradiction(performance_uniquely_constitutive, obligation_permanently_released).
narrative_ontology:cs_axiom_contradiction(obligation_permanently_released, performance_uniquely_constitutive).
narrative_ontology:cs_axiom_contradiction(study_constitutes_fulfillment, obligation_permanently_released).
narrative_ontology:cs_axiom_contradiction(obligation_permanently_released, study_constitutes_fulfillment).
