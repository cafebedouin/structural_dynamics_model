% Axiom contradictions for kernel: sacrifice_commandment
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% study_as_performance↔performance_only: Study-as-performance holds that intellectual engagement fulfills the commandment; performance-only holds that only physical execution fulfills it. No single framework can hold both: either study IS worship or it is NOT worship.

narrative_ontology:cs_axiom_contradiction(study_fulfills_sacrifice_commandment, performance_requires_temple).
narrative_ontology:cs_axiom_contradiction(performance_requires_temple, study_fulfills_sacrifice_commandment).
