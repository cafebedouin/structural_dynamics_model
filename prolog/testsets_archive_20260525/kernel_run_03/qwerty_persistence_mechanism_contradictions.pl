% Axiom contradictions for kernel: qwerty_persistence_mechanism
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% naturalization_reading↔lock_in_reading: Naturalization requires QWERTY to be efficient or efficiency-neutral; lock-in requires it to be inferior to available alternatives. No framework holds both QWERTY-is-adequate and QWERTY-is-inferior simultaneously.

narrative_ontology:cs_axiom_contradiction(dvorak_performance_negligible, path_dependence_explains_persistence).
narrative_ontology:cs_axiom_contradiction(path_dependence_explains_persistence, dvorak_performance_negligible).
