% Axiom contradictions for kernel: personhood_boundary
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% conception_reading↔birth_reading: Conception reading's axiom (moral status is intrinsic to human organism from fertilization) is mutually exclusive with birth reading's axiom (moral status requires independent biological existence or social recognition). No coherent framework can hold both: if personhood is intrinsic at conception, it cannot also require birth; if it requires birth, it cannot be intrinsic at conception.
% conception_reading↔viability_reading: Conception reading's axiom (personhood is binary and present from fertilization) is mutually exclusive with viability reading's axiom (personhood emerges gradually with developmental capacity). No framework can hold both: if personhood is present from conception, it cannot emerge later; if it emerges at viability, it was not present earlier.

narrative_ontology:cs_axiom_contradiction(genetic_continuity_personhood, bodily_autonomy_foundational).
narrative_ontology:cs_axiom_contradiction(bodily_autonomy_foundational, genetic_continuity_personhood).
narrative_ontology:cs_axiom_contradiction(genetic_continuity_personhood, fetal_capacity_for_independence_threshold).
narrative_ontology:cs_axiom_contradiction(fetal_capacity_for_independence_threshold, genetic_continuity_personhood).
