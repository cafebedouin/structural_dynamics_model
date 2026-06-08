% Axiom contradictions for kernel: catastrophe_proxy_sufficiency
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% simulation_as_proxy_catastrophe_reading↔catastrophe_necessity_reading: Simulation-sufficiency axiom (drills are structurally equivalent to real events for competence maintenance) cannot coexist with catastrophe-necessity axiom (irreducible difference between simulation and reality makes only real events competence-sustaining)
% catastrophe_necessity_reading↔simulation_fidelity_threshold: Catastrophe-necessity axiom (no simulation can substitute for real events) cannot coexist with threshold-conditional axiom (sufficiently high-fidelity simulation crosses into equivalence)

narrative_ontology:cs_axiom_contradiction(simulation_indefinitely_sufficient, irreducible_uncertainty_principle).
narrative_ontology:cs_axiom_contradiction(irreducible_uncertainty_principle, simulation_indefinitely_sufficient).
narrative_ontology:cs_axiom_contradiction(irreducible_uncertainty_principle, simulation_fidelity_sufficiency).
narrative_ontology:cs_axiom_contradiction(simulation_fidelity_sufficiency, irreducible_uncertainty_principle).
