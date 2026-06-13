% Axiom contradictions for kernel: actinide_replenishment_mechanism
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% neutron_star_bombardment_reading↔superheavy_decay_reading: Neutron star reading requires external energy source (companion star) continuously driving transmutation; superheavy decay reading requires primordial element present since stellar formation decaying passively. No framework holds both: either actinides are externally produced (bombardment) or internally sourced (decay), not both.

narrative_ontology:cs_axiom_contradiction(binary_companion_necessity, r_process_superheavy_production_viable).
narrative_ontology:cs_axiom_contradiction(r_process_superheavy_production_viable, binary_companion_necessity).
