% --- 1. Entities & Intervals ---
entity(original_ship, process).
entity(replacement_component, group).
entity(material_storage, group).
entity(reconstruction_scaffold, scaffold).
entity(conceptual_boundary_scaffold, scaffold).

interval(replacement_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(component_decay, initialization, 0, [actor(time), property(structural_entropy)]).
event(replacement_act, processing, 5, [action(substitution), state(gradual_transformation)]).
event(collection_completion, update, 9, [action(storage), property(original_planks)]).
event(paradox_divergence, termination, 10, [condition(dual_identity_collision)]).

omega_variable(omega_1, conceptual, definition_of_persistence_identity_vs_spatio_temporal_continuity).
omega_variable(omega_2, preference, prioritizing_functional_utility_over_historical_authenticity).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(material_continuity, mountain).
constraint_metric(material_continuity, intensity, 0.05).
constraint_metric(material_continuity, suppression_requirement, 0.01).
constraint_metric(material_continuity, snap_back_potential, 0.00).
constraint_metric(material_continuity, extractiveness, 0.02).

constraint_claim(functional_identity, rope).
constraint_metric(functional_identity, intensity, 0.30).
constraint_metric(functional_identity, suppression_requirement, 0.10).
constraint_metric(functional_identity, snap_back_potential, 0.05).
constraint_metric(functional_identity, extractiveness, 0.25).

constraint_claim(total_replacement_logic, tangled_rope).
constraint_metric(total_replacement_logic, intensity, 0.60).
constraint_metric(total_replacement_logic, suppression_requirement, 0.35).
constraint_metric(total_replacement_logic, snap_back_potential, 0.45).
constraint_metric(total_replacement_logic, extractiveness, 0.50).

constraint_claim(dual_existence_coherence, snare).
constraint_metric(dual_existence_coherence, intensity, 0.95).
constraint_metric(dual_existence_coherence, suppression_requirement, 0.88).
constraint_metric(dual_existence_coherence, snap_back_potential, 0.92).
constraint_metric(dual_existence_coherence, extractiveness, 0.90).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_functional_role_as_primary_identifier).
affects_constraint(rec_01, functional_identity).

recommendation(rec_02, reform_identity_criteria_to_allow_for_composite_histories).
affects_constraint(rec_02, total_replacement_logic).

recommendation(rec_03, cut_exclusive_uniqueness_requirement_via_multi_entity_scaffolding).
affects_constraint(rec_03, dual_existence_coherence).

veto_actor(historical_purist).
veto_exposed(historical_purist, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.90, 0.95, 0.80, 0.85]).
measurement(10, [0.30, 0.15, 0.50, 0.05]).

% --- 6. Intent Evidence ---
intent_alternative(mereological_essentialism, identity_fragility).
intent_alternative(four_dimensionalism, temporal_part_unification).
intent_beneficiary(observers, cognitive_categorical_stability).
intent_power_delta(physical_reality_vs_linguistic_convention, high_conceptual_friction).
