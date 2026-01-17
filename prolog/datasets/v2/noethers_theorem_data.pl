% --- 1. Entities & Intervals ---
entity(continuous_symmetry_system, process).
entity(conservation_law_set, group).
entity(action_functional_manifold, group).
entity(lagrangian_mechanics_pool, group).
entity(coordinate_transformation_scaffold, scaffold).
entity(variational_calculus_scaffold, scaffold).

interval(proof_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(symmetry_definition, initialization, 0, [actor(emmy_noether), property(invariance_under_transformation)]).
event(variation_derivation, processing, 4, [action(integration_by_parts), state(vanishing_divergence)]).
event(current_generation, update, 7, [action(noether_current_construction), result(divergence_free_field)]).
event(charge_stabilization, termination, 10, [condition(time_independent_integral)]).

omega_variable(omega_1, conceptual, definition_of_gauge_symmetry_redundancy_versus_physical_symmetry).
omega_variable(omega_2, empirical, exact_coupling_constants_in_broken_symmetry_regimes).
omega_variable(omega_3, preference, prioritization_of_manifest_covariance_versus_computational_simplicity).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Action Invariance (The unbending requirement for conservation)
constraint_claim(variational_invariance, mountain).
constraint_metric(variational_invariance, intensity, 0.05).
constraint_metric(variational_invariance, suppression_requirement, 0.05).
constraint_metric(variational_invariance, snap_back_potential, 0.0).
constraint_metric(variational_invariance, extractiveness, 0.02).

% ROPE: Differentiability Norms (The tether of smooth coordinate transformations)
constraint_claim(smooth_manifold_continuity, rope).
constraint_metric(smooth_manifold_continuity, intensity, 0.32).
constraint_metric(smooth_manifold_continuity, suppression_requirement, 0.12).
constraint_metric(smooth_manifold_continuity, snap_back_potential, 0.05).
constraint_metric(smooth_manifold_continuity, extractiveness, 0.28).

% TANGLED ROPE: Symmetry Breaking Friction (Complexity of near-symmetries in real systems)
constraint_claim(approximate_symmetry_friction, tangled_rope).
constraint_metric(approximate_symmetry_friction, intensity, 0.58).
constraint_metric(approximate_symmetry_friction, suppression_requirement, 0.35).
constraint_metric(approximate_symmetry_friction, snap_back_potential, 0.40).
constraint_metric(approximate_symmetry_friction, extractiveness, 0.52).

% NOOSE: Discrete Symmetry Obstruction (The limit where continuous calculus fails)
constraint_claim(discrete_transformation_noose, noose).
constraint_metric(discrete_transformation_noose, intensity, 0.96).
constraint_metric(discrete_transformation_noose, suppression_requirement, 0.88).
constraint_metric(discrete_transformation_noose, snap_back_potential, 0.92).
constraint_metric(discrete_transformation_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_the_link_between_symmetry_and_conservation_as_invariant).
affects_constraint(rec_01, variational_invariance).

recommendation(rec_02, maintain_lagrangian_smoothness_for_classical_field_integrity).
affects_constraint(rec_02, smooth_manifold_continuity).

recommendation(rec_03, reform_non_conserved_quantities_via_coordinate_transformation_scaffold).
affects_constraint(rec_03, approximate_symmetry_friction).

recommendation(rec_04, cut_calculus_limitations_via_variational_calculus_scaffold).
affects_constraint(rec_04, discrete_transformation_noose).

veto_actor(quantum_anomaly_observer).
veto_exposed(quantum_anomaly_observer, rec_01).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.90, 0.70, 0.80]).
measurement(10, [0.45, 0.99, 0.98, 0.95]).

% --- 6. Intent Evidence ---
intent_alternative(hamiltonian_symmetry, focuses_on_canonical_transformations).
intent_alternative(ward_identities, quantum_field_theoretic_generalization).
intent_beneficiary(theoretical_physicists, unification_of_geometry_and_physics).
intent_power_delta(mathematical_symmetry_vs_physical_observation, total_predictive_determination).
