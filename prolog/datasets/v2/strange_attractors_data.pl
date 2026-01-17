% --- 1. Entities & Intervals ---
entity(dynamical_system_f, process).
entity(phase_space_topology, group).
entity(nonlinear_trajectories, group).
entity(fractal_dimension_scaffold, scaffold).
entity(lyapunov_exponent_scaffold, scaffold).

interval(evolution_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(initial_condition_input, initialization, 0, [actor(observer), property(sensitive_dependence)]).
event(bifurcation_sequence, processing, 3, [action(period_doubling), state(transition_to_chaos)]).
event(orbit_folding, update, 7, [action(stretching_and_folding), property(non_periodic)]).
event(attractor_stabilization, termination, 10, [condition(global_convergence_to_fractal_set)]).

omega_variable(omega_1, empirical, exact_prediction_of_long_term_state_positions).
omega_variable(omega_2, conceptual, definition_of_boundary_between_stochastic_noise_and_deterministic_chaos).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(deterministic_laws_rigidity, mountain).
constraint_metric(deterministic_laws_rigidity, intensity, 0.05).
constraint_metric(deterministic_laws_rigidity, suppression_requirement, 0.01).
constraint_metric(deterministic_laws_rigidity, snap_back_potential, 0.00).
constraint_metric(deterministic_laws_rigidity, extractiveness, 0.02).

constraint_claim(phase_space_boundedness, rope).
constraint_metric(phase_space_boundedness, intensity, 0.30).
constraint_metric(phase_space_boundedness, suppression_requirement, 0.10).
constraint_metric(phase_space_boundedness, snap_back_potential, 0.05).
constraint_metric(phase_space_boundedness, extractiveness, 0.25).

constraint_claim(initial_state_sensitivity, tangled_rope).
constraint_metric(initial_state_sensitivity, intensity, 0.60).
constraint_metric(initial_state_sensitivity, suppression_requirement, 0.35).
constraint_metric(initial_state_sensitivity, snap_back_potential, 0.50).
constraint_metric(initial_state_sensitivity, extractiveness, 0.55).

constraint_claim(infinite_precision_requirement, noose).
constraint_metric(infinite_precision_requirement, intensity, 0.95).
constraint_metric(infinite_precision_requirement, suppression_requirement, 0.88).
constraint_metric(infinite_precision_requirement, snap_back_potential, 0.92).
constraint_metric(infinite_precision_requirement, extractiveness, 0.90).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_topological_invariance_in_model_mapping).
affects_constraint(rec_01, phase_space_boundedness).

recommendation(rec_02, reform_prediction_models_via_probabilistic_density_functions).
affects_constraint(rec_02, initial_state_sensitivity).

recommendation(rec_03, cut_reliance_on_absolute_trajectories_via_fractal_dimension_scaffolding).
affects_constraint(rec_03, infinite_precision_requirement).

veto_actor(laplaces_demon).
veto_exposed(laplaces_demon, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.95, 0.85, 0.70, 0.60]).
measurement(10, [0.05, 0.98, 0.20, 0.10]).

% --- 6. Intent Evidence ---
intent_alternative(linear_approximation, low_accuracy_high_stability).
intent_alternative(lorenz_system_3d, specific_atmospheric_modeling).
intent_beneficiary(complexity_scientists, mapping_non_linear_dynamics).
intent_power_delta(order_vs_disorder, emergent_geometric_order_from_local_instability).
