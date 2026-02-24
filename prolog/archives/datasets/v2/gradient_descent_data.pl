% --- 1. Entities & Intervals ---
entity(objective_function_manifold, process).
entity(parameter_vector_theta, group).
entity(learning_rate_hyperparameter, group).
entity(momentum_scaffold, scaffold).
entity(adaptive_rate_scaffold, scaffold).

interval(optimization_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(initialization, setup, 0, [actor(user), property(random_weight_assignment)]).
event(gradient_computation, processing, 2, [action(partial_differentiation), state(slope_direction)]).
event(parameter_update, update, 5, [action(subtraction_of_gradient_product), property(loss_reduction)]).
event(local_minimum_trap, conflict, 7, [condition(zero_gradient_non_global)]).
event(convergence_stability, termination, 10, [condition(gradient_norm_below_epsilon)]).

omega_variable(omega_1, empirical, true_global_minimum_coordinate_in_non_convex_landscapes).
omega_variable(omega_2, conceptual, definition_of_optimal_convergence_vs_generalization_tradeoff).
omega_variable(omega_3, preference, risk_tolerance_for_aggressive_learning_rates).

% --- 3. Constraint Claims & Kinetic Metrics ---
% Mountain: Differentiability is the unbending requirement for gradient calculation.
constraint_claim(smoothness_invariance, mountain).
constraint_metric(smoothness_invariance, intensity, 0.05).
constraint_metric(smoothness_invariance, suppression_requirement, 0.04).
constraint_metric(smoothness_invariance, snap_back_potential, 0.00).
constraint_metric(smoothness_invariance, extractiveness, 0.02).

% Rope: The learning rate acts as the primary tether for stability.
constraint_claim(step_size_tether, rope).
constraint_metric(step_size_tether, intensity, 0.30).
constraint_metric(step_size_tether, suppression_requirement, 0.12).
constraint_metric(step_size_tether, snap_back_potential, 0.08).
constraint_metric(step_size_tether, extractiveness, 0.28).

% Tangled Rope: Oscillations in ravines represent a friction-heavy state requiring reform.
constraint_claim(path_oscillation_friction, tangled_rope).
constraint_metric(path_oscillation_friction, intensity, 0.58).
constraint_metric(path_oscillation_friction, suppression_requirement, 0.35).
constraint_metric(path_oscillation_friction, snap_back_potential, 0.40).
constraint_metric(path_oscillation_friction, extractiveness, 0.52).

% Snare: Vanishing gradients in deep architectures create a terminal obstruction.
constraint_claim(gradient_vanishing_noose, snare).
constraint_metric(gradient_vanishing_noose, intensity, 0.96).
constraint_metric(gradient_vanishing_noose, suppression_requirement, 0.88).
constraint_metric(gradient_vanishing_noose, snap_back_potential, 0.92).
constraint_metric(gradient_vanishing_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_differentiability_as_invariant_for_gradient_access).
affects_constraint(rec_01, smoothness_invariance).

recommendation(rec_02, maintain_conservative_learning_bounds_to_prevent_divergence).
affects_constraint(rec_02, step_size_tether).

recommendation(rec_03, reform_trajectory_via_momentum_scaffold).
affects_constraint(rec_03, path_oscillation_friction).

recommendation(rec_04, cut_vanishing_limit_via_adaptive_rate_scaffold).
affects_constraint(rec_04, gradient_vanishing_noose).

veto_actor(non_convex_local_optima).
veto_exposed(non_convex_local_optima, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.95, 0.40, 0.10, 0.60]).
measurement(10, [0.30, 0.98, 0.95, 0.15]).

% --- 6. Intent Evidence ---
intent_alternative(stochastic_gradient_descent, adds_noise_for_escape).
intent_alternative(second_order_methods, utilizes_hessian_for_curvature).
intent_beneficiary(machine_learning_models, optimization_of_cost_functions).
intent_power_delta(hyperparameter_control_vs_objective_complexity, high_systemic_sensitivity).
