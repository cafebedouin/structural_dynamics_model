% --- 1. Entities & Intervals ---
entity(root_finding_process, process).
entity(initial_guess_x0, group).
entity(tangent_line_projection, group).
entity(convergence_basin, group).
entity(differentiability_scaffold, scaffold).
entity(second_order_scaffold, scaffold).

interval(iteration_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(initialization, setup, 0, [actor(user), property(starting_point)]).
event(derivative_calculation, processing, 2, [action(slope_determination), state(linear_approximation)]).
event(iteration_step, update, 5, [action(x_intercept_mapping), property(quadratic_convergence)]).
event(divergence_event, conflict, 8, [condition(zero_derivative_slope)]).
event(root_stabilization, termination, 10, [condition(tolerance_threshold_met)]).

omega_variable(omega_1, conceptual, definition_of_optimal_starting_guess_for_fractal_basins).
omega_variable(omega_2, empirical, existence_of_discontinuities_in_higher_order_derivatives).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(local_linearity_assumption, mountain).
constraint_metric(local_linearity_assumption, intensity, 0.05).
constraint_metric(local_linearity_assumption, suppression_requirement, 0.04).
constraint_metric(local_linearity_assumption, snap_back_potential, 0.00).
constraint_metric(local_linearity_assumption, extractiveness, 0.02).

constraint_claim(differentiability_requirement, rope).
constraint_metric(differentiability_requirement, intensity, 0.30).
constraint_metric(differentiability_requirement, suppression_requirement, 0.12).
constraint_metric(differentiability_requirement, snap_back_potential, 0.05).
constraint_metric(differentiability_requirement, extractiveness, 0.25).

constraint_claim(starting_point_sensitivity, tangled_rope).
constraint_metric(starting_point_sensitivity, intensity, 0.62).
constraint_metric(starting_point_sensitivity, suppression_requirement, 0.38).
constraint_metric(starting_point_sensitivity, snap_back_potential, 0.45).
constraint_metric(starting_point_sensitivity, extractiveness, 0.55).

constraint_claim(stationary_point_failure, noose).
constraint_metric(stationary_point_failure, intensity, 0.95).
constraint_metric(stationary_point_failure, suppression_requirement, 0.85).
constraint_metric(stationary_point_failure, snap_back_potential, 0.92).
constraint_metric(stationary_point_failure, extractiveness, 0.90).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_euclidean_derivative_standards).
affects_constraint(rec_01, differentiability_requirement).

recommendation(rec_02, reform_basin_selection_via_bisection_hybrid_scaffolding).
affects_constraint(rec_02, starting_point_sensitivity).

recommendation(rec_03, cut_divergence_risk_via_second_order_scaffold).
affects_constraint(rec_03, stationary_point_failure).

veto_actor(non_differentiable_function).
veto_exposed(non_differentiable_function, rec_01).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.90, 0.50, 0.30, 0.40]).
measurement(10, [0.40, 0.98, 0.95, 0.10]).

% --- 6. Intent Evidence ---
intent_alternative(bisection_method, guaranteed_convergence_low_speed).
intent_alternative(secant_method, avoids_explicit_differentiation).
intent_beneficiary(numerical_analysts, high_speed_polynomial_resolution).
intent_power_delta(quadratic_convergence_vs_initial_proximity, high_speed_sensitivity_tradeoff).
