% --- 1. Entities & Intervals ---
entity(central_limit_process, process).
entity(independent_random_variables, group).
entity(sample_mean_distribution, group).
entity(gaussian_normal_curve, group).
entity(standard_normalization_scaffold, scaffold).
entity(characteristic_function_scaffold, scaffold).

interval(derivation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(moivre_laplace_initialization, initialization, 0, [actor(de_moivre), property(binomial_convergence)]).
event(summation_of_variables, collection, 3, [action(aggregation), condition(independent_identically_distributed)]).
event(moment_generating_analysis, processing, 6, [action(taylor_expansion), state(higher_order_term_vanishing)]).
event(gaussian_stabilization, termination, 10, [condition(convergence_to_normal_distribution)]).

omega_variable(omega_1, empirical, exact_rate_of_convergence_for_highly_skewed_distributions).
omega_variable(omega_2, conceptual, definition_of_independence_in_weakly_correlated_high_dimensional_systems).
omega_variable(omega_3, preference, acceptable_error_threshold_for_finite_sample_approximations).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Additive Stability (The invariant property that sums of independent effects stabilize)
constraint_claim(additive_stability_invariance, mountain).
constraint_metric(additive_stability_invariance, intensity, 0.05).
constraint_metric(additive_stability_invariance, suppression_requirement, 0.05).
constraint_metric(additive_stability_invariance, snap_back_potential, 0.00).
constraint_metric(additive_stability_invariance, extractiveness, 0.02).

% ROPE: Finite Variance Requirement (The tether ensuring distribution does not drift to infinity)
constraint_claim(finite_variance_tether, rope).
constraint_metric(finite_variance_tether, intensity, 0.30).
constraint_metric(finite_variance_tether, suppression_requirement, 0.12).
constraint_metric(finite_variance_tether, snap_back_potential, 0.05).
constraint_metric(finite_variance_tether, extractiveness, 0.28).

% TANGLED ROPE: Berry-Esseen Friction (The difficulty in measuring the speed of convergence)
constraint_claim(convergence_speed_friction, tangled_rope).
constraint_metric(convergence_speed_friction, intensity, 0.58).
constraint_metric(convergence_speed_friction, suppression_requirement, 0.35).
constraint_metric(convergence_speed_friction, snap_back_potential, 0.40).
constraint_metric(convergence_speed_friction, extractiveness, 0.52).

% NOOSE: Infinite Variance/Fat-Tail Obstruction (Cauchy-style distributions that break the theorem)
constraint_claim(heavy_tail_noose, noose).
constraint_metric(heavy_tail_noose, intensity, 0.96).
constraint_metric(heavy_tail_noose, suppression_requirement, 0.88).
constraint_metric(heavy_tail_noose, snap_back_potential, 0.92).
constraint_metric(heavy_tail_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_the_stability_of_averages_as_a_fundamental_statistical_law).
affects_constraint(rec_01, additive_stability_invariance).

recommendation(rec_02, maintain_variance_finitude_checks_in_experimental_design).
affects_constraint(rec_02, finite_variance_tether).

recommendation(rec_03, reform_error_bounds_via_standard_normalization_scaffold).
affects_constraint(rec_03, convergence_speed_friction).

recommendation(rec_04, cut_the_infinite_variance_limit_via_characteristic_function_scaffold).
affects_constraint(rec_04, heavy_tail_noose).

veto_actor(cauchy_distribution_practitioner).
veto_exposed(cauchy_distribution_practitioner, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.50, 0.30, 0.60]).
measurement(10, [0.35, 0.99, 0.98, 0.92]).

% --- 6. Intent Evidence ---
intent_alternative(law_of_large_numbers, weaker_convergence_without_distribution_shape).
intent_alternative(stable_distributions, extension_to_infinite_variance_cases).
intent_beneficiary(statisticians_and_data_scientists, justification_of_parametric_inference).
intent_power_delta(normal_distribution_vs_arbitrary_noise, total_predictive_dominance_at_scale).
