% --- 1. Entities & Intervals ---
entity(stochastic_convergence_process, process).
entity(sample_mean_sequence, group).
entity(expected_value_mu, group).
entity(strong_law_scaffold, scaffold).
entity(weak_law_scaffold, scaffold).
entity(kolmogorov_criterion_scaffold, scaffold).

interval(convergence_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(independent_trial_initialization, initialization, 0, [actor(observer), property(iid_requirement)]).
event(sample_aggregation, collection, 3, [action(arithmetic_mean), state(fluctuation)]).
event(probability_concentration, update, 7, [action(chebyshev_inequality_application), state(variance_reduction)]).
event(almost_sure_convergence, termination, 10, [condition(limit_equals_expectation)]).

omega_variable(omega_1, empirical, exact_sample_size_n_required_for_prespecified_confidence_interval).
omega_variable(omega_2, conceptual, distinction_between_convergence_in_probability_and_almost_surely).
omega_variable(omega_3, preference, utility_threshold_for_accepting_asymptotic_results_in_finite_time_sampling).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Expected Value Invariance (The constant toward which the mean must gravitate)
constraint_claim(expectation_anchor, mountain).
constraint_metric(expectation_anchor, intensity, 0.05).
constraint_metric(expectation_anchor, suppression_requirement, 0.05).
constraint_metric(expectation_anchor, snap_back_potential, 0.00).
constraint_metric(expectation_anchor, extractiveness, 0.02).

% ROPE: Independence Requirement (The tether ensuring variables do not bias the collective mean)
constraint_claim(iid_tether, rope).
constraint_metric(iid_tether, intensity, 0.30).
constraint_metric(iid_tether, suppression_requirement, 0.12).
constraint_metric(iid_tether, snap_back_potential, 0.05).
constraint_metric(iid_tether, extractiveness, 0.28).

% TANGLED ROPE: Finite Mean Assumption (Cases where the mean exists but convergence speed is unstable)
constraint_claim(convergence_rate_friction, tangled_rope).
constraint_metric(convergence_rate_friction, intensity, 0.55).
constraint_metric(convergence_rate_friction, suppression_requirement, 0.35).
constraint_metric(convergence_rate_friction, snap_back_potential, 0.40).
constraint_metric(convergence_rate_friction, extractiveness, 0.52).

% NOOSE: Infinite Expectation/Cauchy Obstruction (Distributions where the law fails completely)
constraint_claim(undefined_mean_noose, noose).
constraint_metric(undefined_mean_noose, intensity, 0.96).
constraint_metric(undefined_mean_noose, suppression_requirement, 0.88).
constraint_metric(undefined_mean_noose, snap_back_potential, 0.92).
constraint_metric(undefined_mean_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_the_anchor_of_expected_value_as_a_mathematical_invariant).
affects_constraint(rec_01, expectation_anchor).

recommendation(rec_02, maintain_strict_independence_verification_in_dataset_construction).
affects_constraint(rec_02, iid_tether).

recommendation(rec_03, reform_asymptotic_assumptions_via_kolmogorov_criterion_scaffold).
affects_constraint(rec_03, convergence_rate_friction).

recommendation(rec_04, cut_the_infinite_variance_trap_via_strong_law_scaffold).
affects_constraint(rec_04, undefined_mean_noose).

veto_actor(non_integrable_distribution).
veto_exposed(non_integrable_distribution, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.80, 0.40, 0.20, 0.50]).
measurement(10, [0.35, 0.99, 0.95, 0.90]).

% --- 6. Intent Evidence ---
intent_alternative(central_limit_theorem, provides_shape_of_distribution_rather_than_just_the_limit).
intent_alternative(ergodic_theorem, general_extension_for_dependent_systems).
intent_beneficiary(actuaries_and_experimentalists, reliable_prediction_of_long_term_averages).
intent_power_delta(asymptotic_certainty_vs_local_volatility, total_statistical_stabilization).
