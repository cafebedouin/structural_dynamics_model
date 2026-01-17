% --- 1. Entities & Intervals ---
entity(infinite_series_summation, process).
entity(pi_squared_sixths, group).
entity(inverse_squares_pool, group).
entity(harmonic_series_scaffold, scaffold).
entity(euler_sine_product_scaffold, scaffold).

interval(derivation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(mengoli_conjecture, initialization, 0, [actor(pietro_mengoli), property(convergence_observation)]).
event(bernoulli_failure, processing, 2, [actor(jakob_bernoulli), state(divergent_complexity)]).
event(euler_infinite_polynomial_expansion, processing, 5, [action(sine_product_formula), state(coefficient_comparison)]).
event(analytical_verification, termination, 10, [condition(exact_evaluation_pi_sq_6)]).

omega_variable(omega_1, conceptual, definition_of_rigorous_limit_validation_pre_cauchy).
omega_variable(omega_2, empirical, exact_value_of_higher_order_zeta_constants_for_odd_integers).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Convergence Invariance (The series must converge regardless of proof method)
constraint_claim(convergence_stability, mountain).
constraint_metric(convergence_stability, intensity, 0.05).
constraint_metric(convergence_stability, suppression_requirement, 0.05).
constraint_metric(convergence_stability, snap_back_potential, 0.0).
constraint_metric(convergence_stability, extractiveness, 0.02).

% ROPE: Inverse Square Law (The specific structural tether of 1/n^2)
constraint_claim(inverse_square_structure, rope).
constraint_metric(inverse_square_structure, intensity, 0.30).
constraint_metric(inverse_square_structure, suppression_requirement, 0.12).
constraint_metric(inverse_square_structure, snap_back_potential, 0.05).
constraint_metric(inverse_square_structure, extractiveness, 0.31).

% TANGLED ROPE: Summation Calculation Friction (Pre-Euler computational difficulty)
constraint_claim(manual_summation_friction, tangled_rope).
constraint_metric(manual_summation_friction, intensity, 0.58).
constraint_metric(manual_summation_friction, suppression_requirement, 0.38).
constraint_metric(manual_summation_friction, snap_back_potential, 0.45).
constraint_metric(manual_summation_friction, extractiveness, 0.52).

% NOOSE: Infinite Polynomial Assumption (Euler's non-rigorous leap at the time)
constraint_claim(sine_product_rigor_gap, noose).
constraint_metric(sine_product_rigor_gap, intensity, 0.95).
constraint_metric(sine_product_rigor_gap, suppression_requirement, 0.85).
constraint_metric(sine_product_rigor_gap, snap_back_potential, 0.90).
constraint_metric(sine_product_rigor_gap, extractiveness, 0.92).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_convergence_as_invariant_for_p_series_greater_than_one).
affects_constraint(rec_01, convergence_stability).

recommendation(rec_02, maintain_inverse_square_relation_for_harmonic_decay).
affects_constraint(rec_02, inverse_square_structure).

recommendation(rec_03, reform_summation_logic_via_harmonic_series_scaffold).
affects_constraint(rec_03, manual_summation_friction).

recommendation(rec_04, cut_rigor_gap_via_euler_sine_product_scaffold).
affects_constraint(rec_04, sine_product_rigor_gap).

veto_actor(strict_weierstrassian_rigorist).
veto_exposed(strict_weierstrassian_rigorist, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.80, 0.40, 0.10, 0.30]).
measurement(10, [0.40, 0.98, 0.99, 0.92]).

% --- 6. Intent Evidence ---
intent_alternative(taylor_series_expansion, insufficient_for_closed_form).
intent_alternative(weierstrass_factorization_theorem, modern_rigorous_validation).
intent_beneficiary(number_theorists, discovery_of_the_riemann_zeta_function).
intent_power_delta(analytical_intuition_vs_computational_rigor, high_pre_modern_gap).
