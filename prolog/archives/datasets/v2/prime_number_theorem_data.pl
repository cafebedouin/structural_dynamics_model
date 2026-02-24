% --- 1. Entities & Intervals ---
entity(prime_counting_function, process).
entity(logarithmic_integral_li, group).
entity(riemann_zeta_function, group).
entity(complex_analysis_scaffold, scaffold).
entity(hadamard_de_la_vallee_poussin_scaffold, scaffold).

interval(proof_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(gauss_legendre_conjecture, initialization, 0, [actor(gauss), property(density_as_1_over_log_n)]).
event(riemann_hypothesis_insertion, processing, 4, [actor(riemann), action(zeta_function_zero_mapping)]).
event(analytical_verification, update, 8, [actor(hadamard_poussin), property(non_vanishing_on_re_s_1)]).
event(asymptotic_convergence, termination, 10, [condition(ratio_approaches_unity)]).

omega_variable(omega_1, empirical, exact_location_of_non_trivial_zeros_beyond_re_s_half).
omega_variable(omega_2, conceptual, definition_of_error_term_optimality_without_rh).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Asymptotic Law (Unbending requirement for prime density)
constraint_claim(asymptotic_distribution_law, mountain).
constraint_metric(asymptotic_distribution_law, intensity, 0.05).
constraint_metric(asymptotic_distribution_law, suppression_requirement, 0.05).
constraint_metric(asymptotic_distribution_law, snap_back_potential, 0.0).
constraint_metric(asymptotic_distribution_law, extractiveness, 0.02).

% ROPE: Logarithmic Approximation Norms
constraint_claim(logarithmic_integral_standard, rope).
constraint_metric(logarithmic_integral_standard, intensity, 0.30).
constraint_metric(logarithmic_integral_standard, suppression_requirement, 0.12).
constraint_metric(logarithmic_integral_standard, snap_back_potential, 0.05).
constraint_metric(logarithmic_integral_standard, extractiveness, 0.31).

% TANGLED ROPE: Error Term Estimation Friction
constraint_claim(error_bound_uncertainty, tangled_rope).
constraint_metric(error_bound_uncertainty, intensity, 0.58).
constraint_metric(error_bound_uncertainty, suppression_requirement, 0.38).
constraint_metric(error_bound_uncertainty, snap_back_potential, 0.45).
constraint_metric(error_bound_uncertainty, extractiveness, 0.52).

% NOOSE: Non-Analytical Proof Constraints (Pre-Elementary Era)
constraint_claim(elementary_proof_impossibility_trap, snare).
constraint_metric(elementary_proof_impossibility_trap, intensity, 0.95).
constraint_metric(elementary_proof_impossibility_trap, suppression_requirement, 0.85).
constraint_metric(elementary_proof_impossibility_trap, snap_back_potential, 0.90).
constraint_metric(elementary_proof_impossibility_trap, extractiveness, 0.92).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_the_asymptotic_limit_as_foundational_prime_logic).
affects_constraint(rec_01, asymptotic_distribution_law).

recommendation(rec_02, maintain_logarithmic_integral_as_primary_density_gauge).
affects_constraint(rec_02, logarithmic_integral_standard).

recommendation(rec_03, reform_error_estimation_via_complex_analysis_scaffold).
affects_constraint(rec_03, error_bound_uncertainty).

recommendation(rec_04, cut_the_analytic_monopoly_via_hadamard_de_la_vallee_poussin_scaffold).
affects_constraint(rec_04, elementary_proof_impossibility_trap).

veto_actor(selberg_erdos_elementary_traditionalist).
veto_exposed(selberg_erdos_elementary_traditionalist, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.80, 0.50, 0.40, 0.60]).
measurement(10, [0.35, 0.98, 0.99, 0.94]).

% --- 6. Intent Evidence ---
intent_alternative(elementary_proof, avoids_complex_analysis_but_increases_combinatorial_friction).
intent_alternative(zeta_zero_density_estimates, refined_error_bounds).
intent_beneficiary(cryptographers, predictability_of_large_prime_distribution).
intent_power_delta(analytic_methods_vs_number_theory_intuition, high_methodological_dominance).
