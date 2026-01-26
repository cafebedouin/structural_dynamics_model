% --- 1. Entities & Intervals ---
entity(goldbach_conjecture_system, process).
entity(even_integer_pool, group).
entity(prime_pair_sum, group).
entity(circle_method_scaffold, scaffold).
entity(sieve_theory_scaffold, scaffold).

interval(verification_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(correspondence_initiation, initialization, 0, [actor(christian_goldbach), property(letter_to_euler)]).
event(vinogradov_theorem, processing, 4, [action(weak_goldbach_progress), state(odd_case_resolution)]).
event(chen_theorem, update, 7, [action(prime_plus_almost_prime), property(p_plus_p2)]).
event(numerical_verification_limit, collection, 9, [property(search_up_to_4_10_18)]).
event(unproven_equilibrium, termination, 10, [condition(missing_analytical_link)]).

omega_variable(omega_1, empirical, distribution_of_primes_in_extremely_large_arithmetic_progressions).
omega_variable(omega_2, conceptual, definition_of_sufficiently_large_for_asymptotic_approximations).
omega_variable(omega_3, preference, weight_of_probabilistic_evidence_versus_formal_deduction).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Primality Invariance (The definition of primes remains constant)
constraint_claim(primality_definition_rigidity, mountain).
constraint_metric(primality_definition_rigidity, intensity, 0.05).
constraint_metric(primality_definition_rigidity, suppression_requirement, 0.05).
constraint_metric(primality_definition_rigidity, snap_back_potential, 0.0).
constraint_metric(primality_definition_rigidity, extractiveness, 0.02).

% ROPE: Even Sum Structure (The 2n = p1 + p2 requirement)
constraint_claim(even_sum_parity_alignment, rope).
constraint_metric(even_sum_parity_alignment, intensity, 0.30).
constraint_metric(even_sum_parity_alignment, suppression_requirement, 0.12).
constraint_metric(even_sum_parity_alignment, snap_back_potential, 0.05).
constraint_metric(even_sum_parity_alignment, extractiveness, 0.32).

% TANGLED ROPE: Sieve Method Friction (Limitations in capturing additive structures)
constraint_claim(sieve_method_efficiency_friction, tangled_rope).
constraint_metric(sieve_method_efficiency_friction, intensity, 0.60).
constraint_metric(sieve_method_efficiency_friction, suppression_requirement, 0.38).
constraint_metric(sieve_method_efficiency_friction, snap_back_potential, 0.45).
constraint_metric(sieve_method_efficiency_friction, extractiveness, 0.55).

% NOOSE: Analytical Proof Obstruction (The gap between distribution and specific sums)
constraint_claim(analytical_distribution_noose, snare).
constraint_metric(analytical_distribution_noose, intensity, 0.98).
constraint_metric(analytical_distribution_noose, suppression_requirement, 0.88).
constraint_metric(analytical_distribution_noose, snap_back_potential, 0.95).
constraint_metric(analytical_distribution_noose, extractiveness, 0.96).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_primality_axioms_as_universal_invariants).
affects_constraint(rec_01, primality_definition_rigidity).

recommendation(rec_02, maintain_parity_consistency_for_binary_goldbach_claims).
affects_constraint(rec_02, even_sum_parity_alignment).

recommendation(rec_03, reform_additive_approaches_via_sieve_theory_scaffold).
affects_constraint(rec_03, sieve_method_efficiency_friction).

recommendation(rec_04, cut_analytical_deadlocks_via_circle_method_scaffold).
affects_constraint(rec_04, analytical_distribution_noose).

veto_actor(strict_computational_finitist).
veto_exposed(strict_computational_finitist, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.60, 0.40, 0.50]).
measurement(10, [0.15, 0.99, 0.10, 0.02]).

% --- 6. Intent Evidence ---
intent_alternative(weak_goldbach_conjecture, proven_subset_of_logic).
intent_alternative(probabilistic_number_theory, heuristic_confidence_generator).
intent_beneficiary(analytic_number_theorists, mapping_the_limits_of_additive_prime_logic).
intent_power_delta(asymptotic_certainty_vs_absolute_proof, high_epistemological_gap).
