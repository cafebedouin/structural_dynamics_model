% --- 1. Entities & Intervals ---
entity(collatz_process, process).
entity(integer_sequence, group).
entity(parity_oracle, scaffold).
entity(stopping_time_scaffold, scaffold).

interval(iteration_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(input_selection, initialization, 0, [actor(user), property(positive_integer)]).
event(parity_branching, processing, 3, [action(apply_3n_plus_1_or_n_div_2)]).
event(orbit_trajectory, observation, 6, [state(chaotic_expansion)]).
event(cycle_trap, termination, 10, [condition(four_two_one_loop)]).

omega_variable(omega_1, empirical, distribution_of_highest_peaks_in_infinite_sets).
omega_variable(omega_2, conceptual, undecidability_status_within_peano_arithmetic).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(deterministic_pathway, mountain).
constraint_metric(deterministic_pathway, intensity, 0.02).
constraint_metric(deterministic_pathway, suppression_requirement, 0.01).
constraint_metric(deterministic_pathway, snap_back_potential, 0.00).
constraint_metric(deterministic_pathway, extractiveness, 0.01).

constraint_claim(parity_alternation_rule, rope).
constraint_metric(parity_alternation_rule, intensity, 0.30).
constraint_metric(parity_alternation_rule, suppression_requirement, 0.10).
constraint_metric(parity_alternation_rule, snap_back_potential, 0.05).
constraint_metric(parity_alternation_rule, extractiveness, 0.20).

constraint_claim(global_convergence_proof, tangled_rope).
constraint_metric(global_convergence_proof, intensity, 0.60).
constraint_metric(global_convergence_proof, suppression_requirement, 0.40).
constraint_metric(global_convergence_proof, snap_back_potential, 0.50).
constraint_metric(global_convergence_proof, extractiveness, 0.55).

constraint_claim(infinite_ascent_prevention, noose).
constraint_metric(infinite_ascent_prevention, intensity, 0.95).
constraint_metric(infinite_ascent_prevention, suppression_requirement, 0.85).
constraint_metric(infinite_ascent_prevention, snap_back_potential, 0.90).
constraint_metric(infinite_ascent_prevention, extractiveness, 0.88).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_algorithmic_rigor_of_parity_checks).
affects_constraint(rec_01, parity_alternation_rule).

recommendation(rec_02, reform_search_parameters_via_probabilistic_models).
affects_constraint(rec_02, global_convergence_proof).

recommendation(rec_03, cut_reliance_on_standard_induction_for_higher_logic_scaffolds).
affects_constraint(rec_03, infinite_ascent_prevention).

veto_actor(counter_example_finder).
veto_exposed(counter_example_finder, rec_02).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.95, 0.20, 0.10, 0.40]).
measurement(10, [0.05, 0.99, 0.05, 0.10]).

% --- 6. Intent Evidence ---
intent_alternative(terras_theorem, partial_density_stability).
intent_alternative(shizuo_kakutani_diversion, academic_stagnation).
intent_beneficiary(number_theorists, discovery_of_new_arithmetic_structures).
intent_power_delta(simplicity_vs_complexity, low_input_high_entropy_output).
