% --- 1. Entities & Intervals ---
entity(universal_turing_machine, process).
entity(bit_string_s, group).
entity(program_p, group).
entity(description_language, group).
entity(shortest_program_scaffold, scaffold).
entity(compression_algorithm_scaffold, scaffold).

interval(computation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(input_string_definition, initialization, 0, [actor(observer), property(binary_sequence)]).
event(program_execution, processing, 4, [action(decompression), state(output_generation)]).
event(length_minimization, update, 7, [action(search_for_shortest_description)]).
event(incomputability_collision, termination, 10, [condition(berry_paradox_ceiling)]).

omega_variable(omega_1, conceptual, choice_of_universal_turing_machine_constant_additive_term).
omega_variable(omega_2, empirical, exact_shortest_program_length_for_complex_strings).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(invariance_theorem_rigidity, mountain).
constraint_metric(invariance_theorem_rigidity, intensity, 0.04).
constraint_metric(invariance_theorem_rigidity, suppression_requirement, 0.01).
constraint_metric(invariance_theorem_rigidity, snap_back_potential, 0.00).
constraint_metric(invariance_theorem_rigidity, extractiveness, 0.02).

constraint_claim(computable_upper_bounds, rope).
constraint_metric(computable_upper_bounds, intensity, 0.32).
constraint_metric(computable_upper_bounds, suppression_requirement, 0.11).
constraint_metric(computable_upper_bounds, snap_back_potential, 0.05).
constraint_metric(computable_upper_bounds, extractiveness, 0.28).

constraint_claim(algorithmic_randomness_detection, tangled_rope).
constraint_metric(algorithmic_randomness_detection, intensity, 0.58).
constraint_metric(algorithmic_randomness_detection, suppression_requirement, 0.35).
constraint_metric(algorithmic_randomness_detection, snap_back_potential, 0.40).
constraint_metric(algorithmic_randomness_detection, extractiveness, 0.52).

constraint_claim(absolute_incomputability_noose, snare).
constraint_metric(absolute_incomputability_noose, intensity, 0.96).
constraint_metric(absolute_incomputability_noose, suppression_requirement, 0.88).
constraint_metric(absolute_incomputability_noose, snap_back_potential, 0.90).
constraint_metric(absolute_incomputability_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_reference_to_fixed_utm_standard).
affects_constraint(rec_01, invariance_theorem_rigidity).

recommendation(rec_02, reform_string_analysis_via_resource_bounded_complexity).
affects_constraint(rec_02, algorithmic_randomness_detection).

recommendation(rec_03, cut_the_search_for_absolute_shortest_programs_via_heuristic_scaffolding).
affects_constraint(rec_03, absolute_incomputability_noose).

veto_actor(halting_oracle).
veto_exposed(halting_oracle, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.40, 0.20, 0.50]).
measurement(10, [0.05, 0.98, 0.15, 0.02]).

% --- 6. Intent Evidence ---
intent_alternative(shannon_entropy, probabilistic_vs_algorithmic_focus).
intent_alternative(lempel_ziv_compression, computable_approximation).
intent_beneficiary(information_theorists, mathematical_definition_of_randomness).
intent_power_delta(provable_compression_vs_inherent_complexity, high_incomputability_gap).
