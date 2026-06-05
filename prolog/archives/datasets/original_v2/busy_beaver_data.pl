% --- 1. Entities & Intervals ---
entity(turing_machine_m, process).
entity(state_count_n, group).
entity(tape_alphabet, group).
entity(halting_oracle_scaffold, scaffold).
entity(heuristic_bound_scaffold, scaffold).

interval(search_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(machine_definition, initialization, 0, [actor(radò), property(finite_states)]).
event(execution_cycle, processing, 4, [action(state_transition), state(active_computation)]).
event(halting_detection, monitoring, 7, [action(step_counting)]).
event(non_computability_impact, termination, 10, [condition(halting_problem_ceiling)]).

omega_variable(omega_1, conceptual, growth_rate_definition_beyond_recursive_functions).
omega_variable(omega_2, empirical, exact_value_of_bb_5_and_higher).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(finite_alphabet_rigidity, mountain).
constraint_metric(finite_alphabet_rigidity, intensity, 0.03).
constraint_metric(finite_alphabet_rigidity, suppression_requirement, 0.01).
constraint_metric(finite_alphabet_rigidity, snap_back_potential, 0.00).
constraint_metric(finite_alphabet_rigidity, extractiveness, 0.02).

constraint_claim(state_limitation_standard, rope).
constraint_metric(state_limitation_standard, intensity, 0.33).
constraint_metric(state_limitation_standard, suppression_requirement, 0.11).
constraint_metric(state_limitation_standard, snap_back_potential, 0.05).
constraint_metric(state_limitation_standard, extractiveness, 0.22).

constraint_claim(computational_brute_force, tangled_rope).
constraint_metric(computational_brute_force, intensity, 0.58).
constraint_metric(computational_brute_force, suppression_requirement, 0.35).
constraint_metric(computational_brute_force, snap_back_potential, 0.40).
constraint_metric(computational_brute_force, extractiveness, 0.62).

constraint_claim(halting_limit_noose, snare).
constraint_metric(halting_limit_noose, intensity, 0.99).
constraint_metric(halting_limit_noose, suppression_requirement, 0.95).
constraint_metric(halting_limit_noose, snap_back_potential, 0.90).
constraint_metric(halting_limit_noose, extractiveness, 0.98).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_strict_adherence_to_turing_completeness).
affects_constraint(rec_01, state_limitation_standard).

recommendation(rec_02, reform_search_strategies_using_macro_machine_reduction).
affects_constraint(rec_02, computational_brute_force).

recommendation(rec_03, cut_reliance_on_computable_bounds_via_non_constructive_logic).
affects_constraint(rec_03, halting_limit_noose).

veto_actor(godel_incompleteness_limit).
veto_exposed(godel_incompleteness_limit, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.90, 0.40, 0.50, 0.30]).
measurement(10, [0.01, 0.99, 0.10, 0.05]).

% --- 6. Intent Evidence ---
intent_alternative(recursive_functions, bounded_growth_limit).
intent_alternative(ackermann_function, slower_than_bb_growth).
intent_beneficiary(computational_complexity_theorists, mapping_undecidability_frontiers).
intent_power_delta(provability_vs_truth, total_dominance_of_non_computable_growth).
