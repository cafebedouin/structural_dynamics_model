% --- 1. Entities & Intervals ---
entity(hamiltonian_path_problem, process).
entity(vertex_set_v, group).
entity(edge_set_e, group).
entity(search_space, group).
entity(dynamic_programming_scaffold, scaffold).
entity(heuristic_pruning_scaffold, scaffold).

interval(calculation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(graph_input, initialization, 0, [actor(user), property(adjacency_matrix)]).
event(permutation_exploration, processing, 3, [action(factorial_traversal), state(combinatorial_explosion)]).
event(subproblem_memoization, update, 6, [action(bellman_held_karp), property(exponential_complexity)]).
event(search_termination, termination, 10, [condition(path_found_or_exhausted)]).

omega_variable(omega_1, empirical, p_vs_np_resolution_status).
omega_variable(omega_2, conceptual, definition_of_efficiency_in_sub_exponential_approximation).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(graph_connectivity_invariance, mountain).
constraint_metric(graph_connectivity_invariance, intensity, 0.05).
constraint_metric(graph_connectivity_invariance, suppression_requirement, 0.05).
constraint_metric(graph_connectivity_invariance, snap_back_potential, 0.00).
constraint_metric(graph_connectivity_invariance, extractiveness, 0.02).

constraint_claim(vertex_visitation_rule, rope).
constraint_metric(vertex_visitation_rule, intensity, 0.30).
constraint_metric(vertex_visitation_rule, suppression_requirement, 0.12).
constraint_metric(vertex_visitation_rule, snap_back_potential, 0.05).
constraint_metric(vertex_visitation_rule, extractiveness, 0.28).

constraint_claim(backtracking_inefficiency, tangled_rope).
constraint_metric(backtracking_inefficiency, intensity, 0.60).
constraint_metric(backtracking_inefficiency, suppression_requirement, 0.35).
constraint_metric(backtracking_inefficiency, snap_back_potential, 0.45).
constraint_metric(backtracking_inefficiency, extractiveness, 0.55).

constraint_claim(np_complete_ceiling, noose).
constraint_metric(np_complete_ceiling, intensity, 0.98).
constraint_metric(np_complete_ceiling, suppression_requirement, 0.88).
constraint_metric(np_complete_ceiling, snap_back_potential, 0.95).
constraint_metric(np_complete_ceiling, extractiveness, 0.96).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_underlying_topological_constraints).
affects_constraint(rec_01, graph_connectivity_invariance).

recommendation(rec_02, maintain_strict_single_visit_invariants).
affects_constraint(rec_02, vertex_visitation_rule).

recommendation(rec_03, reform_search_via_dynamic_programming_scaffold).
affects_constraint(rec_03, backtracking_inefficiency).

recommendation(rec_04, cut_computational_noose_via_heuristic_pruning_scaffold).
affects_constraint(rec_04, np_complete_ceiling).

veto_actor(brute_force_algorithm).
veto_exposed(brute_force_algorithm, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.90, 0.10, 0.60]).
measurement(10, [0.05, 0.99, 0.02, 0.01]).

% --- 6. Intent Evidence ---
intent_alternative(eulerian_path, polynomial_time_reducibility).
intent_alternative(traveling_salesperson, weighted_optimization_extension).
intent_beneficiary(logistics_planners, route_optimization_limit_mapping).
intent_power_delta(computation_time_vs_input_size, exponential_dominance).
