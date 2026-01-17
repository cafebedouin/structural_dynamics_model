% --- 1. Entities & Intervals ---
entity(flow_network_system, process).
entity(capacity_bounds, group).
entity(augmenting_paths, group).
entity(residual_graph_scaffold, scaffold).
entity(min_cut_scaffold, scaffold).

interval(optimization_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(network_initialization, initialization, 0, [actor(algorithm), property(directed_graph)]).
event(path_augmentation, processing, 4, [action(ford_fulkerson_iteration), state(flow_increase)]).
event(saturation_event, update, 7, [action(edge_capacity_reached), property(bottleneck_formation)]).
event(maximal_convergence, termination, 10, [condition(no_more_augmenting_paths)]).

omega_variable(omega_1, empirical, exact_computational_cost_for_irrational_capacities).
omega_variable(omega_2, conceptual, definition_of_optimal_path_selection_bfs_vs_dfs).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(flow_conservation_rule, mountain).
constraint_metric(flow_conservation_rule, intensity, 0.05).
constraint_metric(flow_conservation_rule, suppression_requirement, 0.05).
constraint_metric(flow_conservation_rule, snap_back_potential, 0.00).
constraint_metric(flow_conservation_rule, extractiveness, 0.02).

constraint_claim(edge_capacity_limit, rope).
constraint_metric(edge_capacity_limit, intensity, 0.30).
constraint_metric(edge_capacity_limit, suppression_requirement, 0.12).
constraint_metric(edge_capacity_limit, snap_back_potential, 0.05).
constraint_metric(edge_capacity_limit, extractiveness, 0.25).

constraint_claim(path_finding_inefficiency, tangled_rope).
constraint_metric(path_finding_inefficiency, intensity, 0.55).
constraint_metric(path_finding_inefficiency, suppression_requirement, 0.35).
constraint_metric(path_finding_inefficiency, snap_back_potential, 0.40).
constraint_metric(path_finding_inefficiency, extractiveness, 0.50).

constraint_claim(bottleneck_obstruction, noose).
constraint_metric(bottleneck_obstruction, intensity, 0.95).
constraint_metric(bottleneck_obstruction, suppression_requirement, 0.85).
constraint_metric(bottleneck_obstruction, snap_back_potential, 0.90).
constraint_metric(bottleneck_obstruction, extractiveness, 0.92).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_conservation_laws_as_invariant).
affects_constraint(rec_01, flow_conservation_rule).

recommendation(rec_02, maintain_capacity_bounds_to_prevent_network_overflow).
affects_constraint(rec_02, edge_capacity_limit).

recommendation(rec_03, reform_search_logic_via_edmonds_karp_bfs_scaffolding).
affects_constraint(rec_03, path_finding_inefficiency).

recommendation(rec_04, cut_bottleneck_impact_via_residual_graph_scaffolding).
affects_constraint(rec_04, bottleneck_obstruction).

veto_actor(sparse_graph_limitation).
veto_exposed(sparse_graph_limitation, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.80, 0.50, 0.40, 0.60]).
measurement(10, [0.30, 0.95, 0.99, 0.10]).

% --- 6. Intent Evidence ---
intent_alternative(dinic_algorithm, faster_level_graph_processing).
intent_alternative(push_relabel_method, local_vs_global_update_efficiency).
intent_beneficiary(network_engineers, maximization_of_throughput).
intent_power_delta(source_vs_sink, total_systemic_dependency_on_min_cut_edges).
