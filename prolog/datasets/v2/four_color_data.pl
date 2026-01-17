% --- 1. Entities & Intervals ---
entity(planar_map_system, process).
entity(graph_nodes, group).
entity(coloring_algorithm, group).
entity(kempe_chain_scaffold, scaffold).
entity(reducible_configuration_scaffold, scaffold).

interval(proof_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(guthrie_conjecture, initialization, 0, [actor(francis_guthrie), property(four_colors_only)]).
event(kempe_false_proof, processing, 2, [action(chain_substitution), state(flawed_logic)]).
event(computer_reduction, update, 7, [action(discretization), actor(appel_haken)]).
event(formal_verification, termination, 10, [condition(coq_proof_completion)]).

omega_variable(omega_1, conceptual, definition_of_non_adjacent_territories_at_points).
omega_variable(omega_2, empirical, exhaustive_list_of_all_unavoidable_configurations).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(planar_topology_rigidity, mountain).
constraint_metric(planar_topology_rigidity, intensity, 0.04).
constraint_metric(planar_topology_rigidity, suppression_requirement, 0.01).
constraint_metric(planar_topology_rigidity, snap_back_potential, 0.00).
constraint_metric(planar_topology_rigidity, extractiveness, 0.02).

constraint_claim(neighbor_exclusion_rule, rope).
constraint_metric(neighbor_exclusion_rule, intensity, 0.32).
constraint_metric(neighbor_exclusion_rule, suppression_requirement, 0.12).
constraint_metric(neighbor_exclusion_rule, snap_back_potential, 0.08).
constraint_metric(neighbor_exclusion_rule, extractiveness, 0.28).

constraint_claim(manual_case_verification, tangled_rope).
constraint_metric(manual_case_verification, intensity, 0.55).
constraint_metric(manual_case_verification, suppression_requirement, 0.38).
constraint_metric(manual_case_verification, snap_back_potential, 0.45).
constraint_metric(manual_case_verification, extractiveness, 0.50).

constraint_claim(unavoidable_set_complexity, noose).
constraint_metric(unavoidable_set_complexity, intensity, 0.92).
constraint_metric(unavoidable_set_complexity, suppression_requirement, 0.82).
constraint_metric(unavoidable_set_complexity, snap_back_potential, 0.88).
constraint_metric(unavoidable_set_complexity, extractiveness, 0.85).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_euler_characteristic_invariants).
affects_constraint(rec_01, planar_topology_rigidity).

recommendation(rec_02, reform_search_via_discharging_procedures).
affects_constraint(rec_02, manual_case_verification).

recommendation(rec_03, cut_human_computational_limits_via_automated_reasoning).
affects_constraint(rec_03, unavoidable_set_complexity).

veto_actor(traditionalist_pure_mathematician).
veto_exposed(traditionalist_pure_mathematician, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.25, 0.15, 0.30]).
measurement(10, [0.20, 0.98, 0.90, 0.95]).

% --- 6. Intent Evidence ---
intent_alternative(five_color_theorem, high_stability_low_utility).
intent_alternative(heeawood_conjecture, genus_specific_expansion).
intent_beneficiary(cartographers, optimal_resource_allocation).
intent_power_delta(computation_vs_intuition, shift_to_algorithmic_validation).
