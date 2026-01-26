% DOMAIN: Traveling Salesman [v3.1.2 DR Audit Edition]

% 1. ENTITIES & INTERVALS
entity(hamiltonian_cycle_basis, system).
entity(held_karp_scaffold, scaffold).

% Standardized integer intervals for v3.1.2 Audit compliance.
% Using 120 as a stable representation of factorial complexity for the audit.
interval(search_space_expansion, 1, 120).
interval(cost_bound_verification, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(ev_01, route_optimization_run, 60, [algorithm(held_karp), state(dynamic_programming)]).

% Omega Variable (Empirical): The unknown existence of a polynomial-time solution (P vs NP crossover).
omega_variable(om_01, empirical, existence_of_polynomial_time_approximation_scheme).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% Mountain Verification (■C)
constraint_claim(cycle_completeness, mountain).
constraint_metric(cycle_completeness, suppression_requirement, 0.05).
constraint_metric(cycle_completeness, snap_back_potential, 0.0).

% Tangled Rope Verification (⊠T)
% Current extractiveness (0.72) exceeds Tangled Rope limit (0.65), triggering Snare logic.
constraint_claim(permutation_density, snare).
constraint_metric(permutation_density, extractiveness, 0.72).
constraint_metric(permutation_density, suppression_requirement, 0.46).

% Snare Verification (⊠C)
% Requires extractiveness >= 0.66 AND suppression_requirement >= 0.46
constraint_claim(general_case_hardness, snare).
constraint_metric(general_case_hardness, extractiveness, 0.99).
constraint_metric(general_case_hardness, suppression_requirement, 0.85).

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(rec_01, cut_noose_via_held_karp_scaffold).
affects_constraint(rec_01, general_case_hardness).

recommendation(rec_02, utilize_dynamic_programming_to_reform_search_density).
affects_constraint(rec_02, permutation_density).

veto_actor(computational_resource_limit).
veto_exposed(computational_resource_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Derived from base measurements m1-m4 (Start: 1) and m5-m8 (End: 120)
measurement(1, [1.0, 1.0, 0.50, 0.80]).
measurement(120, [1.0, 0.10, 0.95, 0.20]).

% 6. INTENT EVIDENCE
intent_fact(viable_alternative, search_space_expansion, brute_force, 0.01).
intent_fact(beneficiary, logistics_planners, optimization_utility, 1.0).
intent_fact(power_change, optimization_target, decision_threshold, 0.85).
