% --- 1. Entities & Intervals ---
entity(matching_market_engine, process).
entity(proposer_agent_pool, group).
entity(receiver_agent_pool, group).
entity(centralized_clearinghouse_scaffold, scaffold).
entity(preference_revelation_scaffold, scaffold).

interval(matching_cycle, 0, 10).

% --- 2. Events & Omega Variables ---
event(market_thickness_check, observation, 1, [property(participant_density)]).
event(preference_submission, action, 3, [property(rank_order_lists)]).
event(algorithm_execution, processing, 6, [action(deferred_acceptance)]).
event(unstable_matching_emergence, conflict, 8, [property(blocking_pairs)]).
event(matching_finalization, termination, 10, [condition(stable_equilibrium)]).

omega_variable(omega_1, conceptual, definition_of_fairness_in_priority_based_allocation).
omega_variable(omega_2, empirical, true_valuation_of_agents_under_strategic_uncertainty).
omega_variable(omega_3, preference, weight_of_proposer_optima_versus_receiver_optima).

% --- 3. Constraint Claims & Kinetic Metrics ---
% Mountain: Stability is non-negotiable for market survival.
constraint_claim(stability_axiom, mountain).
constraint_metric(stability_axiom, intensity, 0.90).
constraint_metric(stability_axiom, suppression_requirement, 0.05).
constraint_metric(stability_axiom, snap_back_potential, 0.0).
constraint_metric(stability_axiom, extractiveness, 0.04).

% Rope: Incentive compatibility maintains system integrity.
constraint_claim(incentive_alignment, rope).
constraint_metric(incentive_alignment, intensity, 0.45).
constraint_metric(incentive_alignment, suppression_requirement, 0.15).
constraint_metric(incentive_alignment, snap_back_potential, 0.08).
constraint_metric(incentive_alignment, extractiveness, 0.35).

% Tangled Rope: Search friction requires structural reform.
constraint_claim(information_asymmetry_friction, tangled_rope).
constraint_metric(information_asymmetry_friction, intensity, 0.65).
constraint_metric(information_asymmetry_friction, suppression_requirement, 0.30).
constraint_metric(information_asymmetry_friction, snap_back_potential, 0.40).
constraint_metric(information_asymmetry_friction, extractiveness, 0.55).

% Snare: Market congestion leads to systemic collapse.
constraint_claim(congestion_trap, snare).
constraint_metric(congestion_trap, intensity, 0.95).
constraint_metric(congestion_trap, suppression_requirement, 0.75).
constraint_metric(congestion_trap, snap_back_potential, 0.85).
constraint_metric(congestion_trap, extractiveness, 0.90).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_stability_as_market_invariant).
affects_constraint(rec_01, stability_axiom).

recommendation(rec_02, maintain_incentive_alignment_protocols).
affects_constraint(rec_02, incentive_alignment).

recommendation(rec_03, reform_information_access_via_preference_revelation_scaffold).
affects_constraint(rec_03, information_asymmetry_friction).

recommendation(rec_04, cut_congestion_via_centralized_clearinghouse_scaffold).
affects_constraint(rec_04, congestion_trap).

veto_actor(strategic_manipulator).
veto_exposed(strategic_manipulator, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.20, 0.40, 0.45]).
measurement(10, [0.40, 0.98, 0.92, 0.88]).

% --- 6. Intent Evidence ---
intent_alternative(random_serial_dictatorship, lacks_long_term_stability).
intent_alternative(decentralized_bargaining, high_congestion_risk).
intent_beneficiary(proposers, optimal_available_match_attainment).
intent_power_delta(system_clearinghouse_vs_individual_agent, high_efficiency_centralization).
