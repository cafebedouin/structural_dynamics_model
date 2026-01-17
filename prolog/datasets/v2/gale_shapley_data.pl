% --- 1. Entities & Intervals ---
entity(stable_matching_market, process).
entity(proposer_set, group).
entity(receiver_set, group).
entity(deferred_acceptance_scaffold, scaffold).
entity(preference_list_scaffold, scaffold).

interval(matching_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(proposal_initialization, proposal, 1, [actor(proposers), target(top_preference)]).
event(tentative_holding, state, 4, [condition(deferred_acceptance)]).
event(rejection_cycle, iteration, 7, [action(next_available_preference)]).
event(stable_convergence, termination, 10, [condition(no_blocking_pairs)]).

omega_variable(omega_1, conceptual, truthfulness_incentive_compatibility_for_receivers).
omega_variable(omega_2, preference, social_welfare_weight_proposer_vs_receiver_optima).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(unmatched_stability, mountain).
constraint_metric(unmatched_stability, intensity, 0.05).
constraint_metric(unmatched_stability, suppression_requirement, 0.02).
constraint_metric(unmatched_stability, snap_back_potential, 0.00).
constraint_metric(unmatched_stability, extractiveness, 0.04).

constraint_claim(deferred_acceptance_rule, rope).
constraint_metric(deferred_acceptance_rule, intensity, 0.30).
constraint_metric(deferred_acceptance_rule, suppression_requirement, 0.12).
constraint_metric(deferred_acceptance_rule, snap_back_potential, 0.05).
constraint_metric(deferred_acceptance_rule, extractiveness, 0.28).

constraint_claim(preference_list_rigidity, tangled_rope).
constraint_metric(preference_list_rigidity, intensity, 0.55).
constraint_metric(preference_list_rigidity, suppression_requirement, 0.38).
constraint_metric(preference_list_rigidity, snap_back_potential, 0.45).
constraint_metric(preference_list_rigidity, extractiveness, 0.52).

constraint_claim(blocking_pair_instability, noose).
constraint_metric(blocking_pair_instability, intensity, 0.92).
constraint_metric(blocking_pair_instability, suppression_requirement, 0.82).
constraint_metric(blocking_pair_instability, snap_back_potential, 0.88).
constraint_metric(blocking_pair_instability, extractiveness, 0.85).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_unmatched_stability_as_system_invariant).
affects_constraint(rec_01, unmatched_stability).

recommendation(rec_02, maintain_deferred_acceptance_to_ensure_convergence).
affects_constraint(rec_02, deferred_acceptance_rule).

recommendation(rec_03, reform_preference_submission_via_indifference_handling_scaffold).
affects_constraint(rec_03, preference_list_rigidity).

recommendation(rec_04, cut_blocking_pair_potential_via_centralized_matching_scaffold).
affects_constraint(rec_04, blocking_pair_instability).

veto_actor(strategic_manipulator).
veto_exposed(strategic_manipulator, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.90, 0.20, 0.30, 0.40]).
measurement(10, [0.45, 0.98, 0.85, 0.92]).

% --- 6. Intent Evidence ---
intent_alternative(random_assignment, high_instability_low_utility).
intent_alternative(top_trading_cycles, house_allocation_variant).
intent_beneficiary(proposers, optimal_available_match_extraction).
intent_power_delta(proposer_side_bias, high_utility_asymmetry).
