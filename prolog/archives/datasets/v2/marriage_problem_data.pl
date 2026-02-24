% --- 1. Entities & Intervals ---
entity(stable_matching_process, process).
entity(suitor_pool, group).
entity(reviewer_pool, group).
entity(proposer_engagement, scaffold).
entity(rejection_buffer, scaffold).

interval(problem_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(proposal_phase_1, proposal, 1, [actor(suitor), recipient(reviewer)]).
event(tentative_acceptance, holding, 3, [state(deferred_acceptance)]).
event(rejection_cycle, update, 5, [action(next_preference)]).
event(stable_convergence, termination, 10, [condition(no_blocking_pairs)]).

omega_variable(omega_1, conceptual, true_preference_ordering_completeness).
omega_variable(omega_2, preference, suitor_optimal_vs_reviewer_optimal_bias).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(deferred_acceptance_algorithm, rope).
constraint_metric(deferred_acceptance_algorithm, intensity, 0.40).
constraint_metric(deferred_acceptance_algorithm, suppression_requirement, 0.12).
constraint_metric(deferred_acceptance_algorithm, snap_back_potential, 0.05).
constraint_metric(deferred_acceptance_algorithm, extractiveness, 0.25).

constraint_claim(blocking_pair_instability, tangled_rope).
constraint_metric(blocking_pair_instability, intensity, 0.70).
constraint_metric(blocking_pair_instability, suppression_requirement, 0.35).
constraint_metric(blocking_pair_instability, snap_back_potential, 0.60).
constraint_metric(blocking_pair_instability, extractiveness, 0.55).

constraint_claim(one_sided_optimality, mountain).
constraint_metric(one_sided_optimality, intensity, 0.10).
constraint_metric(one_sided_optimality, suppression_requirement, 0.02).
constraint_metric(one_sided_optimality, snap_back_potential, 0.00).
constraint_metric(one_sided_optimality, extractiveness, 0.04).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_gale_shapley_standard_for_stability).
affects_constraint(rec_01, deferred_acceptance_algorithm).

recommendation(rec_02, reform_preference_submission_to_allow_indifference).
affects_constraint(rec_02, blocking_pair_instability).

veto_actor(unmatched_participant).
veto_exposed(unmatched_participant, rec_01).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.90, 0.10, 0.20, 0.30]).
measurement(10, [0.40, 0.95, 0.85, 0.90]).

% --- 6. Intent Evidence ---
intent_alternative(random_assignment, high_instability).
intent_beneficiary(proposers, optimal_available_match).
intent_power_delta(proposer_vs_reviewer, high_proposer_advantage).
