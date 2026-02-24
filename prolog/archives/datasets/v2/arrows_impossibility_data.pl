% --- 1. Entities & Intervals ---
entity(social_choice_mechanism, process).
entity(voter_pool, group).
entity(alternative_set, group).
entity(dictator_scaffold, scaffold).
entity(independence_scaffold, scaffold).

interval(aggregation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(preference_profile_input, collection, 1, [actor(voters), property(ordinal_ranking)]).
event(aggregation_attempt, processing, 4, [action(function_application)]).
event(axiom_collision, conflict, 7, [affected(transitivity, independence, non_dictatorship)]).
event(impossibility_convergence, termination, 10, [condition(mathematical_contradiction)]).

omega_variable(omega_1, conceptual, interpersonal_utility_comparability_standard).
omega_variable(omega_2, preference, social_welfare_definition_tradeoff).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(unanimity_condition, mountain).
constraint_metric(unanimity_condition, intensity, 0.05).
constraint_metric(unanimity_condition, suppression_requirement, 0.01).
constraint_metric(unanimity_condition, snap_back_potential, 0.00).
constraint_metric(unanimity_condition, extractiveness, 0.02).

constraint_claim(non_dictatorship_requirement, rope).
constraint_metric(non_dictatorship_requirement, intensity, 0.40).
constraint_metric(non_dictatorship_requirement, suppression_requirement, 0.14).
constraint_metric(non_dictatorship_requirement, snap_back_potential, 0.10).
constraint_metric(non_dictatorship_requirement, extractiveness, 0.30).

constraint_claim(independence_of_irrelevant_alternatives, tangled_rope).
constraint_metric(independence_of_irrelevant_alternatives, intensity, 0.75).
constraint_metric(independence_of_irrelevant_alternatives, suppression_requirement, 0.40).
constraint_metric(independence_of_irrelevant_alternatives, snap_back_potential, 0.55).
constraint_metric(independence_of_irrelevant_alternatives, extractiveness, 0.60).

constraint_claim(transitive_rationality, snare).
constraint_metric(transitive_rationality, intensity, 0.90).
constraint_metric(transitive_rationality, suppression_requirement, 0.55).
constraint_metric(transitive_rationality, snap_back_potential, 0.85).
constraint_metric(transitive_rationality, extractiveness, 0.80).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_unanimity_as_fixed_axiom).
affects_constraint(rec_01, unanimity_condition).

recommendation(rec_02, reform_independence_by_allowing_cardinal_utility).
affects_constraint(rec_02, independence_of_irrelevant_alternatives).

recommendation(rec_03, cut_strict_transitivity_for_acyclic_preferences).
affects_constraint(rec_03, transitive_rationality).

veto_actor(the_dictator).
veto_exposed(the_dictator, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.80, 0.20, 0.30, 0.40]).
measurement(10, [0.10, 0.90, 0.15, 0.05]).

% --- 6. Intent Evidence ---
intent_alternative(borda_count, violates_iia).
intent_alternative(majority_rule, cyclic_instability).
intent_beneficiary(individual_voters, preservation_of_autonomy).
intent_power_delta(system_vs_individual, systemic_inability_to_harmonize).
