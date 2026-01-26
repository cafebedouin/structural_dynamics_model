% --- 1. Entities & Intervals ---
entity(strategic_game_interaction, process).
entity(prisoner_pool, group).
entity(legal_authority_pool, group).
entity(tit_for_tat_scaffold, scaffold).
entity(enforceable_contract_scaffold, scaffold).

interval(game_duration, 0, 10).

% --- 2. Events & Omega Variables ---
event(isolation_phase, initialization, 1, [actor(authority), property(zero_communication)]).
event(payoff_disclosure, notification, 3, [actor(authority), property(dominant_strategy_incentive)]).
event(decision_nexus, processing, 6, [action(defect_vs_cooperate), state(simultaneous_choice)]).
event(equilibrium_result, termination, 10, [condition(nash_equilibrium_reached)]).

omega_variable(omega_1, preference, relative_weight_of_personal_utility_vs_mutual_gain).
omega_variable(omega_2, empirical, certainty_of_opponent_reciprocity_in_iterated_scenarios).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Mathematical Dominance of Defection
constraint_claim(rational_self_interest_dominance, mountain).
constraint_metric(rational_self_interest_dominance, intensity, 0.05).
constraint_metric(rational_self_interest_dominance, suppression_requirement, 0.05).
constraint_metric(rational_self_interest_dominance, snap_back_potential, 0.0).
constraint_metric(rational_self_interest_dominance, extractiveness, 0.03).

% ROPE: One-Shot Game Structure
constraint_claim(single_interaction_rigidity, rope).
constraint_metric(single_interaction_rigidity, intensity, 0.30).
constraint_metric(single_interaction_rigidity, suppression_requirement, 0.12).
constraint_metric(single_interaction_rigidity, snap_back_potential, 0.05).
constraint_metric(single_interaction_rigidity, extractiveness, 0.28).

% TANGLED ROPE: Iterative Reputation Friction
constraint_claim(reputational_shadow_friction, tangled_rope).
constraint_metric(reputational_shadow_friction, intensity, 0.55).
constraint_metric(reputational_shadow_friction, suppression_requirement, 0.35).
constraint_metric(reputational_shadow_friction, snap_back_potential, 0.40).
constraint_metric(reputational_shadow_friction, extractiveness, 0.52).

% NOOSE: Suboptimal Nash Equilibrium
constraint_claim(mutual_defection_trap, snare).
constraint_metric(mutual_defection_trap, intensity, 0.90).
constraint_metric(mutual_defection_trap, suppression_requirement, 0.80).
constraint_metric(mutual_defection_trap, snap_back_potential, 0.85).
constraint_metric(mutual_defection_trap, extractiveness, 0.88).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_rational_dominance_in_non_iterated_frameworks).
affects_constraint(rec_01, rational_self_interest_dominance).

recommendation(rec_02, maintain_strict_isolation_to_preserve_dilemma_purity).
affects_constraint(rec_02, single_interaction_rigidity).

recommendation(rec_03, reform_interaction_frequency_via_tit_for_tat_scaffolding).
affects_constraint(rec_03, reputational_shadow_friction).

recommendation(rec_04, cut_the_defection_trap_via_enforceable_contract_scaffolding).
affects_constraint(rec_04, mutual_defection_trap).

veto_actor(rational_egoist).
veto_exposed(rational_egoist, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.90, 0.80, 0.50, 0.70]).
measurement(10, [0.10, 0.95, 0.20, 0.05]).

% --- 6. Intent Evidence ---
intent_alternative(pareto_optimal_cooperation, mathematically_unstable).
intent_alternative(stag_hunt, coordination_vs_conflict_variant).
intent_beneficiary(collectivists, shift_toward_long_term_reciprocity).
intent_power_delta(individual_vs_collective, high_utility_gap_in_non_cooperative_equilibrium).
