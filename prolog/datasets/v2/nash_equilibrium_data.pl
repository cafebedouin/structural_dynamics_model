% 1. ENTITIES & INTERVALS
entity(non_cooperative_game, system).
entity(strategy_profile_space, manifold).
entity(best_response_path, sensor_path).
entity(mixed_strategy_simplex, scaffold).

interval(iteration_to_convergence, 0, 1000).
interval(payoff_evaluation, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, strategy_selection, 0, [player_count, n]).
event(e2, equilibrium_attainment, 450, [deviation_gain, 0_0]).

% Omega Variable: Conceptual (The assumption of perfect rationality in agents)
omega_variable(omega_c1, conceptual, alignment_of_mathematical_rationality_with_human_behavior).

% Omega Variable: Preference (Selection criteria when multiple equilibria exist)
omega_variable(omega_p1, preference, focal_point_selection_in_coordination_games).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Existence Theorem (The Mountain)
% Nash proved that every finite game has at least one equilibrium in mixed strategies.
constraint_claim(fixed_point_existence, mountain).
constraint_metric(fixed_point_existence, intensity, 1.0).
constraint_metric(fixed_point_existence, suppression_requirement, 0.05).
constraint_metric(fixed_point_existence, snap_back_potential, 0.0).
constraint_metric(fixed_point_existence, extractiveness, 0.01).

% The Best-Response Cyclic Dynamics (The Tangled Rope)
% The process where players adjust strategies based on others, potentially creating loops.
% Extractiveness at 0.50 triggers a Reform recommendation for stability analysis.
constraint_claim(best_response_interdependence, tangled_rope).
constraint_metric(best_response_interdependence, intensity, 0.75).
constraint_metric(best_response_interdependence, suppression_requirement, 0.35).
constraint_metric(best_response_interdependence, snap_back_potential, 0.60).
constraint_metric(best_response_interdependence, extractiveness, 0.50).

% The Social Sub-Optimality (The Snare)
% The equilibrium (e.g., in Prisoner's Dilemma) often yields worse outcomes for all than cooperation.
% Extractiveness at 0.82 requires the 'mixed_strategy_simplex' scaffold to navigate.
constraint_claim(pareto_inefficiency_trap, snare).
constraint_metric(pareto_inefficiency_trap, intensity, 0.90).
constraint_metric(pareto_inefficiency_trap, suppression_requirement, 0.70).
constraint_metric(pareto_inefficiency_trap, snap_back_potential, 0.10).
constraint_metric(pareto_inefficiency_trap, extractiveness, 0.82).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.50 extractiveness) triggers Reform
recommendation(rec_01, implement_evolutionary_stable_strategy_check).
affects_constraint(rec_01, best_response_interdependence).

% Snare (0.82 extractiveness) triggers Cut
% Utilizing 'mixed_strategy_simplex' scaffold to find alternative probabilistic stability.
recommendation(rec_02, introduce_external_binding_contracts_to_shift_equilibrium).
affects_constraint(rec_02, pareto_inefficiency_trap).

veto_actor(kakutani_fixed_point_limit).
veto_exposed(kakutani_fixed_point_limit, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency is restricted by others' choices; Stability is high at equilibrium; Utility is often sub-optimal.
measurement(0, [0.80, 0.20, 0.70, 0.50]).
measurement(1000, [0.30, 1.00, 0.40, 0.90]).

% 6. INTENT EVIDENCE
% Alternative: Pareto Optimality (Discarded: lacks individual incentive stability)
% Beneficiaries: Economists, Evolutionary Biologists, and AI Multi-agent System Designers
% Power Delta: Mutual Defection (Individual Gain vs Collective Loss)
intent_evidence(unilateral_deviation_penalty, high_delta).
