% 1. ENTITIES & INTERVALS
entity(gambler_capital, system).
entity(stochastic_state_space, manifold).
entity(random_walk_trajectory, sensor_path).
entity(absorbing_barrier_zero, scaffold).

interval(betting_sequence, 0, 5000).
interval(ruin_probability_calc, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, initial_stake, 0, [capital, n_units]).
event(e2, absorption_event, 1240, [capital, 0]).

% Omega Variable: Conceptual (Defining 'fairness' in an infinite time horizon)
omega_variable(omega_c1, conceptual, validity_of_expected_value_vs_ruin_probability).

% Omega Variable: Empirical (The impact of minimum/maximum bet limits on trajectory)
omega_variable(omega_e1, empirical, effect_of_table_limits_on_walk_duration).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Random Walk Dynamics (The Mountain)
% Each step is an independent, identically distributed (i.i.d.) event; the physics of the walk is invariant.
constraint_claim(markovian_random_walk, mountain).
constraint_metric(markovian_random_walk, intensity, 1.0).
constraint_metric(markovian_random_walk, suppression_requirement, 0.05).
constraint_metric(markovian_random_walk, snap_back_potential, 0.0).
constraint_metric(markovian_random_walk, extractiveness, 0.01).

% The Capital Fluctuation (The Tangled Rope)
% The variance of the walk causes local entanglements of "wins" and "losses" that mask the long-term trend.
% Extractiveness at 0.45 triggers a Reform recommendation for risk management.
constraint_claim(variance_entrapment, tangled_rope).
constraint_metric(variance_entrapment, intensity, 0.70).
constraint_metric(variance_entrapment, suppression_requirement, 0.30).
constraint_metric(variance_entrapment, snap_back_potential, 0.55).
constraint_metric(variance_entrapment, extractiveness, 0.45).

% The Absorbing Barrier (The Snare)
% Zero capital is an absorbing state; once reached, the system cannot exit. 
% Extractiveness at 0.95 requires the 'absorbing_barrier_zero' scaffold to analyze ruin probability.
constraint_claim(terminal_ruin_bound, snare).
constraint_metric(terminal_ruin_bound, intensity, 0.99).
constraint_metric(terminal_ruin_bound, suppression_requirement, 0.92).
constraint_metric(terminal_ruin_bound, snap_back_potential, 0.02).
constraint_metric(terminal_ruin_bound, extractiveness, 0.95).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.45 extractiveness) triggers Reform
recommendation(rec_01, implement_kelly_criterion_to_optimize_bet_sizing).
affects_constraint(rec_01, variance_entrapment).

% Snare (0.95 extractiveness) triggers Cut
% Utilizing 'absorbing_barrier_zero' scaffold to define a "stop-loss" exit before ruin.
recommendation(rec_02, redefine_system_boundary_to_exit_above_zero_threshold).
affects_constraint(rec_02, terminal_ruin_bound).

veto_actor(law_of_large_numbers).
veto_exposed(law_of_large_numbers, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (control over bets) remains, but Stability and Resilience drop as capital approaches the zero-barrier.
measurement(0, [0.80, 0.60, 0.50, 0.50]).
measurement(5000, [0.00, 0.00, 0.00, 0.00]).

% 6. INTENT EVIDENCE
% Alternative: Martingale strategy (Discarded: leads to faster ruin due to finite bankroll)
% Beneficiaries: Risk analysts, insurance actuaries, and casino mathematicians
% Power Delta: Capital Asymmetry (Finite player wealth vs Infinite house reserves)
intent_evidence(stochastic_absorption, high_delta).
