% 1. ENTITIES & INTERVALS
entity(st_petersburg_lottery, system).
entity(expected_value_manifold, manifold).
entity(coin_flip_sequence, sensor_path).
entity(utility_function_log, scaffold).

interval(game_duration, 0, 100).
interval(payout_calculation, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, initial_flip, 1, [outcome, tails]).
event(e2, sequence_termination, 5, [payout, 32]).

% Omega Variable: Conceptual (Defining 'rationality' when expected value is infinite)
omega_variable(omega_c1, conceptual, validity_of_expected_value_as_a_decision_metric).

% Omega Variable: Preference (Risk aversion coefficients for individual actors)
omega_variable(omega_p1, preference, subjective_threshold_for_finite_participation_fee).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Geometric Payout Growth (The Mountain)
% The prize $2^n$ doubles with each flip; the mathematical structure of the series is invariant.
constraint_claim(geometric_progression, mountain).
constraint_metric(geometric_progression, intensity, 1.0).
constraint_metric(geometric_progression, suppression_requirement, 0.05).
constraint_metric(geometric_progression, snap_back_potential, 0.0).
constraint_metric(geometric_progression, extractiveness, 0.01).

% The Divergent Series (The Tangled Rope)
% The sum of probabilities times payouts yields $1/2 + 1/2 + 1/2...$ which is infinite.
% Extractiveness at 0.55 triggers a Reform recommendation for convergent utility.
constraint_claim(infinite_expectation, tangled_rope).
constraint_metric(infinite_expectation, intensity, 0.90).
constraint_metric(infinite_expectation, suppression_requirement, 0.45).
constraint_metric(infinite_expectation, snap_back_potential, 0.60).
constraint_metric(infinite_expectation, extractiveness, 0.55).

% The Infinite Wealth Fallacy (The Snare)
% The assumption that the casino/house has infinite resources to pay out.
% This "strangles" the real-world application and requires the 'utility_function_log' scaffold.
constraint_claim(unbounded_resource_assumption, snare).
constraint_metric(unbounded_resource_assumption, intensity, 0.98).
constraint_metric(unbounded_resource_assumption, suppression_requirement, 0.90).
constraint_metric(unbounded_resource_assumption, snap_back_potential, 0.05).
constraint_metric(unbounded_resource_assumption, extractiveness, 0.95).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.55 extractiveness) triggers Reform
recommendation(rec_01, replace_expected_payout_with_expected_utility_log_scale).
affects_constraint(rec_01, infinite_expectation).

% Snare (0.95 extractiveness) triggers Cut
% Utilizing 'utility_function_log' scaffold to bound the value by diminishing marginal utility.
recommendation(rec_02, impose_finite_wealth_limit_on_lottery_operator).
affects_constraint(rec_02, unbounded_resource_assumption).

veto_actor(bernoulli_utility_limit).
veto_exposed(bernoulli_utility_limit, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (potential gain) is infinite, but Utility (perceived value) remains low.
measurement(0, [1.00, 0.50, 0.10, 0.10]).
measurement(1, [1.00, 0.10, 0.15, 0.05]).

% 6. INTENT EVIDENCE
% Alternative: Median payout analysis (Discarded: ignores the heavy-tail potential)
% Beneficiaries: Economists (Utility Theory) and Actuaries (Risk Management)
% Power Delta: Convergence Gap (Mathematical infinity vs Behavioral finiteness)
intent_evidence(diminishing_marginal_utility, high_delta).
