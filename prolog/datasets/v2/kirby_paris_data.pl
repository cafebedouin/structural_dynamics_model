% 1. ENTITIES & INTERVALS
entity(goodstein_process, system).
entity(hereditary_base_notation, manifold).
entity(base_shift_path, sensor_path).
entity(epsilon_zero_ordinal, scaffold).

interval(iteration_sequence, 0, 1000000).
interval(convergence_window, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, base_transformation, 1, [base, 2]).
event(e2, explosive_growth, 3, [value, approx_10_to_10_to_15]).

% Omega Variable: Conceptual (The nature of 'truth' for sequences that exceed physical computation)
omega_variable(omega_c1, conceptual, alignment_of_logical_truth_with_transfinite_termination).

% Omega Variable: Empirical (The inability to represent Goodstein numbers in standard memory)
omega_variable(omega_e1, empirical, bit_width_exhaustion_at_low_base_increments).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Hereditary Notation (The Mountain)
% The rule for writing numbers in hereditary base-n notation is the fixed, rigid foundation.
constraint_claim(hereditary_representation, mountain).
constraint_metric(hereditary_representation, intensity, 1.0).
constraint_metric(hereditary_representation, suppression_requirement, 0.05).
constraint_metric(hereditary_representation, snap_back_potential, 0.0).
constraint_metric(hereditary_representation, extractiveness, 0.01).

% The Base-Increment Explosion (The Tangled Rope)
% Replacing base n with n+1 leads to a massive value jump while the ordinal remains the same.
% Extractiveness at 0.65 triggers a Reform recommendation for ordinal tracking.
constraint_claim(exponential_base_shift, tangled_rope).
constraint_metric(exponential_base_shift, intensity, 0.95).
constraint_metric(exponential_base_shift, suppression_requirement, 0.45).
constraint_metric(exponential_base_shift, snap_back_potential, 0.85).
constraint_metric(exponential_base_shift, extractiveness, 0.65).

% The PA Unprovability Barrier (The Snare)
% The proof of termination requires induction up to Epsilon-Zero, which PA cannot perform.
% Extractiveness at 0.99 requires the 'epsilon_zero_ordinal' scaffold to resolve.
constraint_claim(transfinite_induction_limit, snare).
constraint_metric(transfinite_induction_limit, intensity, 0.99).
constraint_metric(transfinite_induction_limit, suppression_requirement, 0.96).
constraint_metric(transfinite_induction_limit, snap_back_potential, 0.01).
constraint_metric(transfinite_induction_limit, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.65 extractiveness) triggers Reform
recommendation(rec_01, map_goodstein_sequence_to_strictly_decreasing_ordinals).
affects_constraint(rec_01, exponential_base_shift).

% Snare (0.99 extractiveness) triggers Cut
% Utilizing 'epsilon_zero_ordinal' scaffold to provide the necessary transfinite strength.
recommendation(rec_02, adopt_second_order_arithmetic_for_termination_proof).
affects_constraint(rec_02, transfinite_induction_limit).

veto_actor(peano_axiom_consistency).
veto_exposed(peano_axiom_consistency, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (internal rules) is 1.0; Stability of the proof within PA is zero.
measurement(0, [1.00, 1.00, 0.80, 0.95]).
measurement(1000000, [0.10, 0.00, 0.99, 0.05]).

% 6. INTENT EVIDENCE
% Alternative: Gentzen's consistency proof (Discarded: applies to PA itself, not specific sequences)
% Beneficiaries: Proof theorists and combinatorists (Gentzen-style analysis)
% Power Delta: Ordinal Rank (Base-n dynamics vs Transfinite order)
intent_evidence(provability_gap, high_delta).
