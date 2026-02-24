% 1. ENTITIES & INTERVALS
entity(universal_turing_machine_u, system).
entity(prefix_free_program_space, manifold).
entity(computation_trace_path, sensor_path).
entity(algorithmic_information_scaffold, scaffold).

interval(program_length, 1, infinity).
interval(halting_probability_sum, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, prefix_free_encoding, 0, [constraint, no_program_is_prefix_of_another]).
event(e2, bit_determination_collapse, 1, [result, n_th_bit_of_omega]).

% Omega Variable: Conceptual (The transition from mathematical certainty to absolute randomness)
omega_variable(omega_c1, conceptual, alignment_of_logical_completeness_with_algorithmic_randomness).

% Omega Variable: Empirical (The inability to compute more than the first few bits of Omega for any UTM)
omega_variable(omega_e1, empirical, computational_horizon_imposed_by_the_busy_beaver_function).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Prefix-Free Invariant (The Mountain)
% The requirement that the set of halting programs is prefix-free; this ensures the sum is <= 1.
% This is the rigid mathematical foundation (Kraft's Inequality).
constraint_claim(prefix_free_topology, mountain).
constraint_metric(prefix_free_topology, intensity, 1.0).
constraint_metric(prefix_free_topology, suppression_requirement, 0.05).
constraint_metric(prefix_free_topology, snap_back_potential, 0.0).
constraint_metric(prefix_free_topology, extractiveness, 0.01).

% The Infinite Summation Entanglement (The Tangled Rope)
% Omega is defined as $\sum 2^{-|p|}$ for all halting programs $p$. The halting of one impacts the total sum.
% Extractiveness at 0.68 triggers a Reform recommendation for algorithmic complexity.
constraint_claim(halting_set_summation, tangled_rope).
constraint_metric(halting_set_summation, intensity, 0.92).
constraint_metric(halting_set_summation, suppression_requirement, 0.45).
constraint_metric(halting_set_summation, snap_back_potential, 0.85).
constraint_metric(halting_set_summation, extractiveness, 0.68).

% The Absolute Uncomputability Snare (The Snare)
% Knowing the first $n$ bits of Omega allows one to solve the Halting Problem for all programs of length $n$.
% This "strangles" any attempt to find a pattern or a generating algorithm.
% Extractiveness at 1.0 requires the 'algorithmic_information_scaffold' to describe its entropy.
constraint_claim(halting_oracle_equivalence, snare).
constraint_metric(halting_oracle_equivalence, intensity, 1.0).
constraint_metric(halting_oracle_equivalence, suppression_requirement, 0.99).
constraint_metric(halting_oracle_equivalence, snap_back_potential, 0.0).
constraint_metric(halting_oracle_equivalence, extractiveness, 1.0).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.68 extractiveness) triggers Reform
recommendation(rec_01, utilize_kolmogorov_complexity_to_measure_bit_irreducibility).
affects_constraint(rec_01, halting_set_summation).

% Snare (1.0 extractiveness) triggers Cut
% Utilizing 'algorithmic_information_scaffold' to accept Omega as a "random" number with max entropy.
recommendation(rec_02, abandon_deterministic_prediction_for_statistical_randomness_analysis).
affects_constraint(rec_02, halting_oracle_equivalence).

veto_actor(turing_halting_undecidability_axiom).
veto_exposed(turing_halting_undecidability_axiom, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (program design) is 1.0; Stability of the bits is absolute, yet unknowable.
measurement(0, [1.00, 1.00, 0.70, 0.95]).
measurement(1, [0.00, 1.00, 1.00, 1.00]).

% 6. INTENT EVIDENCE
% Alternative: Berry Paradox (Discarded: A linguistic paradox, whereas Omega is a formal number)
% Beneficiaries: Theoretical computer scientists and Kolmogorov complexity researchers
% Power Delta: Logic vs Randomness (Mathematical structure vs Algorithmic compression limit)
intent_evidence(ultimate_uncomputability, high_delta).
