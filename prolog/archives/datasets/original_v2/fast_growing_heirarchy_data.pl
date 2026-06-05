% 1. ENTITIES & INTERVALS
entity(ordinal_indexed_functions, system).
entity(growth_rate_continuum, manifold).
entity(diagonalization_path, sensor_path).
entity(fundamental_sequence_scaffold, scaffold).

interval(successor_step, 0, 1).
interval(limit_diagonalization, 0, omega).

% 2. EVENTS & OMEGA VARIABLES
event(e1, function_iteration, 0, [base, f_alpha]).
event(e2, transfinite_jump, 1, [index, limit_ordinal_lambda]).

% Omega Variable: Conceptual (The transition from finite iteration to transfinite diagonalization)
omega_variable(omega_c1, conceptual, alignment_of_ordinal_indexing_with_computational_resource_bounds).

% Omega Variable: Empirical (The inability to compute even f_3(3) without reaching astronomical scales)
omega_variable(omega_e1, empirical, physical_memory_limits_on_storing_nested_recursive_calls).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Successor Rule (The Mountain)
% The rule f_{alpha+1}(n) = f_{alpha}^n(n) is the rigid, invariant physics of the growth mechanism.
constraint_claim(successor_iteration, mountain).
constraint_metric(successor_iteration, intensity, 1.0).
constraint_metric(successor_iteration, suppression_requirement, 0.05).
constraint_metric(successor_iteration, snap_back_potential, 0.0).
constraint_metric(successor_iteration, extractiveness, 0.01).

% The Fundamental Sequence Choice (The Tangled Rope)
% At limit ordinals, the growth depends on the choice of the sequence lambda_n. 
% Extractiveness at 0.55 triggers a Reform recommendation for standardizing sequences.
constraint_claim(limit_diagonalization_strategy, tangled_rope).
constraint_metric(limit_diagonalization_strategy, intensity, 0.82).
constraint_metric(limit_diagonalization_strategy, suppression_requirement, 0.40).
constraint_metric(limit_diagonalization_strategy, snap_back_potential, 0.70).
constraint_metric(limit_diagonalization_strategy, extractiveness, 0.55).

% The Recursive Unreachability (The Snare)
% The hierarchy eventually exceeds all functions provably total in a given theory (e.g., PA).
% Extractiveness at 0.99 requires the 'fundamental_sequence_scaffold' to index beyond epsilon_zero.
constraint_claim(theoretical_provability_ceiling, snare).
constraint_metric(theoretical_provability_ceiling, intensity, 0.99).
constraint_metric(theoretical_provability_ceiling, suppression_requirement, 0.95).
constraint_metric(theoretical_provability_ceiling, snap_back_potential, 0.01).
constraint_metric(theoretical_provability_ceiling, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.55 extractiveness) triggers Reform
recommendation(rec_01, utilize_standard_wainer_hierarchy_definitions_to_unify_limit_jumps).
affects_constraint(rec_01, limit_diagonalization_strategy).

% Snare (0.99 extractiveness) triggers Cut
% Utilizing 'fundamental_sequence_scaffold' to bridge the gap into the Bachmann-Howard ordinal range.
recommendation(rec_02, abandon_first_order_induction_to_verify_higher_ordinal_growth).
affects_constraint(rec_02, theoretical_provability_ceiling).

veto_actor(church_kleene_ordinal_limit).
veto_exposed(church_kleene_ordinal_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (definition) is high; Stability of the computation collapses almost immediately.
measurement(0, [1.00, 1.00, 0.90, 0.95]).
measurement(1, [1.00, 0.00, 0.40, 0.10]).

% 6. INTENT EVIDENCE
% Alternative: Grzegorczyk hierarchy (Discarded: limited to primitive recursive functions)
% Beneficiaries: Proof theorists (measuring consistency strength) and Googologists
% Power Delta: Iteration vs Diagonalization (Finite repetition vs Infinite scaling)
intent_evidence(ordinal_arithmetic_complexity, high_delta).
