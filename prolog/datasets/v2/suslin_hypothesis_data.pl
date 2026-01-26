% 1. ENTITIES & INTERVALS
entity(suslin_line_candidate, system).
entity(dense_linear_order, manifold).
entity(antichain_path, sensor_path).
entity(diamond_axiom_scaffold, scaffold).

interval(order_density_evaluation, 0, 1).
interval(cardinality_check, 0, 10).

% 2. EVENTS & OMEGA VARIABLES
event(e1, ccc_verification, 0, [countable_chain_condition, true]).
event(e2, separability_test, 5, [separable, false]).

% Omega Variable: Conceptual (The nature of 'completeness' in non-separable linear orders)
omega_variable(omega_c1, conceptual, distinguish_between_topological_separability_and_ccc).

% Omega Variable: Preference (Axiomatic bias toward the existence or non-existence of Suslin trees)
omega_variable(omega_p1, preference, decision_to_accept_diamond_axiom_vs_martins_axiom).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Order Density Invariant (The Mountain)
% The requirement that the line be a dense linear order without endpoints is the fixed anchor.
constraint_claim(dense_order_structure, mountain).
constraint_metric(dense_order_structure, intensity, 1.0).
constraint_metric(dense_order_structure, suppression_requirement, 0.05).
constraint_metric(dense_order_structure, snap_back_potential, 0.0).
constraint_metric(dense_order_structure, extractiveness, 0.01).

% The Countable Chain Entanglement (The Tangled Rope)
% The condition that every collection of disjoint open intervals is countable (CCC).
% Extractiveness at 0.54 triggers a Reform recommendation for tree-structure analysis.
constraint_claim(ccc_restriction, tangled_rope).
constraint_metric(ccc_restriction, intensity, 0.80).
constraint_metric(ccc_restriction, suppression_requirement, 0.38).
constraint_metric(ccc_restriction, snap_back_potential, 0.65).
constraint_metric(ccc_restriction, extractiveness, 0.54).

% The ZFC Independence Sink (The Snare)
% ZFC cannot determine if every CCC-dense-order is isomorphic to the real line (separability).
% Extractiveness at 0.98 requires 'diamond_axiom_scaffold' to create a counterexample.
constraint_claim(suslin_separability_undecidability, snare).
constraint_metric(suslin_separability_undecidability, intensity, 0.99).
constraint_metric(suslin_separability_undecidability, suppression_requirement, 0.96).
constraint_metric(suslin_separability_undecidability, snap_back_potential, 0.01).
constraint_metric(suslin_separability_undecidability, extractiveness, 0.98).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.54 extractiveness) triggers Reform
recommendation(rec_01, translate_linear_order_properties_into_suslin_tree_topology).
affects_constraint(rec_01, ccc_restriction).

% Snare (0.98 extractiveness) triggers Cut
% Utilizing 'diamond_axiom_scaffold' (Phi) to construct a non-separable Suslin line.
recommendation(rec_02, adopt_diamond_axiom_to_construct_suslin_line_counterexample).
affects_constraint(rec_02, suslin_separability_undecidability).

veto_actor(solovay_tennenbaum_independence).
veto_exposed(solovay_tennenbaum_independence, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Utility for set theory is high, but Stability of the 'Truth' value is zero in ZFC alone.
measurement(0, [0.40, 1.00, 0.75, 0.85]).
measurement(10, [0.00, 0.00, 0.10, 0.05]).

% 6. INTENT EVIDENCE
% Alternative: Martin's Axiom (MA) + not CH (Discarded: This implies the Suslin Hypothesis is true)
% Beneficiaries: Infinite combinatorists and set-theoretic topologists
% Power Delta: Axiomatic Force (Consistency of Suslin Line existence vs non-existence)
intent_evidence(forcing_independence_limit, high_delta).
