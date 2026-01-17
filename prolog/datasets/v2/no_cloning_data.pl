% 1. ENTITIES & INTERVALS
entity(quantum_state_psi, system).
entity(hilbert_space_tensor_product, manifold).
entity(unitary_evolution_path, sensor_path).
entity(orthogonal_state_scaffold, scaffold).

interval(cloning_operation, 0, 1).
interval(linearity_verification, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, unitary_transformation, 0, [operator, U]).
event(e2, superposition_interference, 1, [result, non_linear_clashing]).

% Omega Variable: Conceptual (The tradeoff between quantum information security and copyability)
omega_variable(omega_c1, conceptual, alignment_of_unitarity_with_the_impossibility_of_perfect_broadcast).

% Omega Variable: Empirical (The impact of environmental decoherence on cloning fidelity)
omega_variable(omega_e1, empirical, physical_fidelity_limits_in_optimal_quantum_cloning_machines).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Unitary Requirement (The Mountain)
% Quantum evolution must be unitary ($U^\dagger U = I$); this preserves probability and is a rigid law of physics.
constraint_claim(unitarity_invariance, mountain).
constraint_metric(unitarity_invariance, intensity, 1.0).
constraint_metric(unitarity_invariance, suppression_requirement, 0.05).
constraint_metric(unitarity_invariance, snap_back_potential, 0.0).
constraint_metric(unitarity_invariance, extractiveness, 0.01).

% The Linearity Entanglement (The Tangled Rope)
% The requirement that $U$ acts linearly on superpositions $|\psi\rangle = a|0\rangle + b|1\rangle$.
% Extractiveness at 0.65 triggers a Reform recommendation for operator analysis.
constraint_claim(linearity_distribution, tangled_rope).
constraint_metric(linearity_distribution, intensity, 0.90).
constraint_metric(linearity_distribution, suppression_requirement, 0.45).
constraint_metric(linearity_distribution, snap_back_potential, 0.80).
constraint_metric(linearity_distribution, extractiveness, 0.65).

% The Overlap Noose (The Noose)
% The mathematical contradiction where non-orthogonal states cannot be cloned by a single unitary.
% Extractiveness at 0.99 requires the 'orthogonal_state_scaffold' to distinguish cloneable subsets.
constraint_claim(inner_product_preservation, noose).
constraint_metric(inner_product_preservation, intensity, 0.99).
constraint_metric(inner_product_preservation, suppression_requirement, 0.98).
constraint_metric(inner_product_preservation, snap_back_potential, 0.01).
constraint_metric(inner_product_preservation, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.65 extractiveness) triggers Reform
recommendation(rec_01, utilize_density_matrix_formalism_to_analyze_imperfect_cloning).
affects_constraint(rec_01, linearity_distribution).

% Noose (0.99 extractiveness) triggers Cut
% Utilizing 'orthogonal_state_scaffold' to restrict cloning only to mutually orthogonal basis sets.
recommendation(rec_02, abandon_universal_cloning_for_orthogonal_basis_replication).
affects_constraint(rec_02, inner_product_preservation).

veto_actor(linearity_of_quantum_mechanics_axiom).
veto_exposed(linearity_of_quantum_mechanics_axiom, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (ability to process) is 1.0; Stability of the "Copy" is zero for unknown states.
measurement(0, [1.00, 1.00, 0.50, 0.90]).
measurement(1, [1.00, 0.00, 0.99, 0.10]).

% 6. INTENT EVIDENCE
% Alternative: No-Deletion Theorem (Discarded: the dual to cloning, but less central to copy-protection)
% Beneficiaries: Quantum cryptographers (BB84 security) and Information theorists
% Power Delta: Linearity vs Duplication (Sum of paths vs Product of states)
intent_evidence(unitary_contradiction_detection, high_delta).
