% 1. ENTITIES & INTERVALS
entity(nonstandard_arithmetic, system).
entity(compactness_manifold, manifold).
entity(infinite_integer_path, sensor_path).
entity(overspill_scaffold, scaffold).

interval(standard_segment, 0, 1000). % Finite N
interval(external_comparison, 1000, 10000). % Entry into the non-standard galaxy

% 2. EVENTS & OMEGA VARIABLES
event(e1, standard_induction_check, 0, [result, success]).
event(e2, infinite_integer_detection, 1001, [property, larger_than_any_standard_n]).

% Omega Variable: Conceptual (The distinction between 'internal' and 'external' sets)
omega_variable(omega_c1, conceptual, existence_of_non_definable_external_sets_within_the_model).

% Omega Variable: Empirical (The inability to compute the 'actual' value of an infinite integer)
omega_variable(omega_e1, empirical, resolution_limit_of_human_notation_for_nonstandard_elements).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Peano Axiom Invariance (The Mountain)
% The model satisfies all first-order axioms of PA; it is indistinguishable from N by first-order logic.
constraint_claim(first_order_equivalence, mountain).
constraint_metric(first_order_equivalence, intensity, 1.0).
constraint_metric(first_order_equivalence, suppression_requirement, 0.05).
constraint_metric(first_order_equivalence, snap_back_potential, 0.0).
constraint_metric(first_order_equivalence, extractiveness, 0.01).

% The Order Type Entanglement (The Tangled Rope)
% The structure is N + (Z * Q), creating a dense, discrete entanglement of "galaxies."
% Extractiveness at 0.60 triggers a Reform recommendation for order-type mapping.
constraint_claim(non_well_ordered_topology, tangled_rope).
constraint_metric(non_well_ordered_topology, intensity, 0.85).
constraint_metric(non_well_ordered_topology, suppression_requirement, 0.45).
constraint_metric(non_well_ordered_topology, snap_back_potential, 0.70).
constraint_metric(non_well_ordered_topology, extractiveness, 0.60).

% The Induction Failure Gap (The Noose)
% The "set of standard numbers" is not an internal set, so induction cannot prove its properties.
% Extractiveness at 0.95 requires the 'overspill_scaffold' to bridge the standard/nonstandard gap.
constraint_claim(induction_external_limitation, noose).
constraint_metric(induction_external_limitation, intensity, 0.98).
constraint_metric(induction_external_limitation, suppression_requirement, 0.92).
constraint_metric(induction_external_limitation, snap_back_potential, 0.05).
constraint_metric(induction_external_limitation, extractiveness, 0.95).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.60 extractiveness) triggers Reform
recommendation(rec_01, utilize_lowenheim_skolem_indices_to_track_galaxy_position).
affects_constraint(rec_01, non_well_ordered_topology).

% Noose (0.95 extractiveness) triggers Cut
% Utilizing 'overspill_scaffold' to allow properties holding for all standard n to hold for some infinite c.
recommendation(rec_02, apply_overspill_principle_to_extend_standard_properties).
affects_constraint(rec_02, induction_external_limitation).

veto_actor(tennenbaum_theorem_limit).
veto_exposed(tennenbaum_theorem_limit, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (internal logic) is 1.0; Stability is low when viewed externally as a non-well-ordered set.
measurement(0, [1.00, 1.00, 1.00, 1.00]).
measurement(1001, [1.00, 0.15, 0.40, 0.60]).

% 6. INTENT EVIDENCE
% Alternative: True Arithmetic (Discarded: This model satisfies PA, not necessarily all truths of N)
% Beneficiaries: Model theorists and non-standard analysts (infinitesimal foundations)
% Power Delta: Compactness Force (Finite consistency vs Infinite realization)
intent_evidence(compactness_theorem_application, high_delta).
