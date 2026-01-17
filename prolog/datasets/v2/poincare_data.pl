% --- 1. Entities & Intervals ---
entity(poincare_conjecture, process).
entity(three_sphere_manifold, group).
entity(compact_simply_connected_manifold, group).
entity(ricci_flow_scaffold, scaffold).
entity(surgery_procedure_scaffold, scaffold).

interval(calculation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(conjecture_formulation, initialization, 0, [actor(henri_poincare), property(homeomorphism_to_3_sphere)]).
event(higher_dimension_resolution, processing, 4, [actor(smale_stallings_freedman), result(proven_for_n_greater_than_3)]).
event(singular_singularity_formation, conflict, 7, [action(ricci_flow_evolution), state(neck_pinch)]).
event(perelman_verification, termination, 10, [condition(proof_of_geometrization)]).

omega_variable(omega_1, conceptual, definition_of_smooth_structure_equivalence_in_four_dimensions).
omega_variable(omega_2, empirical, exact_topological_classification_of_all_non_simply_connected_three_manifolds).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Topological Invariance of Simple Connectivity
constraint_claim(simple_connectivity_invariance, mountain).
constraint_metric(simple_connectivity_invariance, intensity, 0.05).
constraint_metric(simple_connectivity_invariance, suppression_requirement, 0.05).
constraint_metric(simple_connectivity_invariance, snap_back_potential, 0.00).
constraint_metric(simple_connectivity_invariance, extractiveness, 0.02).

% ROPE: Compactness Constraint
constraint_claim(compactness_requirement, rope).
constraint_metric(compactness_requirement, intensity, 0.32).
constraint_metric(compactness_requirement, suppression_requirement, 0.12).
constraint_metric(compactness_requirement, snap_back_potential, 0.05).
constraint_metric(compactness_requirement, extractiveness, 0.28).

% TANGLED ROPE: Ricci Flow Singularity
constraint_claim(ricci_flow_singularity_friction, tangled_rope).
constraint_metric(ricci_flow_singularity_friction, intensity, 0.60).
constraint_metric(ricci_flow_singularity_friction, suppression_requirement, 0.35).
constraint_metric(ricci_flow_singularity_friction, snap_back_potential, 0.40).
constraint_metric(ricci_flow_singularity_friction, extractiveness, 0.55).

% NOOSE: Geometrization Conjecture Obstruction
constraint_claim(geometrization_completeness_noose, noose).
constraint_metric(geometrization_completeness_noose, intensity, 0.95).
constraint_metric(geometrization_completeness_noose, suppression_requirement, 0.88).
constraint_metric(geometrization_completeness_noose, snap_back_potential, 0.92).
constraint_metric(geometrization_completeness_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_topological_invariants_as_unending_geometry_bases).
affects_constraint(rec_01, simple_connectivity_invariance).

recommendation(rec_02, maintain_finitude_boundaries_via_compactness_standards).
affects_constraint(rec_02, compactness_requirement).

recommendation(rec_03, reform_manifold_evolution_via_ricci_flow_scaffold).
affects_constraint(rec_03, ricci_flow_singularity_friction).

recommendation(rec_04, cut_topological_deadlocks_via_surgery_procedure_scaffold).
affects_constraint(rec_04, geometrization_completeness_noose).

veto_actor(non_triangulable_manifold).
veto_exposed(non_triangulable_manifold, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.80, 0.40, 0.20, 0.50]).
measurement(10, [0.25, 0.99, 0.98, 0.90]).

% --- 6. Intent Evidence ---
intent_alternative(thurston_geometrization, structural_precursor).
intent_alternative(hamilton_program, failed_without_surgery).
intent_beneficiary(low_dimensional_topologists, unification_of_three_manifold_theory).
intent_power_delta(smooth_manifold_vs_topological_manifold, high_dimension_logic_asymmetry).
