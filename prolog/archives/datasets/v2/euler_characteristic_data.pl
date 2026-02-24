% --- 1. Entities & Intervals ---
entity(topological_manifold_v, process).
entity(vertex_set, group).
entity(edge_set, group).
entity(face_set, group).
entity(triangulation_scaffold, scaffold).
entity(genus_mapping_scaffold, scaffold).

interval(calculation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(polyhedral_input, initialization, 0, [actor(geometry), property(convexity)]).
event(element_count, collection, 3, [action(v_e_f_summation), state(topological_invariant)]).
event(homeomorphism_test, processing, 6, [action(deformation), property(stretching_constancy)]).
event(genus_determination, termination, 10, [condition(chi_equals_two_minus_two_g)]).

omega_variable(omega_1, conceptual, definition_of_boundary_integration_for_non_orientable_manifolds).
omega_variable(omega_2, empirical, exact_face_count_for_high_dimensional_simplicial_complexes).

% --- 3. Constraint Claims & Kinetic Metrics ---
% Mountain: Invariance under homeomorphism (Topological Rigidity)
constraint_claim(topological_invariance, mountain).
constraint_metric(topological_invariance, intensity, 0.05).
constraint_metric(topological_invariance, suppression_requirement, 0.04).
constraint_metric(topological_invariance, snap_back_potential, 0.0).
constraint_metric(topological_invariance, extractiveness, 0.02).

% Rope: Euler Formula for Convex Polyhedra (V - E + F = 2)
constraint_claim(euler_convex_rule, rope).
constraint_metric(euler_convex_rule, intensity, 0.32).
constraint_metric(euler_convex_rule, suppression_requirement, 0.12).
constraint_metric(euler_convex_rule, snap_back_potential, 0.05).
constraint_metric(euler_convex_rule, extractiveness, 0.28).

% Tangled Rope: Curvature-Topology Link (Gauss-Bonnet Coupling)
constraint_claim(curvature_coupling_friction, tangled_rope).
constraint_metric(curvature_coupling_friction, intensity, 0.58).
constraint_metric(curvature_coupling_friction, suppression_requirement, 0.35).
constraint_metric(curvature_coupling_friction, snap_back_potential, 0.40).
constraint_metric(curvature_coupling_friction, extractiveness, 0.52).

% Snare: Singularities and Non-Compactness Obstructions
constraint_claim(singularity_noose, snare).
constraint_metric(singularity_noose, intensity, 0.96).
constraint_metric(singularity_noose, suppression_requirement, 0.88).
constraint_metric(singularity_noose, snap_back_potential, 0.92).
constraint_metric(singularity_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_invariance_as_fundamental_topological_constant).
affects_constraint(rec_01, topological_invariance).

recommendation(rec_02, maintain_combinatorial_verification_for_planar_graphs).
affects_constraint(rec_02, euler_convex_rule).

recommendation(rec_03, reform_metric_analysis_via_triangulation_scaffold).
affects_constraint(rec_03, curvature_coupling_friction).

recommendation(rec_04, cut_topological_instability_via_genus_mapping_scaffold).
affects_constraint(rec_04, singularity_noose).

veto_actor(non_manifold_geometry).
veto_exposed(non_manifold_geometry, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.60, 0.50, 0.70]).
measurement(10, [0.35, 0.99, 0.98, 0.92]).

% --- 6. Intent Evidence ---
intent_alternative(betti_numbers, alternative_homology_description).
intent_alternative(poincare_duality, symmetric_structural_constraint).
intent_beneficiary(algebraic_topologists, classification_of_surfaces).
intent_power_delta(topology_vs_geometry, total_structural_domination_over_metric_changes).
