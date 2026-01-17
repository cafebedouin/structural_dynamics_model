% 1. ENTITIES & INTERVALS
entity(unit_interval, system).
entity(unit_square, manifold).
entity(space_filling_path, sensor_path).
entity(quadtree_grid, scaffold).

interval(recursion_depth, 0, 9).
interval(spatial_coverage, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, base_transformation, 0, [generator, nine_sub_squares]).
event(e2, surjection_verification, 9, [coverage_ratio, 1_0]).

% Omega Variable: Conceptual (Defining 'dimension' when topological dim is 1 but Hausdorff dim is 2)
omega_variable(omega_c1, conceptual, conflict_between_path_continuity_and_area_coverage).

% Omega Variable: Empirical (Computational limits of mapping floating point precision to discrete grid cells)
omega_variable(omega_e1, empirical, numerical_instability_in_high_order_coordinate_mapping).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Continuity Constraint (The Mountain)
% The Peano curve is a continuous surjection by definition; the path cannot break.
constraint_claim(topological_continuity, mountain).
constraint_metric(topological_continuity, intensity, 1.0).
constraint_metric(topological_continuity, suppression_requirement, 0.05).
constraint_metric(topological_continuity, snap_back_potential, 0.0).
constraint_metric(topological_continuity, extractiveness, 0.01).

% The Self-Intersecting Density (The Tangled Rope)
% As n increases, the path becomes infinitely dense within the square, creating maximal entanglement.
% Extractiveness at 0.62 triggers a Reform recommendation for indexing.
constraint_claim(recursive_density, tangled_rope).
constraint_metric(recursive_density, intensity, 0.88).
constraint_metric(recursive_density, suppression_requirement, 0.44).
constraint_metric(recursive_density, snap_back_potential, 0.80).
constraint_metric(recursive_density, extractiveness, 0.62).

% The Differentiability Collapse (The Noose)
% The curve is nowhere differentiable; the 'velocity' of the path is undefined everywhere.
% Extractiveness at 0.96 requires the 'quadtree_grid' scaffold to provide discrete navigation.
constraint_claim(derivative_nullification, noose).
constraint_metric(derivative_nullification, intensity, 0.99).
constraint_metric(derivative_nullification, suppression_requirement, 0.94).
constraint_metric(derivative_nullification, snap_back_potential, 0.02).
constraint_metric(derivative_nullification, extractiveness, 0.96).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.62 extractiveness) triggers Reform
recommendation(rec_01, implement_z_order_or_hilbert_indexing_for_locality).
affects_constraint(rec_01, recursive_density).

% Noose (0.96 extractiveness) triggers Cut
% Utilizing 'quadtree_grid' scaffold to replace continuous motion with discrete jumps.
recommendation(rec_02, discard_infinitesimal_calculus_in_favor_of_discrete_measure).
affects_constraint(rec_02, derivative_nullification).

veto_actor(net_analysis_standard).
veto_exposed(net_analysis_standard, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (coverage) reaches 1.0, but Stability (predictability of direction) vanishes.
measurement(0, [0.10, 1.00, 0.20, 1.00]).
measurement(9, [1.00, 0.00, 0.90, 0.80]).

% 6. INTENT EVIDENCE
% Alternative: Hilbert Curve (Discarded: Peano uses 3x3 generator vs Hilbert 2x2)
% Beneficiaries: Database engineers (spatial indexing) and Image compression theorists
% Power Delta: Dimensionality Jump (1D line vs 2D area saturation)
intent_evidence(surjective_mapping, high_delta).
