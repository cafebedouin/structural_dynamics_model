% 1. ENTITIES & INTERVALS
entity(unit_interval_closed, system).
entity(cantor_dust, manifold).
entity(recursive_deletion_path, sensor_path).
entity(ternary_expansion_grid, scaffold).

interval(iteration_depth, 0, 10).
interval(measure_evaluation, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, initial_segment_removal, 1, [removed_interval, open_one_third_to_two_thirds]).
event(e2, measure_zero_convergence, 10, [lebesgue_measure, 0_0]).

% Omega Variable: Conceptual (The 'thickness' of a set with no intervals but uncountably many points)
omega_variable(omega_c1, conceptual, physical_intuition_of_zero_measure_uncountable_sets).

% Omega Variable: Empirical (Limit of visual representation for fractal dimensions)
omega_variable(omega_e1, empirical, resolution_limit_of_rendered_subsegments).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Boundary Retention (The Mountain)
% The endpoints of every removed interval are never removed; they form the skeleton.
constraint_claim(endpoint_permanence, mountain).
constraint_metric(endpoint_permanence, intensity, 1.0).
constraint_metric(endpoint_permanence, suppression_requirement, 0.05).
constraint_metric(endpoint_permanence, snap_back_potential, 0.0).
constraint_metric(endpoint_permanence, extractiveness, 0.01).

% The Iterative Fragmentation (The Tangled Rope)
% The process of $2^n$ segments creates a highly complex, self-similar entanglement.
% Extractiveness at 0.58 triggers a Reform recommendation to shift to ternary logic.
constraint_claim(self_similar_fragmentation, tangled_rope).
constraint_metric(self_similar_fragmentation, intensity, 0.80).
constraint_metric(self_similar_fragmentation, suppression_requirement, 0.40).
constraint_metric(self_similar_fragmentation, snap_back_potential, 0.70).
constraint_metric(self_similar_fragmentation, extractiveness, 0.58).

% The Lebesgue Measure Dissolution (The Noose)
% The total length of the set vanishes to zero, "strangling" the 1D spatial utility.
% Extractiveness at 0.94 requires the 'ternary_expansion_grid' scaffold to resolve.
constraint_claim(measure_exhaustion, noose).
constraint_metric(measure_exhaustion, intensity, 0.98).
constraint_metric(measure_exhaustion, suppression_requirement, 0.92).
constraint_metric(measure_exhaustion, snap_back_potential, 0.05).
constraint_metric(measure_exhaustion, extractiveness, 0.94).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.58 extractiveness) triggers Reform
recommendation(rec_01, transition_to_base_3_representation_for_point_identification).
affects_constraint(rec_01, self_similar_fragmentation).

% Noose (0.94 extractiveness) triggers Cut
% Utilizing 'ternary_expansion_grid' to map points to the binary set {0, 2}.
recommendation(rec_02, redefine_utility_via_fractal_dimension_rather_than_length).
affects_constraint(rec_02, measure_exhaustion).

veto_actor(perfect_set_property).
veto_exposed(perfect_set_property, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (cardinality) remains high, but Utility (measure) drops to zero.
measurement(0, [1.00, 1.00, 1.00, 1.00]).
measurement(10, [1.00, 0.10, 0.00, 1.00]).

% 6. INTENT EVIDENCE
% Alternative: Fat Cantor Set (Discarded: results in positive Lebesgue measure)
% Beneficiaries: General topologists and chaos theorists (strange attractors)
% Power Delta: Hausdorff Dimension ($\ln 2 / \ln 3$ vs Topological Dimension 0)
intent_evidence(fractal_scaling_ratio, high_delta).
