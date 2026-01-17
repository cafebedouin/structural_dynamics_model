% 1. ENTITIES & INTERVALS
entity(coloration_system, system).
entity(integer_interval_n, manifold).
entity(arithmetic_progression_path, sensor_path).
entity(szemeredi_scaffold, scaffold).

interval(coloring_assignment, 1, 100). % Arbitrary finite N
interval(progression_search, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, pigeonhole_distribution, 0, [colors, r]).
event(e2, monochromatic_emergence, 1, [progression_length, k]).

% Omega Variable: Conceptual (The nature of 'inevitable structure' in random distributions)
omega_variable(omega_c1, conceptual, alignment_of_ramsey_theory_with_anti_entropy_principles).

% Omega Variable: Empirical (The astronomical growth rate of the Van der Waerden number W(r, k))
omega_variable(omega_e1, empirical, computational_impossibility_of_calculating_exact_bounds_for_large_k).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Total Partition Invariant (The Mountain)
% The requirement that every integer must receive a color; no gaps in the coloring are permitted.
constraint_claim(total_partitioning, mountain).
constraint_metric(total_partitioning, intensity, 1.0).
constraint_metric(total_partitioning, suppression_requirement, 0.05).
constraint_metric(total_partitioning, snap_back_potential, 0.0).
constraint_metric(total_partitioning, extractiveness, 0.01).

% The Arithmetic Pattern Entanglement (The Tangled Rope)
% The overlapping sets of potential progressions (a, a+d, ..., a+(k-1)d) in the interval.
% Extractiveness at 0.58 triggers a Reform recommendation for combinatorial tracking.
constraint_claim(progression_overlap_density, tangled_rope).
constraint_metric(progression_overlap_density, intensity, 0.85).
constraint_metric(progression_overlap_density, suppression_requirement, 0.40).
constraint_metric(progression_overlap_density, snap_back_potential, 0.70).
constraint_metric(progression_overlap_density, extractiveness, 0.58).

% The Wowzer-Type Growth Noose (The Noose)
% The bound N(r, k) grows faster than any simple exponential, "strangling" search algorithms.
% Extractiveness at 0.98 requires the 'szemeredi_scaffold' to resolve via density arguments.
constraint_claim(primitive_recursive_bound_failure, noose).
constraint_metric(primitive_recursive_bound_failure, intensity, 0.99).
constraint_metric(primitive_recursive_bound_failure, suppression_requirement, 0.95).
constraint_metric(primitive_recursive_bound_failure, snap_back_potential, 0.01).
constraint_metric(primitive_recursive_bound_failure, extractiveness, 0.98).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.58 extractiveness) triggers Reform
recommendation(rec_01, utilize_polymath_indices_to_track_higher_order_patterns).
affects_constraint(rec_01, progression_overlap_density).

% Noose (0.98 extractiveness) triggers Cut
% Utilizing 'szemeredi_scaffold' to prove existence via density rather than pure coloring.
recommendation(rec_02, replace_color_search_with_szemeredi_density_analysis).
affects_constraint(rec_02, primitive_recursive_bound_failure).

veto_actor(shelah_primitive_recursive_limit).
veto_exposed(shelah_primitive_recursive_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (ability to color) is 1.0; Utility is high for combinatorics, but search Stability is 0.
measurement(1, [1.00, 1.00, 0.80, 0.95]).
measurement(100, [1.00, 0.00, 0.99, 0.20]).

% 6. INTENT EVIDENCE
% Alternative: Hales-Jewett Theorem (Discarded: applies to high-dimensional cubes, more general)
% Beneficiaries: Combinatorists and additive number theorists
% Power Delta: Color Partition (Randomized assignment vs Inevitable pattern)
intent_evidence(partition_regularity, high_delta).
