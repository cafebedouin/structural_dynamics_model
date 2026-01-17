% --- 1. Entities & Intervals ---
entity(euclidean_geometry, process).
entity(right_angled_triangle, group).
entity(squared_sides_sum, group).
entity(hypotenuse_square, group).
entity(algebraic_rearrangement_scaffold, scaffold).
entity(geometric_dissection_scaffold, scaffold).

interval(proof_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(construction_of_right_angle, initialization, 0, [actor(geometry), property(perpendicularity)]).
event(side_measurement, collection, 2, [actor(observer), property(lengths_a_b_c)]).
event(area_transformation, processing, 5, [action(squaring), state(expansion_to_two_dimensions)]).
event(equative_convergence, termination, 10, [condition(a2_plus_b2_equals_c2)]).

omega_variable(omega_1, conceptual, extension_to_non_euclidean_manifolds).
omega_variable(omega_2, empirical, measurement_precision_limits_in_physical_space).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(planar_flatness_requirement, mountain).
constraint_metric(planar_flatness_requirement, intensity, 0.04).
constraint_metric(planar_flatness_requirement, suppression_requirement, 0.01).
constraint_metric(planar_flatness_requirement, snap_back_potential, 0.00).
constraint_metric(planar_flatness_requirement, extractiveness, 0.02).

constraint_claim(right_angle_orthogonality, rope).
constraint_metric(right_angle_orthogonality, intensity, 0.32).
constraint_metric(right_angle_orthogonality, suppression_requirement, 0.12).
constraint_metric(right_angle_orthogonality, snap_back_potential, 0.05).
constraint_metric(right_angle_orthogonality, extractiveness, 0.28).

constraint_claim(arithmetic_integer_bias, tangled_rope).
constraint_metric(arithmetic_integer_bias, intensity, 0.55).
constraint_metric(arithmetic_integer_bias, suppression_requirement, 0.30).
constraint_metric(arithmetic_integer_bias, snap_back_potential, 0.40).
constraint_metric(arithmetic_integer_bias, extractiveness, 0.48).

constraint_claim(fixed_metric_space_limit, noose).
constraint_metric(fixed_metric_space_limit, intensity, 0.92).
constraint_metric(fixed_metric_space_limit, suppression_requirement, 0.82).
constraint_metric(fixed_metric_space_limit, snap_back_potential, 0.88).
constraint_metric(fixed_metric_space_limit, extractiveness, 0.85).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_euclidean_axioms_for_standard_triangulation).
affects_constraint(rec_01, planar_flatness_requirement).

recommendation(rec_02, reform_preference_for_pythagorean_triples_to_include_irrationals).
affects_constraint(rec_02, arithmetic_integer_bias).

recommendation(rec_03, cut_limitations_of_flat_geometry_via_spherical_trigonometry_scaffolding).
affects_constraint(rec_03, fixed_metric_space_limit).

veto_actor(strict_classical_geometer).
veto_exposed(strict_classical_geometer, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.95, 0.70, 0.90]).
measurement(10, [0.40, 0.99, 0.98, 0.95]).

% --- 6. Intent Evidence ---
intent_alternative(cosine_rule, general_triangle_application).
intent_alternative(fermat_last_theorem, higher_exponent_impossibility).
intent_beneficiary(navigators_and_architects, spatial_calculation_reliability).
intent_power_delta(geometry_vs_algebra, total_structural_synthesis).
