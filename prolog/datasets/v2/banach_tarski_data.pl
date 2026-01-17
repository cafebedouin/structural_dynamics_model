% --- 1. Entities & Intervals ---
entity(solid_ball_unit_v, process).
entity(isometry_group_so3, group).
entity(point_set_decomposition, group).
entity(axiom_of_choice_scaffold, scaffold).
entity(non_measurable_set_scaffold, scaffold).

interval(derivation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(hausdorff_paradox_input, initialization, 0, [actor(felix_hausdorff), property(sphere_decomposition)]).
event(partitioning_phase, processing, 3, [action(group_action_rotation), state(five_piece_split)]).
event(reassembly_operation, update, 7, [action(rigid_motion), result(volume_doubling)]).
event(set_theoretic_closure, termination, 10, [condition(existence_without_algorithm)]).

omega_variable(omega_1, conceptual, definition_of_volume_for_non_measurable_subsets).
omega_variable(omega_2, preference, acceptance_of_axiom_of_choice_over_determinacy).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(rigid_motion_invariance, mountain).
constraint_metric(rigid_motion_invariance, intensity, 0.05).
constraint_metric(rigid_motion_invariance, suppression_requirement, 0.01).
constraint_metric(rigid_motion_invariance, snap_back_potential, 0.00).
constraint_metric(rigid_motion_invariance, extractiveness, 0.02).

constraint_claim(axiom_of_choice_dependency, rope).
constraint_metric(axiom_of_choice_dependency, intensity, 0.35).
constraint_metric(axiom_of_choice_dependency, suppression_requirement, 0.14).
constraint_metric(axiom_of_choice_dependency, snap_back_potential, 0.10).
constraint_metric(axiom_of_choice_dependency, extractiveness, 0.30).

constraint_claim(point_set_partitioning, tangled_rope).
constraint_metric(point_set_partitioning, intensity, 0.60).
constraint_metric(point_set_partitioning, suppression_requirement, 0.40).
constraint_metric(point_set_partitioning, snap_back_potential, 0.50).
constraint_metric(point_set_partitioning, extractiveness, 0.55).

constraint_claim(measure_preservation_limit, noose).
constraint_metric(measure_preservation_limit, intensity, 0.95).
constraint_metric(measure_preservation_limit, suppression_requirement, 0.85).
constraint_metric(measure_preservation_limit, snap_back_potential, 0.90).
constraint_metric(measure_preservation_limit, extractiveness, 0.92).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_isometry_definitions_for_subset_translations).
affects_constraint(rec_01, rigid_motion_invariance).

recommendation(rec_02, reform_decomposition_logic_to_specify_non_measurable_boundaries).
affects_constraint(rec_02, point_set_partitioning).

recommendation(rec_03, cut_reliance_on_finite_additivity_for_singular_point_sets).
affects_constraint(rec_03, measure_preservation_limit).

veto_actor(constructivist_mathematician).
veto_exposed(constructivist_mathematician, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.80, 0.90, 0.30, 0.70]).
measurement(10, [0.10, 0.05, 0.95, 0.01]).

% --- 6. Intent Evidence ---
intent_alternative(solovay_model, excludes_non_measurable_sets).
intent_alternative(tarski_circle_squaring, two_dimensional_variant).
intent_beneficiary(formalist_logicians, validation_of_zfc_extremes).
intent_power_delta(infinite_precision_vs_physical_reality, total_theoretical_decoupling).
