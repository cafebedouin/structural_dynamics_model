% --- 1. Entities & Intervals ---
entity(field_extension_system, process).
entity(base_field_f, group).
entity(splitting_field_e, group).
entity(galois_group_g, group).
entity(intermediate_field_scaffold, scaffold).
entity(normal_subgroup_scaffold, scaffold).

interval(algebraic_evolution, 0, 10).

% --- 2. Events & Omega Variables ---
event(polynomial_definition, initialization, 0, [actor(algebraist), property(irreducibility)]).
event(root_permutation, processing, 4, [action(symmetry_mapping), state(automorphism_group)]).
event(fixed_field_identification, update, 7, [action(galois_correspondence), state(subgroup_field_duality)]).
event(solvability_determination, termination, 10, [condition(group_solvability_equals_radical_solvability)]).

omega_variable(omega_1, conceptual, definition_of_solvability_for_non_standard_characteristic_fields).
omega_variable(omega_2, empirical, exact_computational_cost_of_calculating_galois_groups_for_high_degree_polynomials).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Fundamental Theorem of Galois Theory (The unbending duality between fields and groups)
constraint_claim(galois_correspondence_invariance, mountain).
constraint_metric(galois_correspondence_invariance, intensity, 0.04).
constraint_metric(galois_correspondence_invariance, suppression_requirement, 0.03).
constraint_metric(galois_correspondence_invariance, snap_back_potential, 0.00).
constraint_metric(galois_correspondence_invariance, extractiveness, 0.02).

% ROPE: Normal Extension Requirement (The tether ensuring all roots are present)
constraint_claim(normality_requirement, rope).
constraint_metric(normality_requirement, intensity, 0.30).
constraint_metric(normality_requirement, suppression_requirement, 0.12).
constraint_metric(normality_requirement, snap_back_potential, 0.05).
constraint_metric(normality_requirement, extractiveness, 0.28).

% TANGLED ROPE: Radical Solvability Friction (Complexity of determining if roots are expressible by radicals)
constraint_claim(radical_expression_complexity, tangled_rope).
constraint_metric(radical_expression_complexity, intensity, 0.55).
constraint_metric(radical_expression_complexity, suppression_requirement, 0.35).
constraint_metric(radical_expression_complexity, snap_back_potential, 0.40).
constraint_metric(radical_expression_complexity, extractiveness, 0.52).

% NOOSE: Abel-Ruffini Impossibility (The terminal limit for polynomials of degree 5 or higher)
constraint_claim(quintic_solvability_noose, noose).
constraint_metric(quintic_solvability_noose, intensity, 0.95).
constraint_metric(quintic_solvability_noose, suppression_requirement, 0.88).
constraint_metric(quintic_solvability_noose, snap_back_potential, 0.92).
constraint_metric(quintic_solvability_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_the_fundamental_correspondence_as_structural_invariant).
affects_constraint(rec_01, galois_correspondence_invariance).

recommendation(rec_02, maintain_separability_and_normality_for_stable_group_mapping).
affects_constraint(rec_02, normality_requirement).

recommendation(rec_03, reform_root_analysis_via_intermediate_field_scaffold).
affects_constraint(rec_03, radical_expression_complexity).

recommendation(rec_04, cut_solvability_deadlocks_via_normal_subgroup_scaffold).
affects_constraint(rec_04, quintic_solvability_noose).

veto_actor(non_separable_field_characteristic).
veto_exposed(non_separable_field_characteristic, rec_02).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.60, 0.40, 0.50]).
measurement(10, [0.40, 0.99, 0.98, 0.92]).

% --- 6. Intent Evidence ---
intent_alternative(field_theory_without_groups, lacks_solvability_criteria).
intent_alternative(inverse_galois_problem, unsolved_mapping_direction).
intent_beneficiary(algebraic_number_theorists, unification_of_equations_and_group_symmetry).
intent_power_delta(group_theory_vs_algebraic_geometry, high_symmetric_determination).
