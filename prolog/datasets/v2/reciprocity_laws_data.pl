% --- 1. Entities & Intervals ---
entity(reciprocity_system, process).
entity(prime_ideal_pool, group).
entity(residue_symbol_field, group).
entity(artin_map_scaffold, scaffold).
entity(class_field_scaffold, scaffold).

interval(theory_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(euler_conjecture, initialization, 0, [actor(leonhard_euler), property(quadratic_residues)]).
event(gauss_disquisitiones, processing, 3, [actor(carl_friedrich_gauss), result(quadratic_reciprocity_proof)]).
event(hilbert_symbol_unification, update, 7, [action(local_global_principle), property(norm_residue_symbol)]).
event(langlands_correspondence, termination, 10, [condition(non_abelian_generalization)]).

omega_variable(omega_1, conceptual, definition_of_non_abelian_reciprocity_for_higher_dimensional_representations).
omega_variable(omega_2, empirical, exact_computation_of_artin_l_functions_at_singular_points).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Law of Quadratic Reciprocity (Invariance of the Legendre symbol relation)
constraint_claim(quadratic_symmetry_invariance, mountain).
constraint_metric(quadratic_symmetry_invariance, intensity, 0.05).
constraint_metric(quadratic_symmetry_invariance, suppression_requirement, 0.05).
constraint_metric(quadratic_symmetry_invariance, snap_back_potential, 0.00).
constraint_metric(quadratic_symmetry_invariance, extractiveness, 0.02).

% ROPE: Abelian Class Field Theory (The tether of Abelian extensions)
constraint_claim(abelian_extension_limit, rope).
constraint_metric(abelian_extension_limit, intensity, 0.30).
constraint_metric(abelian_extension_limit, suppression_requirement, 0.12).
constraint_metric(abelian_extension_limit, snap_back_potential, 0.05).
constraint_metric(abelian_extension_limit, extractiveness, 0.28).

% TANGLED ROPE: Artin Reciprocity Friction (Complexity of the Artin map mapping)
constraint_claim(artin_map_complexity, tangled_rope).
constraint_metric(artin_map_complexity, intensity, 0.55).
constraint_metric(artin_map_complexity, suppression_requirement, 0.35).
constraint_metric(artin_map_complexity, snap_back_potential, 0.45).
constraint_metric(artin_map_complexity, extractiveness, 0.52).

% NOOSE: Non-Abelian Obstruction (The limit of standard Class Field Theory)
constraint_claim(non_abelian_logic_gap, noose).
constraint_metric(non_abelian_logic_gap, intensity, 0.95).
constraint_metric(non_abelian_logic_gap, suppression_requirement, 0.88).
constraint_metric(non_abelian_logic_gap, snap_back_potential, 0.92).
constraint_metric(non_abelian_logic_gap, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_the_quadratic_reciprocity_law_as_foundational_symmetry).
affects_constraint(rec_01, quadratic_symmetry_invariance).

recommendation(rec_02, maintain_abelian_frameworks_for_local_global_consistency).
affects_constraint(rec_02, abelian_extension_limit).

recommendation(rec_03, reform_ideal_class_mapping_via_artin_map_scaffold).
affects_constraint(rec_03, artin_map_complexity).

recommendation(rec_04, cut_abelian_limitations_via_class_field_scaffold).
affects_constraint(rec_04, non_abelian_logic_gap).

veto_actor(elementary_number_theorist).
veto_exposed(elementary_number_theorist, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.60, 0.40, 0.50]).
measurement(10, [0.40, 0.99, 0.98, 0.92]).

% --- 6. Intent Evidence ---
intent_alternative(kronecker_weber_theorem, limited_to_rational_extensions).
intent_alternative(local_reciprocity, focus_on_p_adic_fields).
intent_beneficiary(arithmetic_geometers, unification_of_number_fields_and_function_fields).
intent_power_delta(langlands_program_vs_classical_number_theory, high_abstraction_dominance).
