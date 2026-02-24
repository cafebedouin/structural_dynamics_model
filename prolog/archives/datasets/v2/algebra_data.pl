% --- 1. Entities & Intervals ---
entity(complex_field, process).
entity(polynomial_ring, group).
entity(degree_n_expression, group).
entity(splitting_field_scaffold, scaffold).
entity(analytic_continuation_scaffold, scaffold).

interval(derivation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(d_alembert_conjecture, initialization, 0, [actor(jean_le_rond_d_alembert), property(existence_of_root)]).
event(gauss_first_proof, processing, 3, [actor(carl_friedrich_gauss), action(geometric_algebraic_hybrid)]).
event(liouvilles_application, update, 7, [action(bounded_entire_function_check), state(analytic_proof)]).
event(topological_closure, termination, 10, [condition(algebraic_completeness_of_c)]).

omega_variable(omega_1, conceptual, definition_of_algebraic_vs_analytic_purity_in_proofs).
omega_variable(omega_2, empirical, exact_computational_complexity_of_root_approximation_for_arbitrary_n).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Field Completeness of C
constraint_claim(complex_algebraic_closure, mountain).
constraint_metric(complex_algebraic_closure, intensity, 0.05).
constraint_metric(complex_algebraic_closure, suppression_requirement, 0.05).
constraint_metric(complex_algebraic_closure, snap_back_potential, 0.0).
constraint_metric(complex_algebraic_closure, extractiveness, 0.02).

% ROPE: Polynomial Degree Invariant
constraint_claim(degree_root_correspondence, rope).
constraint_metric(degree_root_correspondence, intensity, 0.32).
constraint_metric(degree_root_correspondence, suppression_requirement, 0.12).
constraint_metric(degree_root_correspondence, snap_back_potential, 0.05).
constraint_metric(degree_root_correspondence, extractiveness, 0.28).

% TANGLED ROPE: Non-Constructive Existence Friction
constraint_claim(existence_vs_construction_friction, tangled_rope).
constraint_metric(existence_vs_construction_friction, intensity, 0.58).
constraint_metric(existence_vs_construction_friction, suppression_requirement, 0.35).
constraint_metric(existence_vs_construction_friction, snap_back_potential, 0.40).
constraint_metric(existence_vs_construction_friction, extractiveness, 0.52).

% NOOSE: Real Field Incompleteness
constraint_claim(real_field_limit_noose, snare).
constraint_metric(real_field_limit_noose, intensity, 0.95).
constraint_metric(real_field_limit_noose, suppression_requirement, 0.88).
constraint_metric(real_field_limit_noose, snap_back_potential, 0.92).
constraint_metric(real_field_limit_noose, extractiveness, 0.94).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_the_algebraic_closure_of_complex_numbers_as_invariant).
affects_constraint(rec_01, complex_algebraic_closure).

recommendation(rec_02, maintain_degree_n_equals_n_roots_standard).
affects_constraint(rec_02, degree_root_correspondence).

recommendation(rec_03, reform_root_finding_logic_via_splitting_field_scaffold).
affects_constraint(rec_03, existence_vs_construction_friction).

recommendation(rec_04, cut_real_number_limitations_via_analytic_continuation_scaffold).
affects_constraint(rec_04, real_field_limit_noose).

veto_actor(strict_finitist_algebraist).
veto_exposed(strict_finitist_algebraist, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.60, 0.50, 0.70]).
measurement(10, [0.35, 0.99, 0.98, 0.92]).

% --- 6. Intent Evidence ---
intent_alternative(rouches_theorem, complex_analysis_utility).
intent_alternative(galois_theory, structural_extension).
intent_beneficiary(numerical_analysts, guarantee_of_solution_space).
intent_power_delta(complex_vs_real_analysis, total_systemic_convergence_dominance).
