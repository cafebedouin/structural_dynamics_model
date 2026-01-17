% --- 1. Entities & Intervals ---
entity(group_classification_process, process).
entity(finite_simple_group_atlas, group).
entity(sporadic_groups, group).
entity(minimal_simple_groups, group).
entity(inductive_step_scaffold, scaffold).
entity(signalizer_functor_scaffold, scaffold).

interval(proof_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(feit_thompson_theorem, initialization, 1, [property(odd_order_solvability)]).
event(gorenstein_program_launch, processing, 3, [action(structural_stratification)]).
event(quasi_thin_case_resolution, update, 7, [action(aschbacher_smith_verification)]).
event(atlas_finalization, termination, 10, [condition(exhaustive_list_verification)]).

omega_variable(omega_1, empirical, length_and_error_distribution_of_original_distributed_proof_text).
omega_variable(omega_2, conceptual, definition_of_computational_minimality_for_cross_verification).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Parity Invariance (Feit-Thompson odd order result is non-negotiable)
constraint_claim(odd_order_solvability, mountain).
constraint_metric(odd_order_solvability, intensity, 0.05).
constraint_metric(odd_order_solvability, suppression_requirement, 0.05).
constraint_metric(odd_order_solvability, snap_back_potential, 0.00).
constraint_metric(odd_order_solvability, extractiveness, 0.02).

% ROPE: Prime Power Order Norms (Sylow-based structural constraints)
constraint_claim(sylow_power_tether, rope).
constraint_metric(sylow_power_tether, intensity, 0.32).
constraint_metric(sylow_power_tether, suppression_requirement, 0.12).
constraint_metric(sylow_power_tether, snap_back_potential, 0.05).
constraint_metric(sylow_power_tether, extractiveness, 0.30).

% TANGLED ROPE: Component Type Identification (Difficulty in local subgroup analysis)
constraint_claim(local_subgroup_friction, tangled_rope).
constraint_metric(local_subgroup_friction, intensity, 0.58).
constraint_metric(local_subgroup_friction, suppression_requirement, 0.35).
constraint_metric(local_subgroup_friction, snap_back_potential, 0.40).
constraint_metric(local_subgroup_friction, extractiveness, 0.52).

% NOOSE: Uniqueness Theorem Verification (The 10,000+ page complexity wall)
constraint_claim(proof_volume_noose, noose).
constraint_metric(proof_volume_noose, intensity, 0.98).
constraint_metric(proof_volume_noose, suppression_requirement, 0.90).
constraint_metric(proof_volume_noose, snap_back_potential, 0.95).
constraint_metric(proof_volume_noose, extractiveness, 0.97).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_the_solvability_of_odd_order_groups_as_foundational).
affects_constraint(rec_01, odd_order_solvability).

recommendation(rec_02, maintain_sylow_and_hall_subgroup_consistency_checks).
affects_constraint(rec_02, sylow_power_tether).

recommendation(rec_03, reform_local_structural_analysis_via_signalizer_functor_scaffold).
affects_constraint(rec_03, local_subgroup_friction).

recommendation(rec_04, cut_proof_complexity_via_inductive_step_scaffold).
affects_constraint(rec_04, proof_volume_noose).

veto_actor(strict_single_volume_formalist).
veto_exposed(strict_single_volume_formalist, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.40, 0.20, 0.50]).
measurement(10, [0.35, 0.99, 0.98, 0.95]).

% --- 6. Intent Evidence ---
intent_alternative(sporadic_group_exclusion, logically_incomplete_system).
intent_alternative(revision_second_generation_proof, utility_optimization).
intent_beneficiary(finite_algebraists, absolute_dictionary_of_simple_structures).
intent_power_delta(induction_logic_vs_combinatorial_chaos, high_structural_determinism).
