% --- 1. Entities & Intervals ---
entity(intuitionistic_logic, process).
entity(constructive_mathematics, group).
entity(choice_sequences, group).
entity(law_of_excluded_middle_scaffold, scaffold).
entity(brouwer_fixed_point_scaffold, scaffold).

interval(foundation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(rejection_of_lem, initialization, 1, [actor(l_e_j_brouwer), property(non_constructive_negation)]).
event(fixed_point_theorem, processing, 4, [action(topological_mapping), state(existence_proof)]).
event(choice_sequence_introduction, update, 7, [action(infinite_process_definition), property(temporal_growth)]).
event(formalism_conflict, termination, 10, [condition(foundational_split_with_hilbert)]).

omega_variable(omega_1, conceptual, definition_of_mathematical_existence_as_mental_construction).
omega_variable(omega_2, preference, prioritizing_subjective_evidence_over_objective_formalism).

% --- 3. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: Topological Invariance (Brouwer Fixed Point)
constraint_claim(topological_mapping_invariance, mountain).
constraint_metric(topological_mapping_invariance, intensity, 0.05).
constraint_metric(topological_mapping_invariance, suppression_requirement, 0.05).
constraint_metric(topological_mapping_invariance, snap_back_potential, 0.0).
constraint_metric(topological_mapping_invariance, extractiveness, 0.03).

% ROPE: Constructive Proof Standards
constraint_claim(constructive_existence_requirement, rope).
constraint_metric(constructive_existence_requirement, intensity, 0.35).
constraint_metric(constructive_existence_requirement, suppression_requirement, 0.15).
constraint_metric(constructive_existence_requirement, snap_back_potential, 0.05).
constraint_metric(constructive_existence_requirement, extractiveness, 0.32).

% TANGLED ROPE: Choice Sequence Indeterminacy
constraint_claim(continuum_indeterminacy_friction, tangled_rope).
constraint_metric(continuum_indeterminacy_friction, intensity, 0.58).
constraint_metric(continuum_indeterminacy_friction, suppression_requirement, 0.32).
constraint_metric(continuum_indeterminacy_friction, snap_back_potential, 0.40).
constraint_metric(continuum_indeterminacy_friction, extractiveness, 0.55).

% NOOSE: Law of Excluded Middle (LEM) in Infinite Sets
constraint_claim(classical_logic_trap, noose).
constraint_metric(classical_logic_trap, intensity, 0.95).
constraint_metric(classical_logic_trap, suppression_requirement, 0.85).
constraint_metric(classical_logic_trap, snap_back_potential, 0.90).
constraint_metric(classical_logic_trap, extractiveness, 0.92).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_fixed_point_rigidity_in_topology).
affects_constraint(rec_01, topological_mapping_invariance).

recommendation(rec_02, maintain_computable_algorithms_for_existence_claims).
affects_constraint(rec_02, constructive_existence_requirement).

recommendation(rec_03, reform_continuum_models_via_choice_sequence_scaffold).
affects_constraint(rec_03, continuum_indeterminacy_friction).

recommendation(rec_04, cut_lem_dependence_via_intuitionistic_logic_scaffold).
affects_constraint(rec_04, classical_logic_trap).

veto_actor(hilbert_formalist).
veto_exposed(hilbert_formalist, rec_04).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.95, 0.80, 0.60, 0.70]).
measurement(10, [0.30, 0.98, 0.15, 0.05]).

% --- 6. Intent Evidence ---
intent_alternative(classical_analysis, high_utility_low_constructivity).
intent_alternative(bishop_constructivism, pragmatic_relaxation).
intent_beneficiary(computational_theorists, mapping_effective_calculability).
intent_power_delta(mental_intuition_vs_symbolic_manipulation, high_epistemological_friction).
