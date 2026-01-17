% --- 1. Entities & Intervals ---
entity(formal_system_f, process).
entity(arithmetic_axioms, group).
entity(godel_sentence_g, group).
entity(metamathematical_scaffold, scaffold).
entity(recursive_proof_scaffold, scaffold).

interval(derivation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(arithmetization_of_syntax, initialization, 1, [actor(kurt_godel), property(godel_numbering)]).
event(self_reference_construction, processing, 4, [action(diagonal_lemma), state(unprovability_assertion)]).
event(consistency_statement_mapping, update, 7, [action(second_theorem), property(unprovability_of_consistency)]).
event(foundational_limitation, termination, 10, [condition(syntactic_incompleteness)]).

omega_variable(omega_1, conceptual, definition_of_truth_vs_provability_in_formal_logic).
omega_variable(omega_2, preference, adherence_to_hilberts_program_post_1931).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(logical_consistency_rigidity, mountain).
constraint_metric(logical_consistency_rigidity, intensity, 0.05).
constraint_metric(logical_consistency_rigidity, suppression_requirement, 0.01).
constraint_metric(logical_consistency_rigidity, snap_back_potential, 0.00).
constraint_metric(logical_consistency_rigidity, extractiveness, 0.02).

constraint_claim(recursive_enumerability, rope).
constraint_metric(recursive_enumerability, intensity, 0.30).
constraint_metric(recursive_enumerability, suppression_requirement, 0.12).
constraint_metric(recursive_enumerability, snap_back_potential, 0.05).
constraint_metric(recursive_enumerability, extractiveness, 0.28).

constraint_claim(provability_predicate_complexity, tangled_rope).
constraint_metric(provability_predicate_complexity, intensity, 0.60).
constraint_metric(provability_predicate_complexity, suppression_requirement, 0.35).
constraint_metric(provability_predicate_complexity, snap_back_potential, 0.50).
constraint_metric(provability_predicate_complexity, extractiveness, 0.55).

constraint_claim(absolute_completeness_noose, noose).
constraint_metric(absolute_completeness_noose, intensity, 0.98).
constraint_metric(absolute_completeness_noose, suppression_requirement, 0.85).
constraint_metric(absolute_completeness_noose, snap_back_potential, 0.90).
constraint_metric(absolute_completeness_noose, extractiveness, 0.95).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_syntactic_separation_of_object_and_metalanguage).
affects_constraint(rec_01, recursive_enumerability).

recommendation(rec_02, reform_axiomatic_expansions_via_transfinite_induction).
affects_constraint(rec_02, provability_predicate_complexity).

recommendation(rec_03, cut_the_expectation_of_internal_consistency_proofs).
affects_constraint(rec_03, absolute_completeness_noose).

veto_actor(hilbert_formalist).
veto_exposed(hilbert_formalist, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.95, 0.80, 0.70, 0.60]).
measurement(10, [0.15, 0.99, 0.20, 0.10]).

% --- 6. Intent Evidence ---
intent_alternative(gentzens_consistency_proof, requires_higher_order_logic).
intent_alternative(tarskis_undefinability, semantic_limitation).
intent_beneficiary(meta_mathematicians, mapping_the_boundaries_of_formal_systems).
intent_power_delta(intuition_vs_formalism, high_epistemological_disruption).
