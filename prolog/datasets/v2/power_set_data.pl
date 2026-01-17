% --- 1. Entities & Intervals ---
entity(set_theory_foundation, process).
entity(finite_cardinality, group).
entity(infinite_cardinality, group).
entity(diagonal_argument_scaffold, scaffold).
entity(characteristic_function_scaffold, scaffold).

interval(derivation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(element_mapping, initialization, 1, [actor(cantor), property(one_to_one_correspondence)]).
event(diagonalization_phase, processing, 4, [action(negation_of_membership), state(contradiction)]).
event(cardinal_hierarchy_expansion, update, 8, [action(iterative_power_set_application)]).
event(formal_axiomatization, termination, 10, [condition(zfc_power_set_axiom)]).

omega_variable(omega_1, conceptual, definition_of_size_for_proper_classes_vs_sets).
omega_variable(omega_2, preference, acceptance_of_impredicative_definitions_in_large_cardinals).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(empty_set_uniqueness, mountain).
constraint_metric(empty_set_uniqueness, intensity, 0.05).
constraint_metric(empty_set_uniqueness, suppression_requirement, 0.01).
constraint_metric(empty_set_uniqueness, snap_back_potential, 0.00).
constraint_metric(empty_set_uniqueness, extractiveness, 0.02).

constraint_claim(finite_exponential_growth, rope).
constraint_metric(finite_exponential_growth, intensity, 0.30).
constraint_metric(finite_exponential_growth, suppression_requirement, 0.10).
constraint_metric(finite_exponential_growth, snap_back_potential, 0.05).
constraint_metric(finite_exponential_growth, extractiveness, 0.25).

constraint_claim(continuum_hypothesis_mapping, tangled_rope).
constraint_metric(continuum_hypothesis_mapping, intensity, 0.55).
constraint_metric(continuum_hypothesis_mapping, suppression_requirement, 0.38).
constraint_metric(continuum_hypothesis_mapping, snap_back_potential, 0.45).
constraint_metric(continuum_hypothesis_mapping, extractiveness, 0.60).

constraint_claim(absolute_infinity_containment, noose).
constraint_metric(absolute_infinity_containment, intensity, 0.98).
constraint_metric(absolute_infinity_containment, suppression_requirement, 0.90).
constraint_metric(absolute_infinity_containment, snap_back_potential, 0.95).
constraint_metric(absolute_infinity_containment, extractiveness, 0.97).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_ordinal_indexing_for_well_ordered_sets).
affects_constraint(rec_01, finite_exponential_growth).

recommendation(rec_02, reform_cardinal_arithmetic_via_forcing_axioms).
affects_constraint(rec_02, continuum_hypothesis_mapping).

recommendation(rec_03, cut_universal_set_comprehension_to_avoid_cantors_paradox).
affects_constraint(rec_03, absolute_infinity_containment).

veto_actor(strict_finitist).
veto_exposed(strict_finitist, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.90, 0.60, 0.75, 0.50]).
measurement(10, [0.20, 0.98, 0.95, 0.10]).

% --- 6. Intent Evidence ---
intent_alternative(recursive_construction, lower_complexity_bounds).
intent_alternative(type_theoretic_universes, stratified_containment).
intent_beneficiary(higher_order_logicians, exploration_of_uncountable_structures).
intent_power_delta(constructive_mathematics_vs_platonic_realism, high_ontological_friction).
