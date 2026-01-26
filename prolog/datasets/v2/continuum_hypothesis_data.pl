% --- 1. Entities & Intervals ---
entity(cantorian_set_theory, process).
entity(aleph_zero, group).
entity(power_set_of_aleph_zero, group).
entity(zfc_axiomatic_framework, group).
entity(forcing_scaffold, scaffold).
entity(inner_model_scaffold, scaffold).

interval(foundation_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(diagonal_argument, initialization, 0, [actor(georg_cantor), property(uncountability_proof)]).
event(hilbert_first_problem, collection, 2, [actor(david_hilbert), priority(prime)]).
event(consistency_proof, processing, 4, [actor(kurt_godel), result(relative_consistency_with_l)]).
event(independence_proof, update, 7, [actor(paul_cohen), result(forcing_independence)]).
event(undecidability_consensus, termination, 10, [condition(logical_independence_from_zfc)]).

omega_variable(omega_1, conceptual, true_nature_of_the_continuum_size).
omega_variable(omega_2, empirical, existence_of_cardinalities_between_aleph_zero_and_c).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(ordinal_countability_rigidity, mountain).
constraint_metric(ordinal_countability_rigidity, intensity, 0.04).
constraint_metric(ordinal_countability_rigidity, suppression_requirement, 0.01).
constraint_metric(ordinal_countability_rigidity, snap_back_potential, 0.00).
constraint_metric(ordinal_countability_rigidity, extractiveness, 0.02).

constraint_claim(power_set_operation, rope).
constraint_metric(power_set_operation, intensity, 0.30).
constraint_metric(power_set_operation, suppression_requirement, 0.12).
constraint_metric(power_set_operation, snap_back_potential, 0.05).
constraint_metric(power_set_operation, extractiveness, 0.25).

constraint_claim(axiom_of_choice_alignment, tangled_rope).
constraint_metric(axiom_of_choice_alignment, intensity, 0.55).
constraint_metric(axiom_of_choice_alignment, suppression_requirement, 0.38).
constraint_metric(axiom_of_choice_alignment, snap_back_potential, 0.45).
constraint_metric(axiom_of_choice_alignment, extractiveness, 0.52).

constraint_claim(zfc_deductive_limit, snare).
constraint_metric(zfc_deductive_limit, intensity, 0.98).
constraint_metric(zfc_deductive_limit, suppression_requirement, 0.90).
constraint_metric(zfc_deductive_limit, snap_back_potential, 0.95).
constraint_metric(zfc_deductive_limit, extractiveness, 0.97).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, maintain_well_ordering_as_fundamental_to_cardinality).
affects_constraint(rec_01, power_set_operation).

recommendation(rec_02, reform_meta_mathematical_approach_via_forcing_axioms).
affects_constraint(rec_02, axiom_of_choice_alignment).

recommendation(rec_03, cut_zfc_sufficiency_for_higher_set_theoretic_questions).
affects_constraint(rec_03, zfc_deductive_limit).

veto_actor(strict_finitist).
veto_exposed(strict_finitist, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.80, 0.50, 0.60, 0.40]).
measurement(10, [0.10, 0.95, 0.10, 0.05]).

% --- 6. Intent Evidence ---
intent_alternative(woodin_v_equals_ultimate_l, high_complexity_reduction).
intent_alternative(martin_axiom, increased_consistency_flexibility).
intent_beneficiary(set_theorists, discovery_of_large_cardinal_axioms).
intent_power_delta(formal_logic_vs_mathematical_intuition, high_independence_gap).
