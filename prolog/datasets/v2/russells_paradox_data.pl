% --- 1. Entities & Intervals ---
entity(naive_set_theory, process).
entity(the_set_r, group).
entity(logical_axioms, group).
entity(zermelo_fraenkel_scaffold, scaffold).
entity(type_theory_scaffold, scaffold).

interval(crisis_lifespan, 0, 10).

% --- 2. Events & Omega Variables ---
event(frege_grundgesetze_input, initialization, 0, [actor(gottlob_frege), property(unrestricted_comprehension)]).
event(discovery_of_r, processing, 3, [actor(bertrand_russell), action(self_membership_check)]).
event(contradiction_emergence, conflict, 5, [state(logical_explosion), property(r_member_of_r_iff_not_r_member_of_r)]).
event(axiomatic_reconstruction, termination, 10, [condition(restricted_comprehension)]).

omega_variable(omega_1, conceptual, definition_of_set_membership_for_universal_collections).
omega_variable(omega_2, preference, prioritizing_consistency_over_comprehension_intuitive_reach).

% --- 3. Constraint Claims & Kinetic Metrics ---
constraint_claim(binary_logic_rigidity, mountain).
constraint_metric(binary_logic_rigidity, intensity, 0.05).
constraint_metric(binary_logic_rigidity, suppression_requirement, 0.01).
constraint_metric(binary_logic_rigidity, snap_back_potential, 0.00).
constraint_metric(binary_logic_rigidity, extractiveness, 0.02).

constraint_claim(unrestricted_comprehension_axiom, noose).
constraint_metric(unrestricted_comprehension_axiom, intensity, 0.98).
constraint_metric(unrestricted_comprehension_axiom, suppression_requirement, 0.85).
constraint_metric(unrestricted_comprehension_axiom, snap_back_potential, 0.92).
constraint_metric(unrestricted_comprehension_axiom, extractiveness, 0.95).

constraint_claim(predicative_definition_requirement, tangled_rope).
constraint_metric(predicative_definition_requirement, intensity, 0.62).
constraint_metric(predicative_definition_requirement, suppression_requirement, 0.38).
constraint_metric(predicative_definition_requirement, snap_back_potential, 0.45).
constraint_metric(predicative_definition_requirement, extractiveness, 0.58).

constraint_claim(membership_well_foundedness, rope).
constraint_metric(membership_well_foundedness, intensity, 0.30).
constraint_metric(membership_well_foundedness, suppression_requirement, 0.12).
constraint_metric(membership_well_foundedness, snap_back_potential, 0.08).
constraint_metric(membership_well_foundedness, extractiveness, 0.28).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, cut_unrestricted_comprehension_via_zfc_separation).
affects_constraint(rec_01, unrestricted_comprehension_axiom).

recommendation(rec_02, reform_logic_hierarchy_via_ramified_types).
affects_constraint(rec_02, predicative_definition_requirement).

recommendation(rec_03, maintain_the_law_of_excluded_middle).
affects_constraint(rec_03, binary_logic_rigidity).

veto_actor(naive_platonist).
veto_exposed(naive_platonist, rec_01).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.95, 0.10, 0.90, 0.05]).
measurement(10, [0.40, 0.98, 0.85, 0.90]).

% --- 6. Intent Evidence ---
intent_alternative(quines_new_foundations, stratified_comprehension).
intent_alternative(intuitionistic_logic, rejecting_excluded_middle).
intent_beneficiary(mathematicians, safe_foundation_for_analysis).
intent_power_delta(systemic_rigor_vs_intuitive_freedom, high_foundational_shift).
