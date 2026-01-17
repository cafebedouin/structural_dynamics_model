% =============================================================================
% DOMAIN: Axiom of Choice (ZFC Set Theory)
% MISSION: Clinical Sensor Output [v3.1.8]
% =============================================================================

% 1. ENTITIES & INTERVALS
entity(choice_function, system).
entity(zfc_universe, manifold).
entity(hamel_basis, scaffold).

% FIXED: Primary interval only to ensure full measurement repair.
interval(axiom_of_choice_audit, 0, 10).

% 2. EVENTS & OMEGA VARIABLES
event(finite_choice, initialization, 0, [provable_zf]).
event(infinite_product, processing, 5, [requires_ac]).
event(paradox_discovery, termination, 10, [banach_tarski]).

% OMEGA CLASSIFICATION
omega_variable(omega_c1, conceptual, 'Non-constructive existence vs effective calculability.').
omega_variable(omega_p1, preference, 'Tolerance for Banach-Tarski outcomes vs constructive limits.').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% Mountain (■C): suppression <= 0.05, snap = 0.0
constraint_claim(non_empty_requirement, mountain).
constraint_metric(non_empty_requirement, suppression_requirement, 0.05).
constraint_metric(non_empty_requirement, snap_back_potential, 0.0).

% Tangled Rope (Boxed T): ext 0.36-0.65
constraint_claim(infinite_selection_complexity, tangled_rope).
constraint_metric(infinite_selection_complexity, extractiveness, 0.45).
constraint_metric(infinite_selection_complexity, suppression_requirement, 0.30).

% Noose (Boxed C): ext >= 0.66, supp >= 0.46
constraint_claim(paradoxical_decomposition_potential, noose).
constraint_metric(paradoxical_decomposition_potential, extractiveness, 0.96).
constraint_metric(paradoxical_decomposition_potential, suppression_requirement, 0.94).

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(rec_01, 'Utilize Zorns Lemma for maximal element search.').
affects_constraint(rec_01, infinite_selection_complexity).

recommendation(rec_02, 'Restrict selection to constructible sets via hamel_basis scaffold.').
affects_constraint(rec_02, paradoxical_decomposition_potential).

veto_actor(godel_consistency_theorem).
veto_exposed(godel_consistency_theorem, rec_02).

% 5. MEASUREMENTS [Agency, Stability, Utility, Resilience]
measurement(0, [1.00, 1.00, 0.80, 1.00]).
measurement(10, [1.00, 0.20, 0.99, 0.95]).

% 6. INTENT EVIDENCE (Scenario Manager Standard)
% FIXED: intent_fact/4 ensures the Aggregate Magnitude (Kappa) can be calculated.
intent_fact(viable_alternative, axiom_of_choice_audit, axiom_of_determinacy, 0.40).
intent_fact(beneficiary, analysts, mathematical_utility, 1.0).
intent_fact(power_change, non_constructive_existence, constructive_rules, 0.90).
