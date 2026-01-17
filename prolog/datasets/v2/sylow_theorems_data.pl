% =============================================================================
% DOMAIN: Sylow Theorems (Finite Group Theory)
% MISSION: Clinical Sensor Output [v3.1.8]
% =============================================================================

% 1. ENTITIES & INTERVALS
entity(finite_group_theory, process).
entity(sylow_p_subgroups, group).
entity(coset_decomposition_scaffold, scaffold).
entity(group_action_scaffold, scaffold).

interval(derivation_lifespan, 0, 10).

% 2. EVENTS & OMEGA VARIABLES
event(order_factorization, initialization, 0, [order_n, p_power_m]).
event(existence_verification, processing, 3, [first_sylow_theorem]).
event(conjugacy_mapping, update, 6, [second_sylow_theorem]).
event(congruence_calculation, processing, 8, [third_sylow_theorem]).

% Omega Classification
omega_variable(omega_1, empirical, 'Exact computation of subgroup counts for high-order sporadic groups.').
omega_variable(omega_2, conceptual, 'Definition of optimal representation for non-abelian p-groups.').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% Mountain (■C): suppression <= 0.05, snap = 0.0
constraint_claim(prime_order_rigidity, mountain).
constraint_metric(prime_order_rigidity, suppression_requirement, 0.05).
constraint_metric(prime_order_rigidity, snap_back_potential, 0.00).

% Rope (⊞C): ext <= 0.35, supp <= 0.15
constraint_claim(subgroup_presence_tether, rope).
constraint_metric(subgroup_presence_tether, extractiveness, 0.28).
constraint_metric(subgroup_presence_tether, suppression_requirement, 0.12).

% Tangled Rope (Boxed T): ext 0.36-0.65
constraint_claim(congruence_calculation_friction, tangled_rope).
constraint_metric(congruence_calculation_friction, extractiveness, 0.52).
constraint_metric(congruence_calculation_friction, suppression_requirement, 0.35).

% Noose (Boxed C): ext >= 0.66, supp >= 0.46
constraint_claim(non_abelian_simplicity_noose, noose).
constraint_metric(non_abelian_simplicity_noose, extractiveness, 0.94).
constraint_metric(non_abelian_simplicity_noose, suppression_requirement, 0.88).

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(rec_03, 'Reform counting logic via the group_action_scaffold.').
affects_constraint(rec_03, congruence_calculation_friction).

recommendation(rec_04, 'Cut classification deadlocks via the coset_decomposition_scaffold.').
affects_constraint(rec_04, non_abelian_simplicity_noose).

veto_actor(infinite_group_specialist).
veto_exposed(infinite_group_specialist, rec_03).

% 5. MEASUREMENTS [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.60, 0.40, 0.50]).
measurement(10, [0.40, 0.99, 0.98, 0.92]).

% 6. INTENT EVIDENCE (Standardized intent_fact/4)
% Grouped together to prevent "Clauses not together" warnings.
intent_fact(viable_alternative, derivation_lifespan, hall_subgroups, 0.70).
intent_fact(beneficiary, group_theorists, classification_of_simple_groups, 1.0).
intent_fact(power_change, prime_arithmetic, structural_symmetry, 0.90).
