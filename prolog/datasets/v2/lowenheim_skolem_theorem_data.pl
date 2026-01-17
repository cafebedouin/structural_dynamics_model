% 1. ENTITIES & INTERVALS
entity(first_order_theory_t, system).
entity(model_continuum, manifold).
entity(elementary_mapping, sensor_path).
entity(tarski_vaught_test, scaffold).

interval(cardinality_expansion, 0, 10).
interval(submodel_reduction, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, downward_reduction, 0, [target_cardinality, aleph_zero]).
event(e2, upward_expansion, 10, [target_cardinality, beth_two]).

% Omega Variable: Conceptual (The nature of 'categoricity' in first-order logic)
omega_variable(omega_c1, conceptual, inability_of_first_order_logic_to_fix_a_unique_infinite_cardinality).

% Omega Variable: Empirical (The limits of human intuition regarding non-isomorphic models of the same axioms)
omega_variable(omega_e1, empirical, difficulty_of_differentiating_uncountable_models_via_finite_syntax).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Logical Consistency (The Mountain)
% If a theory has an infinite model, it has models of every infinite cardinality. 
% This is a rigid meta-logical theorem; suppression is impossible.
constraint_claim(cardinal_flexibility, mountain).
constraint_metric(cardinal_flexibility, intensity, 1.0).
constraint_metric(cardinal_flexibility, suppression_requirement, 0.05).
constraint_metric(cardinal_flexibility, snap_back_potential, 0.0).
constraint_metric(cardinal_flexibility, extractiveness, 0.01).

% The Elementary Embedding (The Tangled Rope)
% The requirement that submodels/supermodels satisfy the same first-order sentences.
% Extractiveness at 0.40 triggers a Reform recommendation for structural mapping.
constraint_claim(elementary_equivalence, tangled_rope).
constraint_metric(elementary_equivalence, intensity, 0.70).
constraint_metric(elementary_equivalence, suppression_requirement, 0.30).
constraint_metric(elementary_equivalence, snap_back_potential, 0.50).
constraint_metric(elementary_equivalence, extractiveness, 0.40).

% The Categoricity Failure (The Noose)
% The realization that first-order theories cannot be categorical in infinite powers.
% This "strangles" the hope of defining a unique structure (like N or R) using only first-order logic.
% Extractiveness at 0.85 requires the 'tarski_vaught_test' scaffold to resolve submodel existence.
constraint_claim(non_categoricity_trap, noose).
constraint_metric(non_categoricity_trap, intensity, 0.92).
constraint_metric(non_categoricity_trap, suppression_requirement, 0.80).
constraint_metric(non_categoricity_trap, snap_back_potential, 0.05).
constraint_metric(non_categoricity_trap, extractiveness, 0.85).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.40 extractiveness) triggers Reform
recommendation(rec_01, utilize_back_and_forth_method_for_isomorphism_testing).
affects_constraint(rec_01, elementary_equivalence).

% Noose (0.85 extractiveness) triggers Cut
% Utilizing 'tarski_vaught_test' scaffold to ensure the submodel is elementary.
recommendation(rec_02, adopt_second_order_logic_to_restore_categorical_definition).
affects_constraint(rec_02, non_categoricity_trap).

veto_actor(compactness_theorem_limit).
veto_exposed(compactness_theorem_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency in choosing models is high, but Stability of 'uniqueness' is zero.
measurement(0, [1.00, 1.00, 0.85, 0.90]).
measurement(10, [1.00, 0.00, 0.95, 0.70]).

% 6. INTENT EVIDENCE
% Alternative: Morley's Categoricity Theorem (Discarded: applies to specific omega-stable theories only)
% Beneficiaries: Model theorists and structuralists examining the limits of axiomatic systems
% Power Delta: Quantifier Scope (Internal satisfaction vs External cardinality)
intent_evidence(lowenheim_skolem_power, high_delta).
