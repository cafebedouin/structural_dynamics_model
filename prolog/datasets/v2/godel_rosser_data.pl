% 1. ENTITIES & INTERVALS
entity(formal_system_p, system).
entity(provability_manifold, manifold).
entity(diagonal_construction_path, sensor_path).
entity(rosser_predicate_scaffold, scaffold).

interval(derivation_depth, 0, 100).
interval(fixed_point_calculation, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, numbering_injection, 0, [technique, arithmetic_encoding]).
event(e2, rosser_sentence_emergence, 1, [form, rho]).

% Omega Variable: Conceptual (Defining 'truth' for undecidable arithmetic sentences)
omega_variable(omega_c1, conceptual, semantic_interpretation_of_formally_undecidable_propositions).

% Omega Variable: Empirical (The length of the Rosser sentence compared to the standard Gödel sentence)
omega_variable(omega_e1, empirical, growth_of_expression_length_in_nested_negation_checks).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Recursive Enumerability (The Mountain)
% The set of axioms must be effectively enumerable; this provides the invariant foundation.
constraint_claim(axiomatic_recursiveness, mountain).
constraint_metric(axiomatic_recursiveness, intensity, 1.0).
constraint_metric(axiomatic_recursiveness, suppression_requirement, 0.05).
constraint_metric(axiomatic_recursiveness, snap_back_potential, 0.0).
constraint_metric(axiomatic_recursiveness, extractiveness, 0.01).

% The Proof-Checker Logic (The Tangled Rope)
% The complexity of the "for every proof of me, there is a smaller proof of my negation" logic.
% Extractiveness at 0.62 triggers a Reform recommendation for predicate simplification.
constraint_claim(rosser_ordering_entanglement, tangled_rope).
constraint_metric(rosser_ordering_entanglement, intensity, 0.88).
constraint_metric(rosser_ordering_entanglement, suppression_requirement, 0.44).
constraint_metric(rosser_ordering_entanglement, snap_back_potential, 0.80).
constraint_metric(rosser_ordering_entanglement, extractiveness, 0.62).

% The Incompleteness Barrier (The Noose)
% The formal proof that neither Rho nor its negation can be proven in P.
% Extractiveness at 0.99 requires the 'rosser_predicate_scaffold' to achieve undecidability.
constraint_claim(complete_semantic_exhaustion, noose).
constraint_metric(complete_semantic_exhaustion, intensity, 0.99).
constraint_metric(complete_semantic_exhaustion, suppression_requirement, 0.95).
constraint_metric(complete_semantic_exhaustion, snap_back_potential, 0.01).
constraint_metric(complete_semantic_exhaustion, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.62 extractiveness) triggers Reform
recommendation(rec_01, utilize_witness_comparison_shorthand_for_rosser_predicates).
affects_constraint(rec_01, rosser_ordering_entanglement).

% Noose (0.99 extractiveness) triggers Cut
% Utilizing 'rosser_predicate_scaffold' to replace the standard Prov(x) with the Rosser-Prov(x).
recommendation(rec_02, abandon_omega_consistency_requirement_via_rosser_transformation).
affects_constraint(rec_02, complete_semantic_exhaustion).

veto_actor(consistency_axiom_p).
veto_exposed(consistency_axiom_p, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (expression) remains 1.0; Stability of the system's completeness vanishes.
measurement(0, [1.00, 1.00, 0.90, 1.00]).
measurement(1, [1.00, 0.00, 0.15, 0.05]).

% 6. INTENT EVIDENCE
% Alternative: Standard Gödel (Discarded: fails to reach undecidability without omega-consistency)
% Beneficiaries: Proof theorists and logic auditors (verifying system boundaries)
% Power Delta: Proof Length Comparison (Standard derivation vs Rosser ordering)
intent_evidence(strengthened_incompleteness, high_delta).
