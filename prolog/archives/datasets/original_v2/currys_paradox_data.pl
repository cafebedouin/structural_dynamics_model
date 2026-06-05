% 1. ENTITIES & INTERVALS
entity(logical_framework, system).
entity(self_referential_domain, manifold).
entity(derivation_path, sensor_path).
entity(stratified_logic_scaffold, scaffold).

interval(proof_steps, 0, 5).
interval(logical_explosion, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, sentence_construction, 0, [form, if_C_then_Y]).
event(e2, arbitrary_conclusion, 4, [conclusion, Y_is_true]).

% Omega Variable: Conceptual (The validity of the contraction rule in natural deduction)
omega_variable(omega_c1, conceptual, validity_of_unrestricted_contraction_in_self_referential_contexts).

% Omega Variable: Preference (Choosing between losing bivalence or losing the deduction theorem)
omega_variable(omega_p1, preference, tolerance_for_paracomplete_vs_paraconsistent_logic_resolutions).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Conditional Identity (The Mountain)
% The definition of the sentence C as equivalent to (C -> Y). This is the fixed anchor of the paradox.
constraint_claim(reflexive_conditional, mountain).
constraint_metric(reflexive_conditional, intensity, 1.0).
constraint_metric(reflexive_conditional, suppression_requirement, 0.05).
constraint_metric(reflexive_conditional, snap_back_potential, 0.0).
constraint_metric(reflexive_conditional, extractiveness, 0.01).

% The Modus Ponens Loop (The Tangled Rope)
% The process of assuming C, deriving (C -> Y), then concluding Y. 
% Extractiveness at 0.58 triggers a Reform recommendation for structural rules.
constraint_claim(recursive_modus_ponens, tangled_rope).
constraint_metric(recursive_modus_ponens, intensity, 0.82).
constraint_metric(recursive_modus_ponens, suppression_requirement, 0.40).
constraint_metric(recursive_modus_ponens, snap_back_potential, 0.70).
constraint_metric(recursive_modus_ponens, extractiveness, 0.58).

% The Principle of Explosion (The Snare)
% The ability to prove *anything* (Y) regardless of its truth value.
% Extractiveness at 0.99 requires the 'stratified_logic_scaffold' to resolve.
constraint_claim(logical_trivialization, snare).
constraint_metric(logical_trivialization, intensity, 0.99).
constraint_metric(logical_trivialization, suppression_requirement, 0.98).
constraint_metric(logical_trivialization, snap_back_potential, 0.01).
constraint_metric(logical_trivialization, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.58 extractiveness) triggers Reform
recommendation(rec_01, implement_linear_logic_to_restrict_contraction_rules).
affects_constraint(rec_01, recursive_modus_ponens).

% Snare (0.99 extractiveness) triggers Cut
% Utilizing 'stratified_logic_scaffold' to separate the truth-predicate from the sentence level.
recommendation(rec_02, adopt_tarskian_hierarchy_to_prevent_unguarded_self_reference).
affects_constraint(rec_02, logical_trivialization).

veto_actor(deduction_theorem_standard).
veto_exposed(deduction_theorem_standard, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (derivation power) is absolute, but Stability and Utility collapse once any Y is provable.
measurement(0, [1.00, 1.00, 0.90, 0.80]).
measurement(5, [1.00, 0.00, 0.00, 0.00]).

% 6. INTENT EVIDENCE
% Alternative: Liar Paradox (Discarded: Curry's does not require negation, only implication)
% Beneficiaries: Proof theorists and designers of formal verification languages
% Power Delta: Implicit Triviality (Self-reference vs Logical consistency)
intent_evidence(unrestricted_comprehension_failure, high_delta).
