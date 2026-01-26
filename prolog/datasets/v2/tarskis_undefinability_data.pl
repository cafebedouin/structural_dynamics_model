% 1. ENTITIES & INTERVALS
entity(formal_language_L, system).
entity(truth_predicate_space, manifold).
entity(diagonalization_path, sensor_path).
entity(metalanguage_M, scaffold).

interval(language_hierarchy, 0, 1). % 0: Object Language, 1: Metalanguage
interval(proof_derivation, 0, 10).

% 2. EVENTS & OMEGA VARIABLES
event(e1, godel_numbering_assignment, 0, [encoding, arithmetic]).
event(e2, liar_sentence_construction, 5, [form, not_true_phi]).

% Omega Variable: Conceptual (The nature of 'truth' vs 'provability')
omega_variable(omega_c1, conceptual, distinction_between_semantic_truth_and_syntactic_derivability).

% Omega Variable: Empirical (The capacity of a system to represent its own syntax)
omega_variable(omega_e1, empirical, computational_limits_of_nested_godel_encodings).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Expressive Adequacy (The Mountain)
% The language L must be strong enough to represent its own syntax (e.g., via Robinson Arithmetic).
% This is the fixed anchor required for the theorem to hold.
constraint_claim(syntactic_expressivity, mountain).
constraint_metric(syntactic_expressivity, intensity, 1.0).
constraint_metric(syntactic_expressivity, suppression_requirement, 0.05).
constraint_metric(syntactic_expressivity, snap_back_potential, 0.0).
constraint_metric(syntactic_expressivity, extractiveness, 0.01).

% The Diagonal Lemma Entanglement (The Tangled Rope)
% The mechanism that allows the construction of a sentence that refers to its own truth value.
% Extractiveness at 0.55 triggers a Reform recommendation for hierarchy definition.
constraint_claim(diagonal_self_reference, tangled_rope).
constraint_metric(diagonal_self_reference, intensity, 0.85).
constraint_metric(diagonal_self_reference, suppression_requirement, 0.40).
constraint_metric(diagonal_self_reference, snap_back_potential, 0.70).
constraint_metric(diagonal_self_reference, extractiveness, 0.55).

% The Semantic Closure Collapse (The Snare)
% The assumption that a language can define its own truth predicate leads to a contradiction.
% Extractiveness at 0.99 requires the 'metalanguage_M' scaffold to resolve.
constraint_claim(semantic_self_closure, snare).
constraint_metric(semantic_self_closure, intensity, 0.99).
constraint_metric(semantic_self_closure, suppression_requirement, 0.97).
constraint_metric(semantic_self_closure, snap_back_potential, 0.01).
constraint_metric(semantic_self_closure, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.55 extractiveness) triggers Reform
recommendation(rec_01, implement_stratified_truth_predicates_per_language_level).
affects_constraint(rec_01, diagonal_self_reference).

% Snare (0.99 extractiveness) triggers Cut
% Utilizing 'metalanguage_M' scaffold to define truth for L in a higher system.
recommendation(rec_02, abandon_internal_truth_definition_for_external_metalanguage).
affects_constraint(rec_02, semantic_self_closure).

veto_actor(arithmetical_completeness_standard).
veto_exposed(arithmetical_completeness_standard, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (expression) is high, but Stability collapses if the truth predicate is internalized.
measurement(0, [1.00, 1.00, 0.90, 0.80]).
measurement(1, [1.00, 0.00, 0.20, 0.10]).

% 6. INTENT EVIDENCE
% Alternative: Kripke's Theory of Truth (Discarded: allows partial truth, but Tarski addresses total definitions)
% Beneficiaries: Formal logicians and semanticists (understanding limits of formal systems)
% Power Delta: Metalanguage Shift (Object Level vs Meta Level)
intent_evidence(reducio_ad_absurdum_truth, high_delta).
