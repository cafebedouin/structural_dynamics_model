% 1. ENTITIES & INTERVALS
entity(program_index_space, system).
entity(semantic_property_set, manifold).
entity(reduction_path, sensor_path).
entity(trivial_property_identifier, scaffold).

interval(reduction_steps, 0, 500).
interval(decision_attempt, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, property_definition, 0, [type, non_trivial]).
event(e2, turing_reduction_start, 1, [target, halting_problem]).

% Omega Variable: Conceptual (Defining the boundary of 'semantic' vs 'syntactic' properties)
omega_variable(omega_c1, conceptual, distinction_between_code_structure_and_behavioral_output).

% Omega Variable: Preference (Tolerance for false positives in static analysis)
omega_variable(omega_p1, preference, acceptability_of_conservative_approximation_over_exact_truth).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Non-Triviality Requirement (The Mountain)
% If a property applies to some but not all programs, the logic is unyielding.
constraint_claim(non_triviality_clause, mountain).
constraint_metric(non_triviality_clause, intensity, 1.0).
constraint_metric(non_triviality_clause, suppression_requirement, 0.05).
constraint_metric(non_triviality_clause, snap_back_potential, 0.0).
constraint_metric(non_triviality_clause, extractiveness, 0.01).

% The Extensionality Entanglement (The Tangled Rope)
% Programs with different code but identical behavior must satisfy the same property.
% Extractiveness at 0.52 triggers a Reform recommendation for behavior modeling.
constraint_claim(extensional_equivalence, tangled_rope).
constraint_metric(extensional_equivalence, intensity, 0.78).
constraint_metric(extensional_equivalence, suppression_requirement, 0.35).
constraint_metric(extensional_equivalence, snap_back_potential, 0.70).
constraint_metric(extensional_equivalence, extractiveness, 0.52).

% The Undecidability Sink (The Noose)
% The impossibility of a general algorithm to decide semantic truth.
% Extractiveness at 0.98 requires the 'trivial_property_identifier' scaffold to escape.
constraint_claim(semantic_undecidability, noose).
constraint_metric(semantic_undecidability, intensity, 0.99).
constraint_metric(semantic_undecidability, suppression_requirement, 0.96).
constraint_metric(semantic_undecidability, snap_back_potential, 0.01).
constraint_metric(semantic_undecidability, extractiveness, 0.98).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.52 extractiveness) triggers Reform
recommendation(rec_01, utilize_abstract_interpretation_for_property_approximation).
affects_constraint(rec_01, extensional_equivalence).

% Noose (0.98 extractiveness) triggers Cut
% Utilizing 'trivial_property_identifier' scaffold to restrict scope to decidable syntax.
recommendation(rec_02, abandon_exact_behavioral_analysis_for_syntactic_heuristics).
affects_constraint(rec_02, semantic_undecidability).

veto_actor(computability_limit_axiom).
veto_exposed(computability_limit_axiom, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Utility is high for theory, but Agency (the ability to decide) is zero for non-trivial cases.
measurement(0, [0.50, 1.00, 0.80, 1.00]).
measurement(1, [0.00, 0.05, 0.90, 0.70]).

% 6. INTENT EVIDENCE
% Alternative: Decidability on finite-state machines (Discarded: violates TM universality)
% Beneficiaries: Compiler writers and security auditors (understanding tool limits)
% Power Delta: Semantic Mapping (Code space vs Behavior space)
intent_evidence(reduction_to_halting, high_delta).
