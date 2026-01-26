% 1. ENTITIES & INTERVALS
entity(zfc_first_order_theory, system).
entity(countable_lowenheim_model, manifold).
entity(element_enumeration_path, sensor_path).
entity(absolute_set_scaffold, scaffold).

interval(model_construction, 0, 1).
interval(cardinality_clash, 0, 5).

% 2. EVENTS & OMEGA VARIABLES
event(e1, countable_model_assignment, 0, [domain_cardinality, aleph_zero]).
event(e2, power_set_existence_proof, 2, [internal_claim, uncountable]).

% Omega Variable: Conceptual (The distinction between 'internal' and 'external' cardinality)
omega_variable(omega_c1, conceptual, validity_of_cardinality_as_an_absolute_vs_relational_property).

% Omega Variable: Empirical (The inability of first-order logic to 'see' all external bijections)
omega_variable(omega_e1, empirical, resolution_limit_of_quantification_over_uncountable_domains).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Lowenheim-Skolem Invariant (The Mountain)
% The Downward Lowenheim-Skolem theorem is a fixed meta-mathematical law for first-order logic.
constraint_claim(downward_ls_theorem, mountain).
constraint_metric(downward_ls_theorem, intensity, 1.0).
constraint_metric(downward_ls_theorem, suppression_requirement, 0.05).
constraint_metric(downward_ls_theorem, snap_back_potential, 0.0).
constraint_metric(downward_ls_theorem, extractiveness, 0.01).

% The Internal Uncountability (The Tangled Rope)
% Within the model, no bijection exists between a set and N; the set is "internally" uncountable.
% Extractiveness at 0.42 triggers a Reform recommendation for relational mapping.
constraint_claim(internal_bijection_absence, tangled_rope).
constraint_metric(internal_bijection_absence, intensity, 0.75).
constraint_metric(internal_bijection_absence, suppression_requirement, 0.35).
constraint_metric(internal_bijection_absence, snap_back_potential, 0.60).
constraint_metric(internal_bijection_absence, extractiveness, 0.42).

% The External Countability Sink (The Snare)
% Externally, the entire domain is countable, meaning bijections exist but are not elements of the model.
% Extractiveness at 0.97 requires the 'absolute_set_scaffold' to distinguish perspectives.
constraint_claim(relativity_of_cardinality, snare).
constraint_metric(relativity_of_cardinality, intensity, 0.99).
constraint_metric(relativity_of_cardinality, suppression_requirement, 0.95).
constraint_metric(relativity_of_cardinality, snap_back_potential, 0.02).
constraint_metric(relativity_of_cardinality, extractiveness, 0.97).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.42 extractiveness) triggers Reform
recommendation(rec_01, distinguish_between_internal_sets_and_external_functions).
affects_constraint(rec_01, internal_bijection_absence).

% Snare (0.97 extractiveness) triggers Cut
% Utilizing 'absolute_set_scaffold' to realize that uncountability is a non-absolute property.
recommendation(rec_02, abandon_absolute_cardinality_in_first_order_semantics).
affects_constraint(rec_02, relativity_of_cardinality).

veto_actor(first_order_expressivity_limit).
veto_exposed(first_order_expressivity_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency is high in model creation; Stability is low due to the paradoxical "feel" of the result.
measurement(0, [1.00, 1.00, 0.80, 0.90]).
measurement(5, [0.60, 0.20, 0.95, 0.50]).

% 6. INTENT EVIDENCE
% Alternative: Second-order logic (Discarded: avoids the paradox but lacks a complete proof theory)
% Beneficiaries: Model theorists and philosophers of mathematics (limitations of formalism)
% Power Delta: Perspective Shift (Model-internal logic vs Meta-mathematical overview)
intent_evidence(non_absolute_property_detection, high_delta).
