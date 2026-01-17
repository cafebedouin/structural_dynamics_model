% 1. ENTITIES & INTERVALS
entity(whitehead_group_candidate, system).
entity(abelian_category, manifold).
entity(resolution_path, sensor_path).
entity(constructible_universe_L, scaffold).

interval(axiom_selection, 0, 1).
interval(cardinal_evaluation, 0, 2).

% 2. EVENTS & OMEGA VARIABLES
event(e1, countable_case_verification, 0, [result, true_by_stein]).
event(e2, uncountable_shelah_split, 1, [result, independent_of_ZFC]).

% Omega Variable: Conceptual (Defining 'truth' for statements independent of ZFC)
omega_variable(omega_c1, conceptual, validity_of_mathematical_existence_under_varying_axioms).

% Omega Variable: Preference (Selection of Axiom of Constructibility vs. Martin's Axiom)
omega_variable(omega_p1, preference, meta_theoretical_bias_toward_universe_consistency).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Homological Nullity (The Mountain)
% The defining condition Ext(A, Z) = 0 is the invariant structural anchor of the problem.
constraint_claim(ext_zero_condition, mountain).
constraint_metric(ext_zero_condition, intensity, 1.0).
constraint_metric(ext_zero_condition, suppression_requirement, 0.05).
constraint_metric(ext_zero_condition, snap_back_potential, 0.0).
constraint_metric(ext_zero_condition, extractiveness, 0.01).

% The Infinite Abelian Complexity (The Tangled Rope)
% The structural variety of torsion-free abelian groups of large cardinality creates maximal entanglement.
% Extractiveness at 0.52 triggers a Reform recommendation for set-theoretic grounding.
constraint_claim(uncountable_structure_variation, tangled_rope).
constraint_metric(uncountable_structure_variation, intensity, 0.85).
constraint_metric(uncountable_structure_variation, suppression_requirement, 0.40).
constraint_metric(uncountable_structure_variation, snap_back_potential, 0.70).
constraint_metric(uncountable_structure_variation, extractiveness, 0.52).

% The ZFC Independence Sink (The Noose)
% The inability of ZFC to resolve the problem for cardinality Aleph-1.
% Extractiveness at 0.98 requires 'constructible_universe_L' or 'martins_axiom' scaffolds to proceed.
constraint_claim(axiomatic_undecidability, noose).
constraint_metric(axiomatic_undecidability, intensity, 0.99).
constraint_metric(axiomatic_undecidability, suppression_requirement, 0.95).
constraint_metric(axiomatic_undecidability, snap_back_potential, 0.01).
constraint_metric(axiomatic_undecidability, extractiveness, 0.98).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.52 extractiveness) triggers Reform
recommendation(rec_01, constrain_analysis_to_countable_torsion_free_groups).
affects_constraint(rec_01, uncountable_structure_variation).

% Noose (0.98 extractiveness) triggers Cut
% Utilizing 'constructible_universe_L' scaffold (V=L) to force a 'Yes' answer.
recommendation(rec_02, adopt_axiom_of_constructibility_to_resolve_freeness).
affects_constraint(rec_02, axiomatic_undecidability).

veto_actor(shelah_independence_theorem).
veto_exposed(shelah_independence_theorem, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (ability to decide) is high only when a specific universe (L or MA) is chosen.
measurement(0, [0.50, 1.00, 0.60, 0.80]).
measurement(2, [0.00, 0.00, 0.10, 0.05]).

% 6. INTENT EVIDENCE
% Alternative: Martin's Axiom (MA + not CH) (Discarded: leads to a 'No' answer, contradicts L)
% Beneficiaries: Homological algebraists and set-theoretic topologists
% Power Delta: Axiom Selection (ZFC vs Extension Axioms)
intent_evidence(metamathematical_indeterminacy, high_delta).
