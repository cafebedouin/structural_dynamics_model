% 1. ENTITIES & INTERVALS
entity(provability_logic_gl, system).
entity(modal_frame_space, manifold).
entity(self_reflective_path, sensor_path).
entity(diagonal_lemma_fixed_point, scaffold).

interval(reflection_depth, 0, 10).
interval(fixed_point_calculation, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, hilbert_bernays_verification, 0, [derivation, condition_3]).
event(e2, lob_fixed_point_emergence, 1, [sentence, psi_equivalent_to_box_psi_to_A]).

% Omega Variable: Conceptual (The 'surprising' nature of the theorem compared to naive intuition)
omega_variable(omega_c1, conceptual, alignment_of_lobs_theorem_with_the_liar_paradox_variants).

% Omega Variable: Preference (Choice of modal system for interpreting 'provability')
omega_variable(omega_p1, preference, selection_of_gl_logic_vs_s4_for_reflection_principles).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Hilbert-Bernays Conditions (The Mountain)
% The three provability conditions (K, 4, and the second incompleteness theorem analogue). 
% These are the fixed, non-negotiable axioms for any system satisfying Löb's.
constraint_claim(provability_axioms, mountain).
constraint_metric(provability_axioms, intensity, 1.0).
constraint_metric(provability_axioms, suppression_requirement, 0.05).
constraint_metric(provability_axioms, snap_back_potential, 0.0).
constraint_metric(provability_axioms, extractiveness, 0.01).

% The Reflexive Implication (The Tangled Rope)
% The hypothesis Box(Box A -> A). This creates a recursive loop of nested provability.
% Extractiveness at 0.58 triggers a Reform recommendation for fixed-point analysis.
constraint_claim(nested_reflection_loop, tangled_rope).
constraint_metric(nested_reflection_loop, intensity, 0.85).
constraint_metric(nested_reflection_loop, suppression_requirement, 0.40).
constraint_metric(nested_reflection_loop, snap_back_potential, 0.70).
constraint_metric(nested_reflection_loop, extractiveness, 0.58).

% The Second Incompleteness Collapse (The Noose)
% The realization that a system can only prove its own consistency (Box(not Box false) -> not Box false) if it is inconsistent.
% Extractiveness at 0.99 requires the 'diagonal_lemma_fixed_point' scaffold to resolve.
constraint_claim(reflection_principle_trivialization, noose).
constraint_metric(reflection_principle_trivialization, intensity, 0.99).
constraint_metric(reflection_principle_trivialization, suppression_requirement, 0.96).
constraint_metric(reflection_principle_trivialization, snap_back_potential, 0.01).
constraint_metric(reflection_principle_trivialization, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.58 extractiveness) triggers Reform
recommendation(rec_01, utilize_modal_logic_k4_frames_to_visualize_provability_depth).
affects_constraint(rec_01, nested_reflection_loop).

% Noose (0.99 extractiveness) triggers Cut
% Utilizing 'diagonal_lemma_fixed_point' scaffold to construct the Rosser-style sentence for the proof.
recommendation(rec_02, abandon_naive_reflection_principles_for_formal_provability_logic).
affects_constraint(rec_02, reflection_principle_trivialization).

veto_actor(modal_logic_gl_frame_consistency).
veto_exposed(modal_logic_gl_frame_consistency, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (internal logic) is high; Stability of the "truth" concept within the system is low.
measurement(0, [1.00, 1.00, 0.80, 0.95]).
measurement(1, [1.00, 0.20, 0.90, 0.60]).

% 6. INTENT EVIDENCE
% Alternative: Modus Ponens (Discarded: Löb's is a meta-logical result specifically about Box)
% Beneficiaries: AI researchers (safety and self-reflective agents) and Pure Logicians
% Power Delta: Fixed Point Logic (Recursive Provability vs Concrete Truth)
intent_evidence(provability_logic_consequence, high_delta).
