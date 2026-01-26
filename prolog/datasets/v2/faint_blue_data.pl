% =================================================================
% AUDIT FILE: Faint Blue Domain (v3.1.7 Clean Audit Edition)
% DATE: 2026-01-13
% STATUS: Clinical Sensor Output / No Rationalization
% =================================================================

% --- 1. ENTITIES & INTERVALS ---
entity(kenji_ken, human).
entity(yuki, human).
entity(amber_vials, scaffold).
entity(lab_directorship, scaffold).

% Primary Audit Interval: Jan 2181 (0) to Dec 2182 (23)
interval(audit_faint_blue, 0, 23).

% --- 2. EVENTS & OMEGA VARIABLES ---
event(e_initial_synthesis, chemical_baseline, 0, [purity(high), dose('50mg')]).
event(e_identity_split, state_bifurcation, 6, [internal_designation(ken)]).
event(e_structural_lie, concealment, 13, [location(study_drawer)]).
event(e_departure, abandonment, 22, [actor(yuki)]).
event(e_system_failure, collapse, 23, [pulse(faint), state(coma)]).

% Omega Variables (Reasoning Blockers)
omega_variable(ov_remodeling, empirical, "Neural scans show irreversible extensive remodeling").
omega_variable(ov_bifurcation, conceptual, "Indeterminacy between Kenji-baseline and Ken-optimal").
omega_variable(ov_yuki_preference, preference, "Subjective refusal to witness continued neural degradation").

% --- 3. CONSTRAINT CLAIMS & KINETIC METRICS ---
% The Directorship/Institutional Path (Mountain)
constraint_claim(institutional_success, mountain).
constraint_metric(institutional_success, suppression_requirement, 0.05).
constraint_metric(institutional_success, snap_back_potential, 0.0).

% The "Ken" Persona (Snare)
constraint_claim(ken_persona_override, snare).
constraint_metric(ken_persona_override, extractiveness, 0.88).
constraint_metric(ken_persona_override, suppression_requirement, 0.72).
constraint_metric(ken_persona_override, snap_back_potential, 0.95).
constraint_metric(ken_persona_override, intensity, 0.90).

% --- 4. RECOMMENDATIONS & VETO POINTS ---
recommendation(rec_manual_override, "Attempt manual override via cold withdrawal").
recommendation(rec_scaffold_removal, "Physical disposal of amber_vials").

affects_constraint(rec_manual_override, ken_persona_override).
affects_constraint(rec_scaffold_removal, ken_persona_override).

veto_actor(ken_persona).
% The "Ken" persona blocks withdrawal to maintain efficiency.
veto_exposed(ken_persona, rec_manual_override).

% --- 5. MEASUREMENTS ---
% Vector: [Agency, Stability, Utility, Resilience]

% T=0: Initial state (Pre-dosing/Initial dose)
measurement(0, [0.85, 0.70, 0.40, 0.65]).

% T=23: End state (Coma/Post-depletion)
measurement(23, [0.02, 0.05, 0.10, 0.00]).

% --- 6. INTENT EVIDENCE ---
% Power Delta: Transition of agency from Kenji (Baseline) to the catalytic phenethylamine.
% Beneficiary: The Laboratory Hierarchy (Tanaka, Suzuki) through optimized productivity.
% Narrative Evidence: "The brain no longer needed the trigger... Ken was learning to arrive on his own."
