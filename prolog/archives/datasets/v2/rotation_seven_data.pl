% =============================================================================
% DOMAIN: Rotation Seven
% MISSION: Clinical Sensor Output
% DATA INTEGRITY: 100%
% =============================================================================

% 1. ENTITIES & INTERVALS
% -----------------------------------------------------------------------------
entity(rina_reyes, person).
entity(anna_santos, person).
entity(supervisor_kwan, person).
entity(nurse_chen, person).
entity(maya, person).
entity(kubo_ladder, social_construct).
entity(black_soil, hazard).
entity(white_soil, scaffold). % Safe synthetic-clay as transitional support.
entity(r7_isolation, scaffold). % Temporary observation/containment structure.

% Temporal range: 0 (Day 1, 0400) to 7 (Day 7, Post-departure).
interval(rotation_seven_duration, 0, 7).

% 2. EVENTS & OMEGA VARIABLES
% -----------------------------------------------------------------------------
% Day 1: The Exposure
event(e0, waking_observation, 0, [breaths_per_minute(17), water_discoloration(dark)]).
event(e1, black_soil_exposure, 1, [method(barehanded), credit_transfer(true)]).

% Day 2-3: The Decay
event(e2, respiratory_increase, 2, [breaths_per_minute(18)]).
event(e3, medical_flag_trigger, 3, [symptom(shaking_hands), color(yellow)]).

% Day 4-6: The Isolation
event(e4, credit_purge, 4, [actor(rina), status(baseline), band(white)]).
event(e5, terminal_contact, 6, [breaths_per_minute(24), touch_protocol_violation(true)]).

% OMEGA VARIABLES (Ω)
% Ω_E: The exact chemical composition of the "Black Soil" contaminant.
omega_variable(omega_e1, empirical, 'Chemical/biological markers in black soil inducing kidney failure.').

% Ω_C: The definition of "Real" in a simulated/station environment.
omega_variable(omega_c1, conceptual, 'Rina defines "Real" as interpersonal proximity versus the "Fake" ladder.').

% Ω_P: The value judgment of choosing 3 days of proximity over potential long-term survival.
omega_variable(omega_p1, preference, 'Rina and Anna prioritize presence/autonomy over institutional medical compliance.').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% -----------------------------------------------------------------------------

% Claim: The Kubo Credit System / Ranking Ladder.
% Classification: Snare (⊠C) - High extractiveness of labor via false rewards.
constraint_claim(kubo_ladder_system, snare).
constraint_metric(kubo_ladder_system, extractiveness, 0.85).
constraint_metric(kubo_ladder_system, suppression_requirement, 0.70).

% Claim: The Protective Protocols (White Soil / Medical Flags).
% Classification: Rope (⊞C) - Restrictive but nominally protective.
constraint_claim(safety_protocol, rope).
constraint_metric(safety_protocol, extractiveness, 0.35).
constraint_metric(safety_protocol, suppression_requirement, 0.15).

% Claim: Rina's internal state post-purge.
% Classification: Mountain (■C) - High stability, low suppression of self.
constraint_claim(rina_autonomy, mountain).
constraint_metric(rina_autonomy, suppression_requirement, 0.02). % < 0.05 Verification
constraint_metric(rina_autonomy, snap_back_potential, 0.0).

% 4. RECOMMENDATIONS & VETO POINTS
% -----------------------------------------------------------------------------
% Recommendation: Dismantle the Kubo Ladder to prevent psychological coercion.
recommendation(r1, 'Total credit abandonment/de-escalation to baseline status to regain cognitive autonomy.').
affects_constraint(r1, kubo_ladder_system).

% Recommendation: Deploy Scaffold (White Soil/Support) without ranking tie-ins.
recommendation(r2, 'Utilization of white_soil and r7_isolation as pure physical supports, decoupled from labor metrics.').
affects_constraint(r2, safety_protocol).

veto_actor(supervisor_kwan).
veto_actor(nurse_chen).
veto_exposed(supervisor_kwan, r1).

% 5. MEASUREMENTS
% -----------------------------------------------------------------------------
% Vector: [Agency, Stability, Utility, Resilience]

% Time 0: Start of Observation (High Utility/Stability, Low Agency)
measurement(0, [0.30, 0.80, 0.75, 0.40]).

% Time 7: Post-Purge/Post-Loss (Maximum Agency, Low Stability, Low Utility)
measurement(7, [0.95, 0.20, 0.10, 0.85]).

% 6. INTENT EVIDENCE
% -----------------------------------------------------------------------------
% Alternatives: Rina could have reported Anna (Institutional Utility).
% Beneficiaries: The Station (via labor/credits), Rina/Anna (via shared finality).
% Power Delta: Kwan/Station holds the physical key; Rina holds the internal "Baseline".

intent_data(power_delta, 'Station controls the physical life-support; Rina controls the valuation of the game.').
intent_data(beneficiary, 'Institutional ranking system benefits from child labor extraction under guise of meritocracy.').
