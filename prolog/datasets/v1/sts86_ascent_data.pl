% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: STS-86 Ascent
% Domain: Procedural Engineering - Space Shuttle Ascent Checklist
% ==========================================================

% --- 1. Entities & Intervals ---
entity(commander_cdr, individual).
entity(pilot_plt, individual).
entity(mission_control_center, organizational).
entity(astronaut_corps, class).
entity(orbital_mechanics_system, structural).
entity(space_shuttle_vehicle, individual).

interval(ascent_to_orbit_cycle, 0, 100). % Lift-off (T0) to MECO/Orbit Insertion (Tn)

% --- 2. Events ---
event(ev01_liftoff, kinetic_initiation, 0, [met('0/00:00:00'), action(srb_ignition)]).
event(ev02_max_q, peak_constraint, 15, [property(max_dynamic_pressure), action(throttle_down)]).
event(ev03_srb_sep, stage_transition, 25, [met('0/00:02:04'), action(srb_jettison)]).
event(ev04_abort_boundary, logic_shift, 65, [type(press_to_ato), result(rtls_option_collapse)]). 
event(ev05_meco, finality, 98, [met('0/00:08:30'), result(velocity_attained)]). 

% --- 3. Constraint Claims & Metrics ---
% Mountain: The objective physical reality of Gravity and Orbital Velocity requirements.
constraint_claim(orbital_insertion_physics, mountain).
constraint_metric(orbital_insertion_physics, accessibility_collapse, 0.98).

% Snare: The "Abort Modes" (RTLS, TAL, ATO). Options disappear as time/altitude increases, narrowing to a single path.
constraint_claim(abort_decision_window, snare).
constraint_metric(abort_decision_window, stakes_inflation, 1.0).

% Piton: The "Man-in-the-Loop" manual override. Legacy pilot-control requirements for systems that are computationally deterministic.
constraint_claim(manual_override_bias, piton).
constraint_metric(manual_override_bias, suppression, 0.85).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Automate Abort Mode selection and initiation to eliminate pilot hesitation in RTLS/TAL boundaries.').
recommendation(rec02, 'Implement real-time trajectory re-optimization to bypass rigid MET-based checklist triggers.').

affects_constraint(rec01, abort_decision_window).
affects_constraint(rec02, orbital_insertion_physics).

veto_actor(mission_control_center).
veto_actor(orbital_mechanics_system).

veto_exposed(mission_control_center, rec01).
veto_exposed(orbital_mechanics_system, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Ignition / Liftoff)
measurement(m01, commander_cdr, accessibility_collapse(individual), 0, 0.30).
measurement(m02, commander_cdr, stakes_inflation(individual), 0, 0.95).
measurement(m03, commander_cdr, suppression(individual), 0, 0.10).
measurement(m04, commander_cdr, resistance(individual), 0, 0.05).

measurement(m05, mission_control_center, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, mission_control_center, stakes_inflation(organizational), 0, 0.40).
measurement(m07, mission_control_center, suppression(organizational), 0, 0.05).
measurement(m08, mission_control_center, resistance(organizational), 0, 0.00).

measurement(m09, astronaut_corps, accessibility_collapse(class), 0, 0.20).
measurement(m10, astronaut_corps, stakes_inflation(class), 0, 0.60).
measurement(m11, astronaut_corps, suppression(class), 0, 0.15).
measurement(m12, astronaut_corps, resistance(class), 0, 0.95).

measurement(m13, orbital_mechanics_system, accessibility_collapse(structural), 0, 0.00).
measurement(m14, orbital_mechanics_system, stakes_inflation(structural), 0, 0.10).
measurement(m15, orbital_mechanics_system, suppression(structural), 0, 0.00). % Beneficiary Logic
measurement(m16, orbital_mechanics_system, resistance(structural), 0, 0.00). % Beneficiary Logic

% Time Tn (Main Engine Cut-Off / MECO)
measurement(m17, commander_cdr, accessibility_collapse(individual), 100, 1.00). % Finality of state attained
measurement(m18, commander_cdr, stakes_inflation(individual), 100, 1.00). % Survival determined
measurement(m19, commander_cdr, suppression(individual), 100, 0.98). % Absolute adherence to checklist
measurement(m20, commander_cdr, resistance(individual), 100, 0.00). % Submission to trajectory

measurement(m21, mission_control_center, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, mission_control_center, stakes_inflation(organizational), 100, 0.90).
measurement(m23, mission_control_center, suppression(organizational), 100, 0.05).
measurement(m24, mission_control_center, resistance(organizational), 100, 0.98).

measurement(m25, astronaut_corps, accessibility_collapse(class), 100, 0.10).
measurement(m26, astronaut_corps, stakes_inflation(class), 100, 0.95).
measurement(m27, astronaut_corps, suppression(class), 100, 0.40).
measurement(m28, astronaut_corps, resistance(class), 100, 0.70).

measurement(m29, orbital_mechanics_system, accessibility_collapse(structural), 100, 0.00).
measurement(m30, orbital_mechanics_system, stakes_inflation(structural), 100, 0.50).
measurement(m31, orbital_mechanics_system, suppression(structural), 100, 0.00). % Beneficiary Logic
measurement(m32, orbital_mechanics_system, resistance(structural), 100, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(ascent_to_orbit_cycle, orbital_mechanics_system, "Autonomous_Fully_Robotic_Ascent").
intent_alternative_rejected(ascent_to_orbit_cycle, orbital_mechanics_system, "Autonomous_Fully_Robotic_Ascent").

intent_beneficiary_class(ascent_to_orbit_cycle, orbital_mechanics_system).
intent_power_change(ascent_to_orbit_cycle, orbital_mechanics_system, 0.90). % Structural gain from precise state alignment

intent_loser_class(ascent_to_orbit_cycle, commander_cdr). % Individual agency sacrificed for checklist compliance
intent_power_change(ascent_to_orbit_cycle, commander_cdr, -0.80).

intent_suppression_level(ascent_to_orbit_cycle, orbital_mechanics_system, structural, 0.0).
intent_resistance_level(ascent_to_orbit_cycle, orbital_mechanics_system, structural, 0.0).

intent_norm_strength(ascent_to_orbit_cycle, 0, 0.90). % Heavy procedural reliance
intent_norm_strength(ascent_to_orbit_cycle, 100, 1.00). % Totalized institutional discipline
