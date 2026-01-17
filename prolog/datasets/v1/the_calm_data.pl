% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: The Calm
% ==========================================================

% --- 1. Entities & Intervals ---
entity(soh, individual).
entity(petrov, individual).
entity(khatri, individual).
entity(chen, individual).
entity(skua_colony, class).
entity(traverse_crew, class).
entity(mcmurdo_ops, organizational).
entity(antarctic_treaty_system, structural).

interval(antarctic_traverse, 0, 100).

% --- 2. Events ---
event(ev_01_skua_death, species_violation, 17, [actor(petrov), tool(ice_axe), result(fatality)]).
event(ev_02_khatri_veto, decision_censorship, 20, [actor(khatri), motive(economic_preservation)]).
event(ev_03_the_calm, environmental_anomaly, 35, [type(thermal_inversion), duration('5_days')]).
event(ev_04_sacrifice_proposal, social_exclusion, 70, [actor(chen), target(petrov), motive(survival)]).
event(ev_05_collective_lie, debrief_fraud, 90, [actor(traverse_crew), target(mcmurdo_ops)]).
event(ev_06_narrative_infection, trauma_transfer, 98, [actor(soh), target(mbatha)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The Thermal Inversion/Radiative Cooling. A literal physical wall.
constraint_claim(thermal_inversion, mountain).
constraint_metric(thermal_inversion, accessibility_collapse, 0.95).

% Noose: Economic Coercion. The 'No one gets paid' threat creates a closing circle.
constraint_claim(economic_censorship, noose).
constraint_metric(economic_censorship, stakes_inflation, 0.88).

% Zombie: The Antarctic Treaty Protocol. A 'dead' rule that is cited but bypassed in crisis.
constraint_claim(treaty_protocol, zombie).
constraint_metric(treaty_protocol, suppression, 0.75).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec_01, 'Decouple mission funding from environmental incident reporting to remove economic penalty.').
recommendation(rec_02, 'Mandatory automated black-box logging for traverse vehicles to bypass human censorship.').

affects_constraint(rec_01, economic_censorship).
affects_constraint(rec_02, treaty_protocol).

veto_actor(mcmurdo_ops).
veto_actor(khatri).

veto_exposed(mcmurdo_ops, rec_01).
veto_exposed(khatri, rec_02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Pre-Incident / Baseline)
measurement(m01, petrov, accessibility_collapse(individual), 0, 0.1).
measurement(m02, petrov, stakes_inflation(individual), 0, 0.2).
measurement(m03, petrov, suppression(individual), 0, 0.1).
measurement(m04, petrov, resistance(individual), 0, 0.0).

measurement(m05, mcmurdo_ops, accessibility_collapse(organizational), 0, 0.1).
measurement(m06, mcmurdo_ops, stakes_inflation(organizational), 0, 0.3).
measurement(m07, mcmurdo_ops, suppression(organizational), 0, 0.0).
measurement(m08, mcmurdo_ops, resistance(organizational), 0, 0.0).

measurement(m09, traverse_crew, accessibility_collapse(class), 0, 0.2).
measurement(m10, traverse_crew, stakes_inflation(class), 0, 0.4).
measurement(m11, traverse_crew, suppression(class), 0, 0.2).
measurement(m12, traverse_crew, resistance(class), 0, 0.1).

measurement(m13, antarctic_treaty_system, accessibility_collapse(structural), 0, 0.0).
measurement(m14, antarctic_treaty_system, stakes_inflation(structural), 0, 0.1).
measurement(m15, antarctic_treaty_system, suppression(structural), 0, 0.0).
measurement(m16, antarctic_treaty_system, resistance(structural), 0, 0.0).

% Time Tn (Post-Traverse / Psychological Entrenchment)
measurement(m17, petrov, accessibility_collapse(individual), 100, 1.0). % Complete internal isolation
measurement(m18, petrov, stakes_inflation(individual), 100, 1.0). % Maximum guilt
measurement(m19, petrov, suppression(individual), 100, 0.9). % Censored by crew
measurement(m20, petrov, resistance(individual), 100, 0.5). % Resistance via repetitive telling

measurement(m21, mcmurdo_ops, accessibility_collapse(organizational), 100, 0.2).
measurement(m22, mcmurdo_ops, stakes_inflation(organizational), 100, 0.5).
measurement(m23, mcmurdo_ops, suppression(organizational), 100, 0.4).
measurement(m24, mcmurdo_ops, resistance(organizational), 100, 0.0).

measurement(m25, traverse_crew, accessibility_collapse(class), 100, 0.6).
measurement(m26, traverse_crew, stakes_inflation(class), 100, 0.9).
measurement(m27, traverse_crew, suppression(class), 100, 1.0). % Absolute collective silence
measurement(m28, traverse_crew, resistance(class), 100, 0.1).

measurement(m29, antarctic_treaty_system, accessibility_collapse(structural), 100, 0.0).
measurement(m30, antarctic_treaty_system, stakes_inflation(structural), 100, 0.3).
measurement(m31, antarctic_treaty_system, suppression(structural), 100, 0.0). % Beneficiary logic
measurement(m32, antarctic_treaty_system, resistance(structural), 100, 0.0). % Beneficiary logic

% --- 6. Intent Evidence ---
intent_viable_alternative(antarctic_traverse, antarctic_treaty_system, 'Immediate_Incident_Reporting_and_Evacuation').
intent_alternative_rejected(antarctic_traverse, antarctic_treaty_system, 'Immediate_Incident_Reporting_and_Evacuation').

intent_beneficiary_class(antarctic_traverse, mcmurdo_ops).
intent_power_change(antarctic_traverse, mcmurdo_ops, 0.25). % Mission success without investigation cost

intent_loser_class(antarctic_traverse, traverse_crew).
intent_power_change(antarctic_traverse, traverse_crew, -0.80). % Extreme psychological/moral injury

intent_suppression_level(antarctic_traverse, mcmurdo_ops, structural, 0.0).
intent_resistance_level(antarctic_traverse, mcmurdo_ops, structural, 0.0).

intent_norm_strength(antarctic_traverse, 0, 0.90).   % Formal treaty adherence high
intent_norm_strength(antarctic_traverse, 100, 0.15). % Informal "silence for survival" takes over
