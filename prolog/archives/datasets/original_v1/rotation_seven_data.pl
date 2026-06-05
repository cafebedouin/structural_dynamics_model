% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Rotation Seven
% ==========================================================

% --- 1. Entities & Intervals ---
entity(rina, individual).
entity(anna, individual).
entity(supervisor_kwan, individual).
entity(nurse_chen, individual).
entity(cohort_7b, class).
entity(management, class).
entity(orbital_dialysis_center, organizational).
entity(medical_department, organizational).

interval(rot7_cycle, 0, 100).

% --- 2. Events ---
event(ev_01_morning, wake_cycle, 0, [actor(rina), actor(anna), symptom(dark_urine)]).
event(ev_02_exposure, toxic_contact, 25, [actor(anna), substance(black_soil), reason(efficiency_pressure)]).
event(ev_03_medical_flag, screening, 60, [actor(kwan), target(anna), trigger(medical_alert)]).
event(ev_04_isolation, removal, 85, [actor(chen), target(anna), destination(r7)]).
event(ev_05_renunciation, credit_drop, 95, [actor(rina), action(drop_to_baseline)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The biological reality of black soil toxicity and kidney failure.
constraint_claim(soil_toxicity, mountain).
constraint_metric(soil_toxicity, accessibility_collapse, 0.85).

% Snare: The isolation protocol (R7) that separates individuals from support networks.
constraint_claim(isolation_protocol, snare).
constraint_metric(isolation_protocol, stakes_inflation, 0.95).

% Piton: The Kubo Credit Ladder (ranking system) used to motivate labor despite danger.
constraint_claim(kubo_ladder, piton).
constraint_metric(kubo_ladder, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec_01, 'Abolish isolation for flagged medical patients to maintain psychological stability.').
recommendation(rec_02, 'Automate black soil transfer to prevent pediatric exposure.').

affects_constraint(rec_01, isolation_protocol).
affects_constraint(rec_02, soil_toxicity).

veto_actor(supervisor_kwan).
veto_actor(nurse_chen).
veto_actor(management).

veto_exposed(management, rec_02).
veto_exposed(nurse_chen, rec_01).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial State)
measurement(m01, rina, accessibility_collapse(individual), 0, 0.1).
measurement(m02, rina, stakes_inflation(individual), 0, 0.4).
measurement(m03, rina, suppression(individual), 0, 0.6).
measurement(m04, rina, resistance(individual), 0, 0.2).

measurement(m05, medical_department, accessibility_collapse(organizational), 0, 0.2).
measurement(m06, medical_department, stakes_inflation(organizational), 0, 0.3).
measurement(m07, medical_department, suppression(organizational), 0, 0.1).
measurement(m08, medical_department, resistance(organizational), 0, 0.1).

measurement(m09, cohort_7b, accessibility_collapse(class), 0, 0.3).
measurement(m10, cohort_7b, stakes_inflation(class), 0, 0.5).
measurement(m11, cohort_7b, suppression(class), 0, 0.7).
measurement(m12, cohort_7b, resistance(class), 0, 0.3).

measurement(m13, management, accessibility_collapse(structural), 0, 0.0).
measurement(m14, management, stakes_inflation(structural), 0, 0.1).
measurement(m15, management, suppression(structural), 0, 0.0). % Beneficiary Rule
measurement(m16, management, resistance(structural), 0, 0.0). % Beneficiary Rule

% Time Tn (Post-Isolation / Credit Drop)
measurement(m17, rina, accessibility_collapse(individual), 100, 0.9).
measurement(m18, rina, stakes_inflation(individual), 100, 1.0).
measurement(m19, rina, suppression(individual), 100, 0.2). % Internal resistance high
measurement(m20, rina, resistance(individual), 100, 0.9). % Dropping credits

measurement(m21, medical_department, accessibility_collapse(organizational), 100, 0.1).
measurement(m22, medical_department, stakes_inflation(organizational), 100, 0.8).
measurement(m23, medical_department, suppression(organizational), 100, 0.2).
measurement(m24, medical_department, resistance(organizational), 100, 0.1).

measurement(m25, cohort_7b, accessibility_collapse(class), 100, 0.7).
measurement(m26, cohort_7b, stakes_inflation(class), 100, 0.9).
measurement(m27, cohort_7b, suppression(class), 100, 0.8).
measurement(m28, cohort_7b, resistance(class), 100, 0.4).

measurement(m29, management, accessibility_collapse(structural), 100, 0.0).
measurement(m30, management, stakes_inflation(structural), 100, 0.2).
measurement(m31, management, suppression(structural), 100, 0.0). % Beneficiary Rule
measurement(m32, management, resistance(structural), 100, 0.0). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(rot7_cycle, management, 'PPE_and_Automation_Upgrade').
intent_alternative_rejected(rot7_cycle, management, 'PPE_and_Automation_Upgrade').

intent_beneficiary_class(rot7_cycle, management).
intent_power_change(rot7_cycle, management, 0.15).

intent_loser_class(rot7_cycle, cohort_7b).
intent_power_change(rot7_cycle, cohort_7b, -0.65).

intent_suppression_level(rot7_cycle, management, structural, 0.0).
intent_resistance_level(rot7_cycle, management, structural, 0.0).

intent_norm_strength(rot7_cycle, 0, 0.85).   % High adherence to Kubo ladder initially
intent_norm_strength(rot7_cycle, 100, 0.40). % Norm erosion via Rina's defiance
