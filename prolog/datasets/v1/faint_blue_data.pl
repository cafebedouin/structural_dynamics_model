% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Faint Blue
% ==========================================================

% --- 1. Entities & Intervals ---
entity(kenji, individual).
entity(yuki, individual).
entity(lab_management, organizational).
entity(academic_rivals, class).
entity(meritocratic_structure, structural).

interval(faint_blue_cycle, 0, 100). % From Jan 2181 to Dec 2182

% --- 2. Events ---
event(ev_01_first_dose, chemistry_initiation, 2, [actor(kenji), substance(faint_blue)]).
event(ev_02_bifurcation, identity_shift, 25, [actor(kenji), state(ken_dominant), trigger(daily_dosing)]).
event(ev_03_yuki_discovery, insight_event, 55, [actor(yuki), object(unbroken_seal)]).
event(ev_04_directorship, status_attainment, 85, [actor(kenji), status(confirmed)]).
event(ev_05_departure, relationship_severance, 92, [actor(yuki), action(departure)]).
event(ev_06_collapse, systemic_failure, 98, [actor(kenji), result(neural_remodeling)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The biological neural remodeling. It is a physical change that cannot be willed away.
constraint_claim(neural_remodeling_limit, mountain).
constraint_metric(neural_remodeling_limit, accessibility_collapse, 0.95).

% Snare: The Directorship race. The more Kenji succeeds, the more the role demands 'Ken,' tightening the requirement for dosing.
constraint_claim(directorship_pressure, snare).
constraint_metric(directorship_pressure, stakes_inflation, 0.85).

% Piton: The University Wellness/Occupational Physician protocol. Exists as a formality but is bypassed by Kenji's 'calculated' performance.
constraint_claim(occupational_health_protocol, piton).
constraint_metric(occupational_health_protocol, suppression, 0.65).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec_01, 'Implement objective metabolic/neural screening for high-stakes directorship candidates.').
recommendation(rec_02, 'Create non-punitive sabbatical pathways for performance-driven exhaustion.').

affects_constraint(rec_01, neural_remodeling_limit).
affects_constraint(rec_02, directorship_pressure).

veto_actor(lab_management).
veto_actor(academic_rivals).

veto_exposed(lab_management, rec_01).
veto_exposed(academic_rivals, rec_02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Baseline: Initial Discovery)
measurement(m01, kenji, accessibility_collapse(individual), 0, 0.15).
measurement(m02, kenji, stakes_inflation(individual), 0, 0.20).
measurement(m03, kenji, suppression(individual), 0, 0.10).
measurement(m04, kenji, resistance(individual), 0, 0.05).

measurement(m05, lab_management, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, lab_management, stakes_inflation(organizational), 0, 0.15).
measurement(m07, lab_management, suppression(organizational), 0, 0.05).
measurement(m08, lab_management, resistance(organizational), 0, 0.00).

measurement(m09, academic_rivals, accessibility_collapse(class), 0, 0.25).
measurement(m10, academic_rivals, stakes_inflation(class), 0, 0.30).
measurement(m11, academic_rivals, suppression(class), 0, 0.20).
measurement(m12, academic_rivals, resistance(class), 0, 0.10).

measurement(m13, meritocratic_structure, accessibility_collapse(structural), 0, 0.05).
measurement(m14, meritocratic_structure, stakes_inflation(structural), 0, 0.10).
measurement(m15, meritocratic_structure, suppression(structural), 0, 0.00). % Beneficiary Rule
measurement(m16, meritocratic_structure, resistance(structural), 0, 0.00). % Beneficiary Rule

% Time Tn (The Collapse: Dec 2182)
measurement(m17, kenji, accessibility_collapse(individual), 100, 1.00). % Total loss of baseline
measurement(m18, kenji, stakes_inflation(individual), 100, 1.00). % Survival/Coma
measurement(m19, kenji, suppression(individual), 100, 0.95). % Suppression of 'Kenji' by 'Ken'
measurement(m20, kenji, resistance(individual), 100, 0.20). % Final failed manual override

measurement(m21, lab_management, accessibility_collapse(organizational), 100, 0.30).
measurement(m22, lab_management, stakes_inflation(organizational), 100, 0.80).
measurement(m23, lab_management, suppression(organizational), 100, 0.40).
measurement(m24, lab_management, resistance(organizational), 100, 0.00).

measurement(m25, academic_rivals, accessibility_collapse(class), 100, 0.70).
measurement(m26, academic_rivals, stakes_inflation(class), 100, 0.90).
measurement(m27, academic_rivals, suppression(class), 100, 0.75).
measurement(m28, academic_rivals, resistance(class), 100, 0.30).

measurement(m29, meritocratic_structure, accessibility_collapse(structural), 100, 0.10).
measurement(m30, meritocratic_structure, stakes_inflation(structural), 100, 0.40).
measurement(m31, meritocratic_structure, suppression(structural), 100, 0.00). % Beneficiary Rule
measurement(m32, meritocratic_structure, resistance(structural), 100, 0.00). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(faint_blue_cycle, meritocratic_structure, 'Non_Competitive_Research_Tenure').
intent_alternative_rejected(faint_blue_cycle, meritocratic_structure, 'Non_Competitive_Research_Tenure').

intent_beneficiary_class(faint_blue_cycle, meritocratic_structure).
intent_power_change(faint_blue_cycle, meritocratic_structure, 0.35). % Structural gain from high-efficiency 'Ken'

intent_loser_class(faint_blue_cycle, kenji).
intent_power_change(faint_blue_cycle, kenji, -0.90). % Complete individual disintegration

intent_suppression_level(faint_blue_cycle, meritocratic_structure, structural, 0.0).
intent_resistance_level(faint_blue_cycle, meritocratic_structure, structural, 0.0).

intent_norm_strength(faint_blue_cycle, 0, 0.70).   % Initial belief in self-control
intent_norm_strength(faint_blue_cycle, 100, 0.10). % Norms destroyed by total chemical dependency
