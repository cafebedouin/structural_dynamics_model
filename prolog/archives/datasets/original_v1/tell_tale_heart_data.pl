% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: THE TELL-TALE HEART
% ==========================================================

% --- 1. Entities & Intervals ---
entity(narrator, individual).
entity(old_man, individual).
entity(police_officers, class).
entity(the_neighbors, class).
entity(legal_system, organizational).
entity(objective_moral_order, structural).

interval(the_guilt_cycle, 0, 100).

% --- 2. Events ---
event(ev01_fixation, psychological_entry, 5, [target(vulture_eye), state(nervous)]).
event(ev02_the_deed, lethal_action, 50, [method(suffocation), dismemberment(true)]).
event(ev03_police_arrival, surveillance_entry, 80, [time(0400), reason(neighbor_report)]).
event(ev04_the_hallucination, coercion_initiation, 90, [stimulus(heartbeat), location(under_floorboards)]).
event(ev05_the_confession, system_submission, 100, [action(outcry), result(admission)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The 'Vulture Eye.' A biological/physical stimulus the narrator cannot 'un-see.'
constraint_claim(vulture_eye_fixation, mountain).
constraint_metric(vulture_eye_fixation, accessibility_collapse, 0.90).

% Snare: The sound of the heartbeat. An auditory feedback loop that tightens until confession.
constraint_claim(hallucinatory_heartbeat, snare).
constraint_metric(hallucinatory_heartbeat, stakes_inflation, 1.00).

% Piton: The narrator's 'Sagacity.' The dead idea that caution/precision proves sanity.
constraint_claim(delusion_of_sagacity, piton).
constraint_metric(delusion_of_sagacity, suppression, 0.85).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Institutionalize the subject upon initial fixation to prevent escalation.').
recommendation(rec02, 'Implement forensic floor-scanning to bypass social performance of \'ease\'.').

affects_constraint(rec01, vulture_eye_fixation).
affects_constraint(rec02, hallucinatory_heartbeat).

veto_actor(narrator).
veto_actor(legal_system).

veto_exposed(narrator, rec01).
veto_exposed(legal_system, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and T100) ---

% Time T0 (Initial Preparation / Eighth Night)
measurement(m01, narrator, accessibility_collapse(individual), 0, 0.10).
measurement(m02, narrator, stakes_inflation(individual), 0, 0.40).
measurement(m03, narrator, suppression(individual), 0, 0.20).
measurement(m04, narrator, resistance(individual), 0, 0.05).

measurement(m05, legal_system, accessibility_collapse(organizational), 0, 0.05).
measurement(m06, legal_system, stakes_inflation(organizational), 0, 0.10).
measurement(m07, legal_system, suppression(organizational), 0, 0.00).
measurement(m08, legal_system, resistance(organizational), 0, 0.00).

measurement(m09, police_officers, accessibility_collapse(class), 0, 0.15).
measurement(m10, police_officers, stakes_inflation(class), 0, 0.20).
measurement(m11, police_officers, suppression(class), 0, 0.05).
measurement(m12, police_officers, resistance(class), 0, 0.00).

measurement(m13, objective_moral_order, accessibility_collapse(structural), 0, 0.00).
measurement(m14, objective_moral_order, stakes_inflation(structural), 0, 0.05).
measurement(m15, objective_moral_order, suppression(structural), 0, 0.00). % Beneficiary Rule
measurement(m16, objective_moral_order, resistance(structural), 0, 0.00). % Beneficiary Rule

% Time T100 (The Outcry / Confession)
measurement(m17, narrator, accessibility_collapse(individual), 100, 1.00). % Agency destroyed by heartbeat
measurement(m18, narrator, stakes_inflation(individual), 100, 1.00). % Life/Sanity at total risk
measurement(m19, narrator, suppression(individual), 100, 0.95). % Hallucination as supreme suppressor
measurement(m20, narrator, resistance(individual), 100, 0.90). % Pacing, foaming, raving

measurement(m21, legal_system, accessibility_collapse(organizational), 100, 0.00).
measurement(m22, legal_system, stakes_inflation(organizational), 100, 0.80).
measurement(m23, legal_system, suppression(organizational), 100, 0.20).
measurement(m24, legal_system, resistance(organizational), 100, 0.00).

measurement(m25, police_officers, accessibility_collapse(class), 100, 0.05).
measurement(m26, police_officers, stakes_inflation(class), 100, 0.90).
measurement(m27, police_officers, suppression(class), 100, 0.10).
measurement(m28, police_officers, resistance(class), 100, 0.95). % Successful resolution

measurement(m29, objective_moral_order, accessibility_collapse(structural), 100, 0.00).
measurement(m30, objective_moral_order, stakes_inflation(structural), 100, 0.40).
measurement(m31, objective_moral_order, suppression(structural), 100, 0.00). % Beneficiary Rule
measurement(m32, objective_moral_order, resistance(structural), 100, 0.00). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(the_guilt_cycle, objective_moral_order, 'Mental_Health_Intervention').
intent_alternative_rejected(the_guilt_cycle, objective_moral_order, 'Mental_Health_Intervention').

intent_beneficiary_class(the_guilt_cycle, objective_moral_order).
intent_power_change(the_guilt_cycle, objective_moral_order, 0.50). % Restoration of order via self-incrimination

intent_loser_class(the_guilt_cycle, narrator).
intent_power_change(the_guilt_cycle, narrator, -1.00). % Total individual collapse

intent_suppression_level(the_guilt_cycle, objective_moral_order, structural, 0.0).
intent_resistance_level(the_guilt_cycle, objective_moral_order, structural, 0.0).

intent_norm_strength(the_guilt_cycle, 0, 0.30).   % Initial belief in the crime's invisibility
intent_norm_strength(the_guilt_cycle, 100, 1.00). % Totalized norm of 'Guilt will out'
