% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Information Theory
% Domain: Communication - A Mathematical Theory of Communication (1948)
% ==========================================================

% --- 1. Entities & Intervals ---
entity(claude_shannon, individual).
entity(bell_telephone_laboratories, organizational).
entity(communication_engineers, class).
entity(information_source, structural).
entity(noise_source, structural).
entity(mathematical_framework, structural).

interval(information_revolution, 0, 100). % From pre-formal (T0) to post-Shannon (Tn)

% --- 2. Events ---
event(ev01_paper_publication, theoretical_initiation, 5, [actor(claude_shannon), concept(entropy_formula)]).
event(ev02_channel_capacity_definition, parameter_setting, 35, [target(communication_channel), logic(fundamental_limit)]).
event(ev03_noise_integration, systemic_overhaul, 70, [subject(signal_to_noise), result(error_correction_coding)]).
event(ev04_digitization_standard, final_verification, 95, [subject(binary_representation), result(universal_standard)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute physical limit of Channel Capacity (C).
constraint_claim(channel_capacity_limit, mountain).
constraint_metric(channel_capacity_limit, accessibility_collapse, 1.0).

% Snare: The 'Promise' of Semantic Meaning. It tightens as engineers try to transmit 'truth' instead of 'bits'.
constraint_claim(semantic_irrelevance_norm, snare).
constraint_metric(semantic_irrelevance_norm, stakes_inflation, 0.90).

% Piton: The Telegraph-centric Linear Logic. A theoretical medium formerly believed to be standard but rendered obsolete.
constraint_claim(linear_telegraphy_logic, piton).
constraint_metric(linear_telegraphy_logic, suppression, 0.80).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Acknowledge that semantic aspects of communication are irrelevant to the engineering problem.').
recommendation(rec02, 'Utilize logarithmic measures (bits) for information to allow mathematical additivity.').

affects_constraint(rec01, semantic_irrelevance_norm).
affects_constraint(rec02, channel_capacity_limit).

veto_actor(communication_engineers).
veto_actor(bell_telephone_laboratories).

veto_exposed(communication_engineers, rec01).
veto_exposed(bell_telephone_laboratories, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Pre-Formal / Intuitive Engineering)
measurement(m01, claude_shannon, accessibility_collapse(individual), 0, 0.75).
measurement(m02, claude_shannon, stakes_inflation(individual), 0, 0.40).
measurement(m03, claude_shannon, suppression(individual), 0, 0.85).
measurement(m04, claude_shannon, resistance(individual), 0, 0.15).

measurement(m05, bell_telephone_laboratories, accessibility_collapse(organizational), 0, 0.15).
measurement(m06, bell_telephone_laboratories, stakes_inflation(organizational), 0, 0.25).
measurement(m07, bell_telephone_laboratories, suppression(organizational), 0, 0.10).
measurement(m08, bell_telephone_laboratories, resistance(organizational), 0, 0.05).

measurement(m09, communication_engineers, accessibility_collapse(class), 0, 0.10).
measurement(m10, communication_engineers, stakes_inflation(class), 0, 0.20).
measurement(m11, communication_engineers, suppression(class), 0, 0.10).
measurement(m12, communication_engineers, resistance(class), 0, 0.90).

measurement(m13, mathematical_framework, accessibility_collapse(structural), 0, 0.00).
measurement(m14, mathematical_framework, stakes_inflation(structural), 0, 0.05).
measurement(m15, mathematical_framework, suppression(structural), 0, 0.00). % Beneficiary logic
measurement(m16, mathematical_framework, resistance(structural), 0, 0.00). % Beneficiary logic

% Time Tn (Post-Formal / Shannon Dominance)
measurement(m17, claude_shannon, accessibility_collapse(individual), 100, 0.05).
measurement(m18, claude_shannon, stakes_inflation(individual), 100, 0.98).
measurement(m19, claude_shannon, suppression(individual), 100, 0.02).
measurement(m20, claude_shannon, resistance(individual), 100, 0.95).

measurement(m21, bell_telephone_laboratories, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, bell_telephone_laboratories, stakes_inflation(organizational), 100, 0.95).
measurement(m23, bell_telephone_laboratories, suppression(organizational), 100, 0.05).
measurement(m24, bell_telephone_laboratories, resistance(organizational), 100, 0.98).

measurement(m25, communication_engineers, accessibility_collapse(class), 100, 0.05).
measurement(m26, communication_engineers, stakes_inflation(class), 100, 0.90).
measurement(m27, communication_engineers, suppression(class), 100, 0.25).
measurement(m28, communication_engineers, resistance(class), 100, 0.80).

measurement(m29, mathematical_framework, accessibility_collapse(structural), 100, 0.00).
measurement(m30, mathematical_framework, stakes_inflation(structural), 100, 0.40).
measurement(m31, mathematical_framework, suppression(structural), 100, 0.00). % Beneficiary logic
measurement(m32, mathematical_framework, resistance(structural), 100, 0.00). % Beneficiary logic

% --- 6. Intent Evidence ---
intent_viable_alternative(information_revolution, mathematical_framework, 'Continuous_Analogue_Transmission_Priority').
intent_alternative_rejected(information_revolution, mathematical_framework, 'Continuous_Analogue_Transmission_Priority').

intent_beneficiary_class(information_revolution, bell_telephone_laboratories).
intent_power_change(information_revolution, bell_telephone_laboratories, 0.95). % Structural monopoly on the 'digital' future

intent_loser_class(information_revolution, communication_engineers). % Forced to unlearn analogue 'semantic' intuition
intent_power_change(information_revolution, communication_engineers, -0.30). % Cognitive and professional transition cost

intent_suppression_level(information_revolution, bell_telephone_laboratories, structural, 0.0).
intent_resistance_level(information_revolution, bell_telephone_laboratories, structural, 0.0).

intent_norm_strength(information_revolution, 0, 0.85). % Telegraphy paradigms
intent_norm_strength(information_revolution, 100, 1.0). % Information Theory as absolute dogma
