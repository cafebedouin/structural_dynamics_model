% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Bhagavad-Gita
% Domain: Foundational Metaphysics - The Discourse of Kurukshetra
% ==========================================================

% --- 1. Entities & Intervals ---
entity(arjuna, individual).
entity(krishna, individual).
entity(pandava_host, organizational).
entity(kaurava_host, organizational).
entity(kshatriya_varna, class).
entity(kinship_class, class).
entity(brahman_structure, structural).
entity(dharma_system, structural).

interval(kurukshetra_dialogue, 0, 100).

% --- 2. Events ---
event(ev01_arjuna_vishad, psychological_collapse, 5, [actor(arjuna), symptom(sinking_in_car), cause(pity_for_kin)]).
event(ev02_sankhya_doctrine, theoretical_reframe, 25, [actor(krishna), subject(immortality_of_soul), effect(separation_of_essence)]).
event(ev03_viswarupa_darshan, systemic_revelation, 75, [actor(krishna), target(arjuna), content(universal_form), result(ego_death)]).
event(ev04_final_resolve, alignment, 100, [actor(arjuna), result(fight_without_attachment)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The biological/physical certainty of death for the 'fleshly garb.'
constraint_claim(physical_mortality, mountain).
constraint_metric(physical_mortality, accessibility_collapse, 1.0).

% Snare: The Kshatriya Dharma (caste duty). It tightens as the battle becomes inevitable.
constraint_claim(varna_duty_mandate, snare).
constraint_metric(varna_duty_mandate, stakes_inflation, 0.95).

% Piton: Vedic Ritualism (Formalism). The 'calculating righteousness' that seeks heaven through works.
constraint_claim(calculating_ritualism, piton).
constraint_metric(calculating_ritualism, suppression, 0.75).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt Nishkama Karma (selfless action) to bypass the bondage of wrought deeds.').
recommendation(rec02, 'Relinquish identification with the transient ego to overcome the fear of killing kin.').

affects_constraint(rec01, varna_duty_mandate).
affects_constraint(rec02, physical_mortality).

veto_actor(arjuna).
veto_actor(kaurava_host).
veto_actor(brahman_structure).

veto_exposed(arjuna, rec01).
veto_exposed(brahman_structure, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Arjuna's Distress / Initial Hesitation)
measurement(m01, arjuna, accessibility_collapse(individual), 0, 0.85). % Options feel blocked by grief
measurement(m02, arjuna, stakes_inflation(individual), 0, 0.90). % Family death feels like total loss
measurement(m03, arjuna, suppression(individual), 0, 0.60). % Suppressed by emotion
measurement(m04, arjuna, resistance(individual), 0, 0.95). % Resisting his role as warrior

measurement(m05, pandava_host, accessibility_collapse(organizational), 0, 0.20).
measurement(m06, pandava_host, stakes_inflation(organizational), 0, 0.30).
measurement(m07, pandava_host, suppression(organizational), 0, 0.10).
measurement(m08, pandava_host, resistance(organizational), 0, 0.05).

measurement(m09, kshatriya_varna, accessibility_collapse(class), 0, 0.30).
measurement(m10, kshatriya_varna, stakes_inflation(class), 0, 0.50).
measurement(m11, kshatriya_varna, suppression(class), 0, 0.40).
measurement(m12, kshatriya_varna, resistance(class), 0, 0.10).

measurement(m13, brahman_structure, accessibility_collapse(structural), 0, 0.00).
measurement(m14, brahman_structure, stakes_inflation(structural), 0, 0.05).
measurement(m15, brahman_structure, suppression(structural), 0, 0.00). % Beneficiary Rule
measurement(m16, brahman_structure, resistance(structural), 0, 0.00). % Beneficiary Rule

% Time Tn (Arjuna's Resolve / Post-Revelation)
measurement(m17, arjuna, accessibility_collapse(individual), 100, 0.05). % Path is clear
measurement(m18, arjuna, stakes_inflation(individual), 100, 0.10). % Outcome of results irrelevant
measurement(m19, arjuna, suppression(individual), 100, 0.98). % Ego fully suppressed by Dharma
measurement(m20, arjuna, resistance(individual), 100, 0.02). % Submission to Divine Will

measurement(m21, pandava_host, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, pandava_host, stakes_inflation(organizational), 100, 0.95).
measurement(m23, pandava_host, suppression(organizational), 100, 0.05).
measurement(m24, pandava_host, resistance(organizational), 100, 0.98).

measurement(m25, kshatriya_varna, accessibility_collapse(class), 100, 0.05).
measurement(m26, kshatriya_varna, stakes_inflation(class), 100, 0.90).
measurement(m27, kshatriya_varna, suppression(class), 100, 0.20).
measurement(m28, kshatriya_varna, resistance(class), 100, 0.95).

measurement(m29, brahman_structure, accessibility_collapse(structural), 100, 0.00).
measurement(m30, brahman_structure, stakes_inflation(structural), 100, 0.50).
measurement(m31, brahman_structure, suppression(structural), 100, 0.00). % Beneficiary Rule
measurement(m32, brahman_structure, resistance(structural), 100, 0.00). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(kurukshetra_dialogue, brahman_structure, 'Pacifist_Ascetic_Flight').
intent_alternative_rejected(kurukshetra_dialogue, brahman_structure, 'Pacifist_Ascetic_Flight').

intent_beneficiary_class(kurukshetra_dialogue, dharma_system).
intent_power_change(kurukshetra_dialogue, dharma_system, 0.90). % Structural restoration of Cosmic Order

intent_loser_class(kurukshetra_dialogue, kinship_class). % Personal emotional ties sacrificed for Duty
intent_power_change(kurukshetra_dialogue, kinship_class, -0.75).

intent_suppression_level(kurukshetra_dialogue, brahman_structure, structural, 0.0).
intent_resistance_level(kurukshetra_dialogue, brahman_structure, structural, 0.0).

intent_norm_strength(kurukshetra_dialogue, 0, 0.45). % Norm of duty is contested by Arjuna's pity
intent_norm_strength(kurukshetra_dialogue, 100, 1.00). % Absolute alignment achieved via Viswarupa
