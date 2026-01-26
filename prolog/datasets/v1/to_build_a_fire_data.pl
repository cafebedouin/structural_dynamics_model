% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: To Build a Fire
% Domain: Literary Naturalism - Thermodynamic Coercion
% ==========================================================

% --- 1. Entities & Intervals ---
entity(the_man, individual).
entity(the_dog, individual).
entity(the_boys_camp, organizational).
entity(old_timers_of_sulphur_creek, class).
entity(thermodynamics, structural).

interval(the_survival_arc, 0, 100). % From 9:00 AM (T0) to Death by Freezing (Tn)

% --- 2. Events ---
% 
event(ev01_creek_breakthrough, breakthrough, 30, [actor(the_man), severity(mid_thigh), temp('-75_f')]). %
event(ev02_fire_failure, accidental_extinguishment, 50, [cause(tree_snow_avalanche), location(under_spruce)]). %
event(ev03_dexterity_collapse, physiological_failure, 75, [part(fingers), part(feet), part(nose), part(cheeks)]). %
event(ev04_dog_kill_attempt, failed_violence, 85, [actor(the_man), target(the_dog), cause(lack_of_clutch)]). %
event(ev05_terminal_sleep, death_by_freezing, 100, [state(drowsiness), condition(anaesthetic_freeze)]). %

% --- 3. Constraint Claims & Metrics ---
% Mountain: Extreme Cold (-75F). The objective physical reality of space-cold smiting the planet.
constraint_claim(absolute_cold_threshold, mountain).
constraint_metric(absolute_cold_threshold, accessibility_collapse, 1.0).

% Snare: The Wet Foot. Once wetted, the time-to-fire narrows the man's survival window to a single path.
constraint_claim(wet_foot_urgency, snare).
constraint_metric(wet_foot_urgency, stakes_inflation, 0.98).

% Piton: Lack of Imagination. The man's initial belief that 50 below is just 'uncomfortable' rather than significant.
constraint_claim(procedural_judgment_bias, piton).
constraint_metric(procedural_judgment_bias, suppression, 0.85).

% Rope: Travel Loneliness. Ties the man's survival solely to his own failing biological dexterity.
constraint_claim(solitary_travel_binding, rope).
constraint_metric(solitary_travel_binding, suppression, 0.90).

% --- 4. Recommendations & Veto Structure ---
% 
recommendation(rec01, 'Travel with a trail-mate after fifty below to ensure redundant fire-building capacity.'). %
recommendation(rec02, 'Build survival fires in the open, away from snow-laden spruce boughs.'). %

affects_constraint(rec01, solitary_travel_binding).
affects_constraint(rec02, wet_foot_urgency).

veto_actor(the_man).
veto_exposed(the_man, rec01). % The man explicitly mocks and rejects the advice of the old-timers.

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (9:00 AM - Journey Start)
measurement(m01, the_man, accessibility_collapse(individual), 0, 0.10).
measurement(m02, the_man, stakes_inflation(individual), 0, 0.20).
measurement(m03, the_man, suppression(individual), 0, 0.05).
measurement(m04, the_man, resistance(individual), 0, 0.05).

measurement(m05, the_boys_camp, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, the_boys_camp, stakes_inflation(organizational), 0, 0.10).
measurement(m07, the_boys_camp, suppression(organizational), 0, 0.00).
measurement(m08, the_boys_camp, resistance(organizational), 0, 0.00).

measurement(m09, old_timers_of_sulphur_creek, accessibility_collapse(class), 0, 0.05).
measurement(m10, old_timers_of_sulphur_creek, stakes_inflation(class), 0, 0.10).
measurement(m11, old_timers_of_sulphur_creek, suppression(class), 0, 0.05).
measurement(m12, old_timers_of_sulphur_creek, resistance(class), 0, 0.95).

measurement(m13, thermodynamics, accessibility_collapse(structural), 0, 0.00).
measurement(m14, thermodynamics, stakes_inflation(structural), 0, 1.00).
measurement(m15, thermodynamics, suppression(structural), 0, 0.00). % Beneficiary Logic
measurement(m16, thermodynamics, resistance(structural), 0, 0.00). % Beneficiary Logic

% Time Tn (Terminal Drowsiness - Survival Failure)
measurement(m17, the_man, accessibility_collapse(individual), 100, 1.00).
measurement(m18, the_man, stakes_inflation(individual), 100, 1.00).
measurement(m19, the_man, suppression(individual), 100, 1.00).
measurement(m20, the_man, resistance(individual), 100, 0.00).

measurement(m21, the_boys_camp, accessibility_collapse(organizational), 100, 0.95). % Goal unreached
measurement(m22, the_boys_camp, stakes_inflation(organizational), 100, 0.90).
measurement(m23, the_boys_camp, suppression(organizational), 100, 0.10).
measurement(m24, the_boys_camp, resistance(organizational), 100, 0.00).

measurement(m25, old_timers_of_sulphur_creek, accessibility_collapse(class), 100, 0.00).
measurement(m26, old_timers_of_sulphur_creek, stakes_inflation(class), 100, 0.80). % Vindicated
measurement(m27, old_timers_of_sulphur_creek, suppression(class), 100, 0.00).
measurement(m28, old_timers_of_sulphur_creek, resistance(class), 100, 0.00).

measurement(m29, thermodynamics, accessibility_collapse(structural), 100, 0.00).
measurement(m30, thermodynamics, stakes_inflation(structural), 100, 0.50).
measurement(m31, thermodynamics, suppression(structural), 100, 0.00). % Beneficiary Logic
measurement(m32, thermodynamics, resistance(structural), 100, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
% 
intent_viable_alternative(the_survival_arc, thermodynamics, 'Companion_Travel_Protocol'). %
intent_alternative_rejected(the_survival_arc, thermodynamics, 'Companion_Travel_Protocol'). %

intent_beneficiary_class(the_survival_arc, thermodynamics). %
intent_power_change(the_survival_arc, thermodynamics, 1.00). % Nature totalizes its dominance over the individual.

intent_loser_class(the_survival_arc, the_man). %
intent_power_change(the_survival_arc, the_man, -1.00). % Complete biological liquidation.

intent_suppression_level(the_survival_arc, thermodynamics, structural, 0.0).
intent_resistance_level(the_survival_arc, thermodynamics, structural, 0.0).

intent_norm_strength(the_survival_arc, 0, 0.10). % The man initialy disregards the 'law' of the Yukon.
intent_norm_strength(the_survival_arc, 100, 1.00). % The old-timer's norm is vindicated as absolute.
