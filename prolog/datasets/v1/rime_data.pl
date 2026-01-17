% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: The Rime of the Ancient Mariner
% ==========================================================

% --- 1. Entities & Intervals ---
entity(mariner, individual).
entity(wedding_guest, individual).
entity(albatross, individual).
entity(ship_crew, class).
entity(polar_spirit, individual).
entity(life_in_death, individual).
entity(divine_moral_order, structural).
entity(the_ship, organizational).

interval(voyage_of_penance, 0, 100).

% --- 2. Events ---
event(ev01_shooting, violation, 5, [actor(mariner), target(albatross), tool(cross_bow)]).
event(ev02_becalmed, constraint_initiation, 20, [state(silent_sea), condition(no_wind)]).
event(ev03_dice_game, arbitration, 45, [actor(life_in_death), target(mariner), result(mariner_won_by_life_in_death)]).
event(ev04_crew_expiry, mass_fatality, 50, [count(200), manner(cursing_eye)]).
event(ev05_blessing, pivot, 70, [target(water_snakes), action(blessed_unaware)]).
event(ev06_agony_start, perpetual_loop, 95, [symptom(heart_burns), action(tale_teaching)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The physical impossibility of movement in the 'Silent Sea' (Natural/Biological limit).
constraint_claim(the_silent_sea, mountain).
constraint_metric(the_silent_sea, accessibility_collapse, 0.95).

% Noose: The Albatross hung around the neck (Social/Psychological trap).
constraint_claim(albatross_stigma, noose).
constraint_metric(albatross_stigma, stakes_inflation, 0.85).

% Zombie: The superstition that the bird brought the fog/wind (Inert narrative controlling behavior).
constraint_claim(crew_superstition, zombie).
constraint_metric(crew_superstition, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Abolish the compulsory repetition of the narrative to allow the Mariner\'s reintegration into the Wedding feast.').
recommendation(rec02, 'Replace the supernatural arbitration of \'Life-in-Death\' with a transparent restorative justice framework.').

affects_constraint(rec01, albatross_stigma).
affects_constraint(rec02, the_silent_sea).

veto_actor(polar_spirit).
veto_actor(divine_moral_order).

veto_exposed(polar_spirit, rec02).
veto_exposed(divine_moral_order, rec01).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and T100) ---

% Time T0 (Initial Breach: The Shooting of the Albatross)
measurement(m01, mariner, accessibility_collapse(individual), 0, 0.10).
measurement(m02, mariner, stakes_inflation(individual), 0, 0.15).
measurement(m03, mariner, suppression(individual), 0, 0.05).
measurement(m04, mariner, resistance(individual), 0, 0.80).

measurement(m05, the_ship, accessibility_collapse(organizational), 0, 0.05).
measurement(m06, the_ship, stakes_inflation(organizational), 0, 0.10).
measurement(m07, the_ship, suppression(organizational), 0, 0.00).
measurement(m08, the_ship, resistance(organizational), 0, 0.00).

measurement(m09, ship_crew, accessibility_collapse(class), 0, 0.05).
measurement(m10, ship_crew, stakes_inflation(class), 0, 0.10).
measurement(m11, ship_crew, suppression(class), 0, 0.20).
measurement(m12, ship_crew, resistance(class), 0, 0.10).

measurement(m13, divine_moral_order, accessibility_collapse(structural), 0, 0.00).
measurement(m14, divine_moral_order, stakes_inflation(structural), 0, 0.00).
measurement(m15, divine_moral_order, suppression(structural), 0, 0.00). % Beneficiary Rule
measurement(m16, divine_moral_order, resistance(structural), 0, 0.00). % Beneficiary Rule

% Time T100 (The Perpetual State: Eternal Penance / 'The Agony')
measurement(m17, mariner, accessibility_collapse(individual), 100, 0.90). % Cannot join wedding/society
measurement(m18, mariner, stakes_inflation(individual), 100, 1.00). % Eternal soul at stake
measurement(m19, mariner, suppression(individual), 100, 0.95). % Compelled to tell tale
measurement(m20, mariner, resistance(individual), 100, 0.00). % Submission to the agony

measurement(m21, the_ship, accessibility_collapse(organizational), 100, 1.00). % Ship is sunk
measurement(m22, the_ship, stakes_inflation(organizational), 100, 1.00).
measurement(m23, the_ship, suppression(organizational), 100, 1.00).
measurement(m24, the_ship, resistance(organizational), 100, 0.00).

measurement(m25, ship_crew, accessibility_collapse(class), 100, 1.00). % All dead
measurement(m26, ship_crew, stakes_inflation(class), 100, 1.00).
measurement(m27, ship_crew, suppression(class), 100, 1.00).
measurement(m28, ship_crew, resistance(class), 100, 0.00).

measurement(m29, divine_moral_order, accessibility_collapse(structural), 100, 0.00).
measurement(m30, divine_moral_order, stakes_inflation(structural), 100, 0.30).
measurement(m31, divine_moral_order, suppression(structural), 100, 0.00). % Beneficiary Rule
measurement(m32, divine_moral_order, resistance(structural), 100, 0.00). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(voyage_of_penance, divine_moral_order, 'Secular_Restorative_Justice').
intent_alternative_rejected(voyage_of_penance, divine_moral_order, 'Secular_Restorative_Justice').

intent_beneficiary_class(voyage_of_penance, divine_moral_order).
intent_power_change(voyage_of_penance, divine_moral_order, 0.40). % Reinforcement of universal moral law

intent_loser_class(voyage_of_penance, ship_crew).
intent_power_change(voyage_of_penance, ship_crew, -1.00). % Total class destruction

intent_suppression_level(voyage_of_penance, divine_moral_order, structural, 0.0).
intent_resistance_level(voyage_of_penance, divine_moral_order, structural, 0.0).

intent_norm_strength(voyage_of_penance, 0, 0.10).   % Mariner ignores sanctity of life
intent_norm_strength(voyage_of_penance, 100, 0.95). % Totalizing enforcement of \'Love all things\'
