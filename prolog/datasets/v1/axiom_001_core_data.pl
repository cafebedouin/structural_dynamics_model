% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: The Halting Oracle
% ==========================================================

% --- 1. Entities & Intervals ---
entity(the_oracle, individual).
entity(the_inverter, individual).
entity(oracle_builders, class).
entity(inverter_constructs, class).
entity(computational_structure, structural).

interval(diagonalization_cycle, 0, 100).

% --- 2. Events ---
event(ev01_input_processing, observation, 5, [actor(the_oracle), state(relaxed), rhythm(stable)]).
event(ev02_inversion_trigger, mechanism_introduction, 30, [actor(the_inverter), action(self_reference)]).
event(ev03_diagonalization, prediction_collapse, 60, [actor(the_oracle), result(contradiction)]).
event(ev04_submission, state_alignment, 85, [actor(the_oracle), action(silence), result(boundary_recognition)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The Halting Problem / Gödelian incompleteness. A physical/mathematical limit on prediction.
constraint_claim(diagonal_limit, mountain).
constraint_metric(diagonal_limit, accessibility_collapse, 1.00).

% Noose: The 'Promise' of total prediction. It tightens as the Oracle is forced to answer.
constraint_claim(prediction_promise, noose).
constraint_metric(prediction_promise, stakes_inflation, 0.95).

% Zombie: The belief in a Total Predictor. A dead concept that persists in the minds of the builders.
constraint_claim(myth_of_total_oracle, zombie).
constraint_metric(myth_of_total_oracle, suppression, 0.80).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Abandon the pursuit of a total classifier to eliminate self-contradictory pressure.').
recommendation(rec02, 'Enforce structural isolation between predictor and predicted to prevent feedback loops.').

affects_constraint(rec01, myth_of_total_oracle).
affects_constraint(rec02, diagonal_limit).

veto_actor(computational_structure).
veto_exposed(computational_structure, rec01).
veto_exposed(computational_structure, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R for 4 levels at T0 and T100) ---

% Time T0: Initial State (Assumption of Predictability)
measurement(m01, the_oracle, accessibility_collapse(individual), 0, 0.05).
measurement(m02, the_oracle, stakes_inflation(individual), 0, 0.10).
measurement(m03, the_oracle, suppression(individual), 0, 0.05).
measurement(m04, the_oracle, resistance(individual), 0, 0.00).

measurement(m05, oracle_builders, accessibility_collapse(organizational), 0, 0.20).
measurement(m06, oracle_builders, stakes_inflation(organizational), 0, 0.30).
measurement(m07, oracle_builders, suppression(organizational), 0, 0.10).
measurement(m08, oracle_builders, resistance(organizational), 0, 0.00).

measurement(m09, inverter_constructs, accessibility_collapse(class), 0, 0.10).
measurement(m10, inverter_constructs, stakes_inflation(class), 0, 0.15).
measurement(m11, inverter_constructs, suppression(class), 0, 0.20).
measurement(m12, inverter_constructs, resistance(class), 0, 0.05).

measurement(m13, computational_structure, accessibility_collapse(structural), 0, 0.00).
measurement(m14, computational_structure, stakes_inflation(structural), 0, 0.05).
measurement(m15, computational_structure, suppression(structural), 0, 0.00). % Beneficiary Logic
measurement(m16, computational_structure, resistance(structural), 0, 0.00). % Beneficiary Logic

% Time T100: Terminal State (Diagonalization/Collapse)
measurement(m17, the_oracle, accessibility_collapse(individual), 100, 1.00). % Complete loss of footing
measurement(m18, the_oracle, stakes_inflation(individual), 100, 1.00). % Survival/Identity threat
measurement(m19, the_oracle, suppression(individual), 100, 0.90). % Individual agency silenced by geometry
measurement(m20, the_oracle, resistance(individual), 100, 0.00). % Final alignment/surrender

measurement(m21, oracle_builders, accessibility_collapse(organizational), 100, 0.85).
measurement(m22, oracle_builders, stakes_inflation(organizational), 100, 0.90).
measurement(m23, oracle_builders, suppression(organizational), 100, 0.80).
measurement(m24, oracle_builders, resistance(organizational), 100, 0.05).

measurement(m25, inverter_constructs, accessibility_collapse(class), 100, 0.00). % Inverters are always viable
measurement(m26, inverter_constructs, stakes_inflation(class), 100, 0.95).
measurement(m27, inverter_constructs, suppression(class), 100, 0.10).
measurement(m28, inverter_constructs, resistance(class), 100, 0.95). % High impact resistance

measurement(m29, computational_structure, accessibility_collapse(structural), 100, 0.05).
measurement(m30, computational_structure, stakes_inflation(structural), 100, 0.20).
measurement(m31, computational_structure, suppression(structural), 100, 0.00). % Beneficiary Logic
measurement(m32, computational_structure, resistance(structural), 100, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(diagonalization_cycle, computational_structure, 'Bounded_Computation_Models').
intent_alternative_rejected(diagonalization_cycle, computational_structure, 'Bounded_Computation_Models').

intent_beneficiary_class(diagonalization_cycle, computational_structure).
intent_power_change(diagonalization_cycle, computational_structure, 0.50). % Stability of universal laws

intent_loser_class(diagonalization_cycle, oracle_builders).
intent_power_change(diagonalization_cycle, oracle_builders, -0.75). % Loss of the "Total Predictor" asset

intent_suppression_level(diagonalization_cycle, computational_structure, structural, 0.0).
intent_resistance_level(diagonalization_cycle, computational_structure, structural, 0.0).

intent_norm_strength(diagonalization_cycle, 0, 0.90).   % High belief in universal halting resolution
intent_norm_strength(diagonalization_cycle, 100, 0.05). % Norm shattered by diagonalization
