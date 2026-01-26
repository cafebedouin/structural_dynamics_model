% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: The Wanderer and the Weight
% ==========================================================

% --- 1. Entities & Intervals ---
entity(wanderer, individual).
entity(constellation_feedback, organizational).
entity(wanderer_class, class).
entity(the_landscape, structural).

interval(wandering_convergence_01, 0, 100).

% --- 2. Events ---
event(ev01_first_steps, orientation, 0, [origin(unmarked), direction(random)]).
event(ev02_pattern_recognition, insight, 45, [object(constellation_oscillation), state(stabilizing)]).
event(ev03_convergence, transition, 95, [transformation(time_to_space), identity(equivalence_with_whole)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The objective, immutable Weight distribution of the terrain.
constraint_claim(immutable_landscape_weight, mountain).
constraint_metric(immutable_landscape_weight, accessibility_collapse, 0.90).

% Snare: The cumulative tracking of the sky (Stars). The past piles up, tightening the statistical baseline.
constraint_claim(cumulative_history_tracking, snare).
constraint_metric(cumulative_history_tracking, stakes_inflation, 0.85).

% Piton: The 'unmarked point' or 'missing map.' The belief that navigation requires an origin dot.
constraint_claim(myth_of_the_origin_dot, piton).
constraint_metric(myth_of_the_origin_dot, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Acknowledge the invariance of the distribution rather than seeking to map specific coordinates.').
recommendation(rec02, 'Decouple current step autonomy from the cumulative tracking constellations to restore \'here-and-now\' focus.').

affects_constraint(rec01, myth_of_the_origin_dot).
affects_constraint(rec02, cumulative_history_tracking).

veto_actor(the_landscape).
veto_exposed(the_landscape, rec01).
veto_exposed(the_landscape, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R for 4 levels at T0 and T100) ---

% Time T0: The Start (Maximum Entropy/Freedom)
measurement(m01, wanderer, accessibility_collapse(individual), 0, 0.10).
measurement(m02, wanderer, stakes_inflation(individual), 0, 0.20).
measurement(m03, wanderer, suppression(individual), 0, 0.05).
measurement(m04, wanderer, resistance(individual), 0, 0.00).

measurement(m05, constellation_feedback, accessibility_collapse(organizational), 0, 0.30).
measurement(m06, constellation_feedback, stakes_inflation(organizational), 0, 0.40).
measurement(m07, constellation_feedback, suppression(organizational), 0, 0.20).
measurement(m08, constellation_feedback, resistance(organizational), 0, 0.10).

measurement(m09, wanderer_class, accessibility_collapse(class), 0, 0.25).
measurement(m10, wanderer_class, stakes_inflation(class), 0, 0.35).
measurement(m11, wanderer_class, suppression(class), 0, 0.15).
measurement(m12, wanderer_class, resistance(class), 0, 0.05).

measurement(m13, the_landscape, accessibility_collapse(structural), 0, 0.00).
measurement(m14, the_landscape, stakes_inflation(structural), 0, 0.10).
measurement(m15, the_landscape, suppression(structural), 0, 0.00). % Beneficiary Logic
measurement(m16, the_landscape, resistance(structural), 0, 0.00). % Beneficiary Logic

% Time T100: The Convergence (Statistical Identity)
measurement(m17, wanderer, accessibility_collapse(individual), 100, 0.85). % Individual path no longer changes the sum
measurement(m18, wanderer, stakes_inflation(individual), 100, 0.90). % Every step is weighted by the total history
measurement(m19, wanderer, suppression(individual), 100, 0.75). % Autonomy is subsumed by statistical inevitability
measurement(m20, wanderer, resistance(individual), 100, 0.10). % Minimal ability to deviate from the distribution

measurement(m21, constellation_feedback, accessibility_collapse(organizational), 100, 0.95).
measurement(m22, constellation_feedback, stakes_inflation(organizational), 100, 0.80).
measurement(m23, constellation_feedback, suppression(organizational), 100, 0.85).
measurement(m24, constellation_feedback, resistance(organizational), 100, 0.05).

measurement(m25, wanderer_class, accessibility_collapse(class), 100, 0.90).
measurement(m26, wanderer_class, stakes_inflation(class), 100, 0.95).
measurement(m27, wanderer_class, suppression(class), 100, 0.90).
measurement(m28, wanderer_class, resistance(class), 100, 0.00).

measurement(m29, the_landscape, accessibility_collapse(structural), 100, 0.05).
measurement(m30, the_landscape, stakes_inflation(structural), 100, 0.20).
measurement(m31, the_landscape, suppression(structural), 100, 0.00). % Beneficiary Logic
measurement(m32, the_landscape, resistance(structural), 100, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(wandering_convergence_01, the_landscape, 'Systematic_Grid_Mapping').
intent_alternative_rejected(wandering_convergence_01, the_landscape, 'Systematic_Grid_Mapping').

intent_beneficiary_class(wandering_convergence_01, the_landscape).
intent_power_change(wandering_convergence_01, the_landscape, 0.45). % Structural persistence/inevitability gained

intent_loser_class(wandering_convergence_01, wanderer).
intent_power_change(wandering_convergence_01, wanderer, -0.65). % Loss of unique path-autonomy vs the ensemble

intent_suppression_level(wandering_convergence_01, the_landscape, structural, 0.0).
intent_resistance_level(wandering_convergence_01, the_landscape, structural, 0.0).

intent_norm_strength(wandering_convergence_01, 0, 0.10).   % Randomness/Entropy
intent_norm_strength(wandering_convergence_01, 100, 0.95). % Convergence/Law of Large Numbers
