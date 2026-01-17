% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: The Arbiter
% Domain: Social Choice Theory - Arrow's Impossibility Theorem
% ==========================================================

% --- 1. Entities & Intervals ---
entity(the_arbiter, individual).
entity(the_circle, class).
entity(one_agent, individual).
entity(the_axioms, organizational).
entity(social_choice_mechanism, structural).

interval(axiom_contraction_cycle, 0, 100).

% --- 2. Events ---
event(ev01_alignment, coordination, 5, [actor(the_circle), target(the_arbiter), property(unanimity)]).
event(ev02_forced_steps, logical_transitivity, 30, [mechanism(pairwise_comparison), effect(forced_gait)]).
event(ev03_contraction, subset_splitting, 60, [action(mechanical_cut), target(the_circle), result(decisive_point)]).
event(ev04_the_snap, rule_failure, 95, [axiom(non_dictatorship), state(broken)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: Transitivity. The mathematical logic of forced steps (If A>B and B>C then A>C).
constraint_claim(transitive_rationality, mountain).
constraint_metric(transitive_rationality, accessibility_collapse, 0.95).

% Noose: Independence of Irrelevant Alternatives. Clamps the ribs; each comparison must stand alone.
constraint_claim(independence_clamp, noose).
constraint_metric(independence_clamp, stakes_inflation, 0.90).

% Zombie: Non-dictatorship. The rule that 'no one gets to dominate' which snaps under the weight of the others.
constraint_claim(non_dictatorship_ideal, zombie).
constraint_metric(non_dictatorship_ideal, suppression, 0.85).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Abandon the independence axiom to allow contextual cross-talk between comparisons.').
recommendation(rec02, 'Adopt cardinal utility scales to bypass the limitations of ordinal ranking.').

affects_constraint(rec01, independence_clamp).
affects_constraint(rec02, transitive_rationality).

veto_actor(the_axioms).
veto_exposed(the_axioms, rec01).
veto_exposed(the_axioms, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial Entry / Broad Potential)
measurement(m01, the_arbiter, accessibility_collapse(individual), 0, 0.10).
measurement(m02, the_arbiter, stakes_inflation(individual), 0, 0.20).
measurement(m03, the_arbiter, suppression(individual), 0, 0.15).
measurement(m04, the_arbiter, resistance(individual), 0, 0.05).

measurement(m05, the_axioms, accessibility_collapse(organizational), 0, 0.05).
measurement(m06, the_axioms, stakes_inflation(organizational), 0, 0.10).
measurement(m07, the_axioms, suppression(organizational), 0, 0.00).
measurement(m08, the_axioms, resistance(organizational), 0, 0.00).

measurement(m09, the_circle, accessibility_collapse(class), 0, 0.20).
measurement(m10, the_circle, stakes_inflation(class), 0, 0.30).
measurement(m11, the_circle, suppression(class), 0, 0.10).
measurement(m12, the_circle, resistance(class), 0, 0.10).

measurement(m13, social_choice_mechanism, accessibility_collapse(structural), 0, 0.00).
measurement(m14, social_choice_mechanism, stakes_inflation(structural), 0, 0.05).
measurement(m15, social_choice_mechanism, suppression(structural), 0, 0.00). % Beneficiary logic
measurement(m16, social_choice_mechanism, resistance(structural), 0, 0.00). % Beneficiary logic

% Time Tn (Terminal Convergence / Dictatorship)
measurement(m17, the_arbiter, accessibility_collapse(individual), 100, 1.00). % Choice space is zero
measurement(m18, the_arbiter, stakes_inflation(individual), 100, 1.00). % Totalized identification
measurement(m19, the_arbiter, suppression(individual), 100, 0.95). % Folded into the structure
measurement(m20, the_arbiter, resistance(individual), 100, 0.00). % Clarity/Alignment

measurement(m21, the_axioms, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, the_axioms, stakes_inflation(organizational), 100, 0.90).
measurement(m23, the_axioms, suppression(organizational), 100, 0.05).
measurement(m24, the_axioms, resistance(organizational), 100, 0.95).

measurement(m25, the_circle, accessibility_collapse(class), 100, 0.95). % Weightless/Discarded
measurement(m26, the_circle, stakes_inflation(class), 100, 0.95).
measurement(m27, the_circle, suppression(class), 100, 1.00). % Absolute erasure
measurement(m28, the_circle, resistance(class), 100, 0.10).

measurement(m29, social_choice_mechanism, accessibility_collapse(structural), 100, 0.00).
measurement(m30, social_choice_mechanism, stakes_inflation(structural), 100, 0.50).
measurement(m31, social_choice_mechanism, suppression(structural), 100, 0.00). % Beneficiary logic
measurement(m32, social_choice_mechanism, resistance(structural), 100, 0.00). % Beneficiary logic

% --- 6. Intent Evidence ---
intent_viable_alternative(axiom_contraction_cycle, social_choice_mechanism, 'Relaxed_Independence_Contextualism').
intent_alternative_rejected(axiom_contraction_cycle, social_choice_mechanism, 'Relaxed_Independence_Contextualism').

intent_beneficiary_class(axiom_contraction_cycle, one_agent).
intent_power_change(axiom_contraction_cycle, one_agent, 0.95). % Emergence of the dictator

intent_loser_class(axiom_contraction_cycle, the_circle).
intent_power_change(axiom_contraction_cycle, the_circle, -0.85). % Total loss of collective leverage

intent_suppression_level(axiom_contraction_cycle, one_agent, structural, 0.0).
intent_resistance_level(axiom_contraction_cycle, one_agent, structural, 0.0).

intent_norm_strength(axiom_contraction_cycle, 0, 0.90). % Belief in four rules
intent_norm_strength(axiom_contraction_cycle, 100, 0.10). % Norm of non-dictatorship shattered
