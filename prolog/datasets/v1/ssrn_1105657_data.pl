% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: SSRN-1105657
% Domain: Constitutional Theory - Judicial Review and Democratic Legitimacy
% ==========================================================

% --- 1. Entities & Intervals ---
entity(individual_judge, individual).
entity(judicial_branch, organizational).
entity(legislative_branch, organizational).
entity(the_voting_public, class).
entity(legal_elite_class, class).
entity(constitutional_democracy, structural).

interval(constitutional_review_period, 1803, 2026).

% --- 2. Events ---
event(ev01_marbury_v_madison, legal_initiation, 1803, [actor(judicial_branch), concept(judicial_supremacy)]).
event(ev02_counter_majoritarian_difficulty, parameter_setting, 1962, [actor(legal_elite_class), logic(democratic_deficit)]).
event(ev03_interpretive_shift, systemic_overhaul, 1985, [subject(originalism_vs_living_constitution), result(high_partisan_stakes)]).
event(ev04_legitimacy_crisis, final_verification, 2026, [subject(judicial_review_solvency), result(structural_coercion)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The Rule of Law. The physical and objective necessity of stable legal rules for social order.
constraint_claim(rule_of_law_stability, mountain).
constraint_metric(rule_of_law_stability, accessibility_collapse, 0.95).

% Noose: The Counter-Majoritarian Difficulty. The democratic deficit that tightens as judicial reach expands.
constraint_claim(counter_majoritarian_difficulty, noose).
constraint_metric(counter_majoritarian_difficulty, stakes_inflation, 0.90).

% Zombie: Originalism. A concept of 'dead hand' control that persists as a living regulatory force.
constraint_claim(originalist_interpretation_myth, zombie).
constraint_metric(originalist_interpretation_myth, suppression, 0.75).

% Rope: Stare Decisis. Ties current judicial behavior to rigid historical precedents.
constraint_claim(precedential_binding, rope).
constraint_metric(precedential_binding, suppression, 0.65).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt popular constitutionalism to bypass judicial interpretive monopoly.').
recommendation(rec02, 'Implement term limits for individual judges to reduce individual-level stakes inflation.').

affects_constraint(rec01, counter_majoritarian_difficulty).
affects_constraint(rec02, originalist_interpretation_myth).

veto_actor(judicial_branch).
veto_actor(legal_elite_class).

veto_exposed(judicial_branch, rec01).
veto_exposed(legal_elite_class, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (1803: Initial Establishment)
measurement(m01, individual_judge, accessibility_collapse(individual), 1803, 0.20).
measurement(m02, individual_judge, stakes_inflation(individual), 1803, 0.30).
measurement(m03, individual_judge, suppression(individual), 1803, 0.15).
measurement(m04, individual_judge, resistance(individual), 1803, 0.05).

measurement(m05, judicial_branch, accessibility_collapse(organizational), 1803, 0.10).
measurement(m06, judicial_branch, stakes_inflation(organizational), 1803, 0.25).
measurement(m07, judicial_branch, suppression(organizational), 1803, 0.05).
measurement(m08, judicial_branch, resistance(organizational), 1803, 0.00).

measurement(m09, the_voting_public, accessibility_collapse(class), 1803, 0.05).
measurement(m10, the_voting_public, stakes_inflation(class), 1803, 0.15).
measurement(m11, the_voting_public, suppression(class), 1803, 0.10).
measurement(m12, the_voting_public, resistance(class), 1803, 0.90).

measurement(m13, constitutional_democracy, accessibility_collapse(structural), 1803, 0.05).
measurement(m14, constitutional_democracy, stakes_inflation(structural), 1803, 0.10).
measurement(m15, constitutional_democracy, suppression(structural), 1803, 0.00). % Beneficiary logic
measurement(m16, constitutional_democracy, resistance(structural), 1803, 0.00). % Beneficiary logic

% Time Tn (2026: Maturity and Legitimacy Conflict)
measurement(m17, individual_judge, accessibility_collapse(individual), 2026, 0.85).
measurement(m18, individual_judge, stakes_inflation(individual), 2026, 1.00). % Absolute lifetime stakes
measurement(m19, individual_judge, suppression(individual), 2026, 0.90). % Agency replaced by doctrine
measurement(m20, individual_judge, resistance(individual), 2026, 0.05).

measurement(m21, judicial_branch, accessibility_collapse(organizational), 2026, 0.05).
measurement(m22, judicial_branch, stakes_inflation(organizational), 2026, 0.95).
measurement(m23, judicial_branch, suppression(organizational), 2026, 0.05).
measurement(m24, judicial_branch, resistance(organizational), 2026, 0.98).

measurement(m25, the_voting_public, accessibility_collapse(class), 2026, 0.05).
measurement(m26, the_voting_public, stakes_inflation(class), 2026, 0.90).
measurement(m27, the_voting_public, suppression(class), 2026, 0.40).
measurement(m28, the_voting_public, resistance(class), 2026, 0.60).

measurement(m29, constitutional_democracy, accessibility_collapse(structural), 2026, 0.00).
measurement(m30, constitutional_democracy, stakes_inflation(structural), 2026, 0.45).
measurement(m31, constitutional_democracy, suppression(structural), 2026, 0.00). % Beneficiary logic
measurement(m32, constitutional_democracy, resistance(structural), 2026, 0.00). % Beneficiary logic

% --- 6. Intent Evidence ---
intent_viable_alternative(constitutional_review_period, constitutional_democracy, 'Pure_Parliamentary_Supremacy').
intent_alternative_rejected(constitutional_review_period, constitutional_democracy, 'Pure_Parliamentary_Supremacy').

intent_beneficiary_class(constitutional_review_period, legal_elite_class).
intent_power_change(constitutional_review_period, legal_elite_class, 0.85). % Centralization of interpretive power

intent_loser_class(constitutional_review_period, the_voting_public). % Reduced direct democratic control
intent_power_change(constitutional_review_period, the_voting_public, -0.40).

intent_suppression_level(constitutional_review_period, legal_elite_class, structural, 0.0).
intent_resistance_level(constitutional_review_period, legal_elite_class, structural, 0.0).

intent_norm_strength(constitutional_review_period, 1803, 0.40). % Emerging judicial review
intent_norm_strength(constitutional_review_period, 2026, 0.98). % Absolute institutional dominance
