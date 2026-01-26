% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Military Intervention (Venezuela)
% Domain: Geopolitical Coercion - Sanctions, Sovereignty, and Interventionism
% ==========================================================

% --- 1. Entities & Intervals ---
entity(nicolas_maduro, individual).
entity(opposition_leadership, individual).
entity(fanb_military, organizational).
entity(us_state_dept, organizational).
entity(venezuelan_populace, class).
entity(ruling_chavista_elite, class).
entity(global_oil_market, structural).
entity(monroe_doctrine_legacy, structural).

interval(intervention_pressure_cycle, 0, 100).

% --- 2. Events ---
event(ev01_sanctions_expansion, economic_action, 10, [actor(us_state_dept), target(ruling_chavista_elite), scope(financial_oil)]).
event(ev02_recognition_pivot, diplomatic_challenge, 35, [actor(opposition_leadership), status(interim_presidency)]).
event(ev03_military_defection_failure, institutional_stress, 65, [actor(fanb_military), result(status_quo_retention)]).
event(ev04_humanitarian_migration_peak, demographic_shift, 90, [actor(venezuelan_populace), scale(millions)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute physical/economic reality of Oil Monoculture and Infrastructure Decay.
constraint_claim(oil_dependency_trap, mountain).
constraint_metric(oil_dependency_trap, accessibility_collapse, 0.95).

% Snare: International Financial Sanctions. A closing circle that traps the state's ability to settle accounts.
constraint_claim(financial_sanctions_noose, snare).
constraint_metric(financial_sanctions_noose, stakes_inflation, 0.92).

% Piton: Absolute Westphalian Sovereignty. A 'dead' principle cited by the state while economic control is externalized.
constraint_claim(zombie_sovereignty, piton).
constraint_metric(zombie_sovereignty, suppression, 0.85).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt a neutral regional mediation framework to bypass binary leadership conflicts.').
recommendation(rec02, 'Automate humanitarian aid corridors via non-aligned international bodies to reduce elite gatekeeping.').

affects_constraint(rec01, zombie_sovereignty).
affects_constraint(rec02, financial_sanctions_noose).

veto_actor(us_state_dept).
veto_actor(ruling_chavista_elite).

veto_exposed(us_state_dept, rec01).
veto_exposed(ruling_chavista_elite, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Onset of High-Pressure Strategy / 2017)
measurement(m01, nicolas_maduro, accessibility_collapse(individual), 0, 0.20).
measurement(m02, nicolas_maduro, stakes_inflation(individual), 0, 0.40).
measurement(m03, nicolas_maduro, suppression(individual), 0, 0.15).
measurement(m04, nicolas_maduro, resistance(individual), 0, 0.90).

measurement(m05, us_state_dept, accessibility_collapse(organizational), 0, 0.05).
measurement(m06, us_state_dept, stakes_inflation(organizational), 0, 0.15).
measurement(m07, us_state_dept, suppression(organizational), 0, 0.00). % Beneficiary Logic
measurement(m08, us_state_dept, resistance(organizational), 0, 0.00). % Beneficiary Logic

measurement(m09, venezuelan_populace, accessibility_collapse(class), 0, 0.45).
measurement(m10, venezuelan_populace, stakes_inflation(class), 0, 0.60).
measurement(m11, venezuelan_populace, suppression(class), 0, 0.35).
measurement(m12, venezuelan_populace, resistance(class), 0, 0.50).

measurement(m13, global_oil_market, accessibility_collapse(structural), 0, 0.10).
measurement(m14, global_oil_market, stakes_inflation(structural), 0, 0.20).
measurement(m15, global_oil_market, suppression(structural), 0, 0.00).
measurement(m16, global_oil_market, resistance(structural), 0, 0.00).

% Time Tn (Maturity of Stalemate and Structural Coercion / 2025)
measurement(m17, nicolas_maduro, accessibility_collapse(individual), 100, 0.85). % Options locked to internal survival
measurement(m18, nicolas_maduro, stakes_inflation(individual), 100, 1.00). % Survival is the only stake
measurement(m19, nicolas_maduro, suppression(individual), 100, 0.20). % High elite cohesion
measurement(m20, nicolas_maduro, resistance(individual), 100, 0.95). % Defiant retrenchment

measurement(m21, us_state_dept, accessibility_collapse(organizational), 100, 0.00).
measurement(m22, us_state_dept, stakes_inflation(organizational), 100, 0.95).
measurement(m23, us_state_dept, suppression(organizational), 100, 0.00). % Beneficiary Logic
measurement(m24, us_state_dept, resistance(organizational), 100, 0.00). % Beneficiary Logic

measurement(m25, venezuelan_populace, accessibility_collapse(class), 100, 0.95). % Total loss of economic agency
measurement(m26, venezuelan_populace, stakes_inflation(class), 100, 1.00). % Basic biological survival
measurement(m27, venezuelan_populace, suppression(class), 100, 0.90). % Mass migration as evidence
measurement(m28, venezuelan_populace, resistance(class), 100, 0.10). % Attrition-based compliance

measurement(m29, global_oil_market, accessibility_collapse(structural), 100, 0.00).
measurement(m30, global_oil_market, stakes_inflation(structural), 100, 0.60).
measurement(m31, global_oil_market, suppression(structural), 100, 0.00).
measurement(m32, global_oil_market, resistance(structural), 100, 0.00).

% --- 6. Intent Evidence ---
intent_viable_alternative(intervention_pressure_cycle, global_oil_market, 'Sanctions_Moratorium_for_Humanitarian_Exchange').
intent_alternative_rejected(intervention_pressure_cycle, global_oil_market, 'Sanctions_Moratorium_for_Humanitarian_Exchange').

intent_beneficiary_class(intervention_pressure_cycle, us_state_dept).
intent_power_change(intervention_pressure_cycle, us_state_dept, 0.40). % Strategic dominance through attrition

intent_loser_class(intervention_pressure_cycle, venezuelan_populace).
intent_power_change(intervention_pressure_cycle, venezuelan_populace, -0.90). % Complete socio-economic hollow-out

intent_suppression_level(intervention_pressure_cycle, us_state_dept, structural, 0.0).
intent_resistance_level(intervention_pressure_cycle, us_state_dept, structural, 0.0).

intent_norm_strength(intervention_pressure_cycle, 0, 0.75). % Monroe doctrine/Sovereignty contested
intent_norm_strength(intervention_pressure_cycle, 100, 0.98). % Sanctions-state as absolute standard
