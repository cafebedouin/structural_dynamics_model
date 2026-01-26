% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: TFR83 Economic Security
% Domain: Macroeconomics - National Economic Security and Policy Coordination
% ==========================================================

% --- 1. Entities & Intervals ---
entity(economic_council, organizational).
entity(central_bank, organizational).
entity(labor_unions, class).
entity(capital_investment_firms, class).
entity(individual_household, individual).
entity(global_market_system, structural).

interval(economic_adjustment_cycle, 0, 100). % From Pre-Reform (T0) to Policy Equilibrium (Tn)

% --- 2. Events ---
event(ev01_policy_announcement, theoretical_initiation, 5, [actor(economic_council), concept(stabilization_mandate)]).
event(ev02_interest_rate_adjustment, parameter_setting, 35, [actor(central_bank), logic(inflation_control)]).
event(ev03_market_correction, systemic_overhaul, 70, [subject(liquidity_crisis), result(targeted_reconstruction)]).
event(ev04_security_verification, final_verification, 95, [subject(household_solvency), result(new_equilibrium)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute physical limit of Resource Scarcity and Productivity.
constraint_claim(resource_scarcity_limit, mountain).
constraint_metric(resource_scarcity_limit, accessibility_collapse, 0.95).

% Snare: The 'Promise' of Perpetual Growth. It tightens as environmental and debt limits are reached.
constraint_claim(growth_imperative_mandate, snare).
constraint_metric(growth_imperative_mandate, stakes_inflation, 0.90).

% Piton: Legacy Industrial Subsidies. Policy mechanisms that are functionally dead but continue to drain capital.
constraint_claim(legacy_subsidy_drain, piton).
constraint_metric(legacy_subsidy_drain, suppression, 0.75).

% Rope: Monetary Policy Standard. Ties all local economic activity to central interest rate constraints.
constraint_claim(monetary_standard_binding, rope).
constraint_metric(monetary_standard_binding, suppression, 0.65).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt a de-coupled local value standard to bypass central monetary volatility.').
recommendation(rec02, 'Implement automated universal basic dividend systems to reduce household debt exposure.').

affects_constraint(rec01, monetary_standard_binding).
affects_constraint(rec02, growth_imperative_mandate).

veto_actor(central_bank).
veto_actor(capital_investment_firms).

veto_exposed(central_bank, rec01).
veto_exposed(capital_investment_firms, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial Volatility State)
measurement(m01, individual_household, accessibility_collapse(individual), 0, 0.40).
measurement(m02, individual_household, stakes_inflation(individual), 0, 0.60).
measurement(m03, individual_household, suppression(individual), 0, 0.30).
measurement(m04, individual_household, resistance(individual), 0, 0.20).

measurement(m05, economic_council, accessibility_collapse(organizational), 0, 0.20).
measurement(m06, economic_council, stakes_inflation(organizational), 0, 0.30).
measurement(m07, economic_council, suppression(organizational), 0, 0.10).
measurement(m08, economic_council, resistance(organizational), 0, 0.05).

measurement(m09, labor_unions, accessibility_collapse(class), 0, 0.50).
measurement(m10, labor_unions, stakes_inflation(class), 0, 0.40).
measurement(m11, labor_unions, suppression(class), 0, 0.60).
measurement(m12, labor_unions, resistance(class), 0, 0.70).

measurement(m13, global_market_system, accessibility_collapse(structural), 0, 0.05).
measurement(m14, global_market_system, stakes_inflation(structural), 0, 0.15).
measurement(m15, global_market_system, suppression(structural), 0, 0.00). % Beneficiary Logic
measurement(m16, global_market_system, resistance(structural), 0, 0.00). % Beneficiary Logic

% Time Tn (Post-Stabilization Maturity)
measurement(m17, individual_household, accessibility_collapse(individual), 100, 0.05).
measurement(m18, individual_household, stakes_inflation(individual), 100, 0.95).
measurement(m19, individual_household, suppression(individual), 100, 0.15).
measurement(m20, individual_household, resistance(individual), 100, 0.85).

measurement(m21, economic_council, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, economic_council, stakes_inflation(organizational), 100, 0.90).
measurement(m23, economic_council, suppression(organizational), 100, 0.05).
measurement(m24, economic_council, resistance(organizational), 100, 0.95).

measurement(m25, labor_unions, accessibility_collapse(class), 100, 0.10).
measurement(m26, labor_unions, stakes_inflation(class), 100, 0.80).
measurement(m27, labor_unions, suppression(class), 100, 0.40).
measurement(m28, labor_unions, resistance(class), 100, 0.60).

measurement(m29, global_market_system, accessibility_collapse(structural), 100, 0.00).
measurement(m30, global_market_system, stakes_inflation(structural), 100, 0.40).
measurement(m31, global_market_system, suppression(structural), 100, 0.00). % Beneficiary Logic
measurement(m32, global_market_system, resistance(structural), 100, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(economic_adjustment_cycle, global_market_system, 'Degrowth_Based_Resource_Equilibrium').
intent_alternative_rejected(economic_adjustment_cycle, global_market_system, 'Degrowth_Based_Resource_Equilibrium').

intent_beneficiary_class(economic_adjustment_cycle, economic_council).
intent_power_change(economic_adjustment_cycle, economic_council, 0.80). % Centralization of regulatory oversight

intent_loser_class(economic_adjustment_cycle, labor_unions). % Reduced collective bargaining autonomy
intent_power_change(economic_adjustment_cycle, labor_unions, -0.35).

intent_suppression_level(economic_adjustment_cycle, economic_council, structural, 0.0).
intent_resistance_level(economic_adjustment_cycle, economic_council, structural, 0.0).

intent_norm_strength(economic_adjustment_cycle, 0, 0.70). % Emerging security priorities
intent_norm_strength(economic_adjustment_cycle, 100, 0.98). % Absolute standard for national fiscal health
