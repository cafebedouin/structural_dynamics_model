% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: End of USD Reserve Status
% Domain: Macroeconomics - Transition from Unipolar to Multipolar Monetary System
% ==========================================================

% --- 1. Entities & Intervals ---
entity(retail_investor, individual).
entity(us_fed, organizational).
entity(american_middle_class, class).
entity(brics_bloc, class).
entity(global_monetary_system, structural).

interval(dollar_hegemony_collapse, 0, 100). % From Hegemony (T0) to Multipolar Equilibrium (Tn)

% --- 2. Events ---
event(ev01_sanction_weaponization, theoretical_initiation, 10, [actor(us_state_dept), target(foreign_central_bank_reserves)]).
event(ev02_petrodollar_erosion, market_shift, 45, [commodity(energy_exports), bypass(usd_settlement)]).
event(ev03_cbdc_clearing_launch, technical_transformation, 80, [system(mbridge_interoperability), effect(disintermediation)]).
event(ev04_sovereign_debt_spiral, systemic_overhaul, 98, [subject(us_treasury_liquidity), result(risk_free_asset_repricing)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The Triffin Dilemma. The mathematical impossibility of a single national currency serving as a global reserve without permanent trade deficits.
constraint_claim(triffin_dilemma_logic, mountain).
constraint_metric(triffin_dilemma_logic, accessibility_collapse, 0.96).

% Snare: The SWIFT System. Geopolitical gatekeeping that narrows a state's options until financial isolation becomes total.
constraint_claim(swift_sanctions_noose, snare).
constraint_metric(swift_sanctions_noose, stakes_inflation, 0.90).

% Piton: Bretton Woods Institutionalism. The legacy rules of IMF/World Bank that persist as structural shells despite shifting power centers.
constraint_claim(bretton_woods_residual, piton).
constraint_metric(bretton_woods_residual, suppression, 0.85).

% Rope: Treasury Risk-Free Asset Peg. Ties global portfolios to a specific debt instrument used for coercive fiscal leverage.
constraint_claim(treasury_risk_free_myth, rope).
constraint_metric(treasury_risk_free_myth, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt a decentralized commodity-backed clearing unit (Bancor 2.0) to bypass national inflation export.').
recommendation(rec02, 'Eliminate unilateral veto power over global payment settlement infrastructures.').

affects_constraint(rec01, triffin_dilemma_logic).
affects_constraint(rec02, swift_sanctions_noose).

veto_actor(us_fed).
veto_exposed(us_fed, rec01).
veto_exposed(us_fed, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Stability / Unipolar Peak)
measurement(m01, retail_investor, accessibility_collapse(individual), 0, 0.10).
measurement(m02, retail_investor, stakes_inflation(individual), 0, 0.15).
measurement(m03, retail_investor, suppression(individual), 0, 0.05).
measurement(m04, retail_investor, resistance(individual), 0, 0.05).

measurement(m05, us_fed, accessibility_collapse(organizational), 0, 0.05).
measurement(m06, us_fed, stakes_inflation(organizational), 0, 0.10).
measurement(m07, us_fed, suppression(organizational), 0, 0.02).
measurement(m08, us_fed, resistance(organizational), 0, 0.02).

measurement(m09, american_middle_class, accessibility_collapse(class), 0, 0.12).
measurement(m10, american_middle_class, stakes_inflation(class), 0, 0.20).
measurement(m11, american_middle_class, suppression(class), 0, 0.10).
measurement(m12, american_middle_class, resistance(class), 0, 0.08).

measurement(m13, global_monetary_system, accessibility_collapse(structural), 0, 0.00).
measurement(m14, global_monetary_system, stakes_inflation(structural), 0, 0.05).
measurement(m15, global_monetary_system, suppression(structural), 0, 0.00). % Beneficiary logic
measurement(m16, global_monetary_system, resistance(structural), 0, 0.00). % Beneficiary logic

% Time Tn (Post-Transition / Multi-polar Shift)
measurement(m17, retail_investor, accessibility_collapse(individual), 100, 0.92). % Loss of purchasing power autonomy
measurement(m18, retail_investor, stakes_inflation(individual), 100, 0.98). % Survival risk in hyperinflationary state
measurement(m19, retail_investor, suppression(individual), 100, 0.88). % Options restricted by capital controls
measurement(m20, retail_investor, resistance(individual), 100, 0.75). % Flight to alternative assets

measurement(m21, us_fed, accessibility_collapse(organizational), 100, 0.95).
measurement(m22, us_fed, stakes_inflation(organizational), 100, 1.00).
measurement(m23, us_fed, suppression(organizational), 100, 0.92).
measurement(m24, us_fed, resistance(organizational), 100, 0.80).

measurement(m25, american_middle_class, accessibility_collapse(class), 100, 0.94).
measurement(m26, american_middle_class, stakes_inflation(class), 100, 1.00). % Total liquidation of retirement/savings value
measurement(m27, american_middle_class, suppression(class), 100, 0.95).
measurement(m28, american_middle_class, resistance(class), 100, 0.85).

measurement(m29, global_monetary_system, accessibility_collapse(structural), 100, 0.00).
measurement(m30, global_monetary_system, stakes_inflation(structural), 100, 0.45).
measurement(m31, global_monetary_system, suppression(structural), 100, 0.00). % New Beneficiary Logic
measurement(m32, global_monetary_system, resistance(structural), 100, 0.00). % New Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(dollar_hegemony_collapse, global_monetary_system, 'Gold_Standard_Reintegration').
intent_alternative_rejected(dollar_hegemony_collapse, global_monetary_system, 'Gold_Standard_Reintegration').

intent_beneficiary_class(dollar_hegemony_collapse, brics_bloc).
intent_power_change(dollar_hegemony_collapse, brics_bloc, 0.85). % Massive structural gain in trade autonomy

intent_loser_class(dollar_hegemony_collapse, american_middle_class).
intent_power_change(dollar_hegemony_collapse, american_middle_class, -0.90). % Total loss of global seigniorage advantage

intent_suppression_level(dollar_hegemony_collapse, brics_bloc, structural, 0.0).
intent_resistance_level(dollar_hegemony_collapse, brics_bloc, structural, 0.0).

intent_norm_strength(dollar_hegemony_collapse, 0, 0.95). % Dollar as unquestioned global unit
intent_norm_strength(dollar_hegemony_collapse, 100, 0.15). % Dollar as regional/contested unit
