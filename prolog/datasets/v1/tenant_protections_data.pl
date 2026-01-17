% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Tenant Protections
% Domain: Regulatory Urban Housing - Rent Control and Eviction Protections
% ==========================================================

% --- 1. Entities & Intervals ---
entity(tenant_incumbent, individual).
entity(landlord_small_holder, individual).
entity(rent_stabilization_board, organizational).
entity(housing_justice_coalition, class).
entity(real_estate_investment_class, class).
entity(urban_tenure_system, structural).

interval(rent_regulation_cycle, 0, 100).

% --- 2. Events ---
event(ev01_legislative_enactment, legal_initiation, 5, [statute(rent_freeze), location(berlin_ny_sf)]).
event(ev02_just_cause_trigger, threshold_crossing, 35, [logic(permanent_tenancy_right), effect(market_liquidity_drop)]).
event(ev03_anti_retaliation_suit, systemic_conflict, 70, [actor(tenant_incumbent), target(landlord_small_holder)]).
event(ev04_structural_saturation, state_stabilization, 95, [result(housing_stock_stagnation)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute physical scarcity of buildable urban land and existing housing supply.
constraint_claim(land_supply_scarcity, mountain).
constraint_metric(land_supply_scarcity, accessibility_collapse, 0.95).

% Noose: Just-Cause Eviction Laws. A closing circle of criteria that restricts a landlord's exit from a contract.
constraint_claim(just_cause_mandate, noose).
constraint_metric(just_cause_mandate, stakes_inflation, 0.88).

% Zombie: Fair Market Rent. A legacy economic concept that persists as a benchmark despite fixed price ceilings.
constraint_claim(market_value_phantom, zombie).
constraint_metric(market_value_phantom, suppression, 0.75).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt means-tested housing vouchers to replace universal price controls.').
recommendation(rec02, 'Implement streamlined arbitration for small-holder owners to bypass court backlogs.').

affects_constraint(rec01, market_value_phantom).
affects_constraint(rec02, just_cause_mandate).

veto_actor(housing_justice_coalition).
veto_actor(rent_stabilization_board).

veto_exposed(housing_justice_coalition, rec01).
veto_exposed(rent_stabilization_board, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial Enactment / Pre-Stabilization)
measurement(m01, tenant_incumbent, accessibility_collapse(individual), 0, 0.70).
measurement(m02, tenant_incumbent, stakes_inflation(individual), 0, 0.85).
measurement(m03, tenant_incumbent, suppression(individual), 0, 0.60).
measurement(m04, tenant_incumbent, resistance(individual), 0, 0.40).

measurement(m05, rent_stabilization_board, accessibility_collapse(organizational), 0, 0.15).
measurement(m06, rent_stabilization_board, stakes_inflation(organizational), 0, 0.25).
measurement(m07, rent_stabilization_board, suppression(organizational), 0, 0.10).
measurement(m08, rent_stabilization_board, resistance(organizational), 0, 0.05).

measurement(m09, housing_justice_coalition, accessibility_collapse(class), 0, 0.20).
measurement(m10, housing_justice_coalition, stakes_inflation(class), 0, 0.40).
measurement(m11, housing_justice_coalition, suppression(class), 0, 0.30).
measurement(m12, housing_justice_coalition, resistance(class), 0, 0.80).

measurement(m13, urban_tenure_system, accessibility_collapse(structural), 0, 0.05).
measurement(m14, urban_tenure_system, stakes_inflation(structural), 0, 0.10).
measurement(m15, urban_tenure_system, suppression(structural), 0, 0.00). % Beneficiary Logic
measurement(m16, urban_tenure_system, resistance(structural), 0, 0.00). % Beneficiary Logic

% Time Tn (Maturity / Long-term Regulatory State)
measurement(m17, tenant_incumbent, accessibility_collapse(individual), 100, 0.10). % Tenure locked
measurement(m18, tenant_incumbent, stakes_inflation(individual), 100, 0.95). % Dependency on regulation
measurement(m19, tenant_incumbent, suppression(individual), 100, 0.05). % Agency high within law
measurement(m20, tenant_incumbent, resistance(individual), 100, 0.98). % Defensive power

measurement(m21, rent_stabilization_board, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, rent_stabilization_board, stakes_inflation(organizational), 100, 0.90).
measurement(m23, rent_stabilization_board, suppression(organizational), 100, 0.05).
measurement(m24, rent_stabilization_board, resistance(organizational), 100, 0.95).

measurement(m25, housing_justice_coalition, accessibility_collapse(class), 100, 0.05).
measurement(m26, housing_justice_coalition, stakes_inflation(class), 100, 0.95).
measurement(m27, housing_justice_coalition, suppression(class), 100, 0.00). % Total victory
measurement(m28, housing_justice_coalition, resistance(class), 100, 0.95).

measurement(m29, urban_tenure_system, accessibility_collapse(structural), 100, 0.00).
measurement(m30, urban_tenure_system, stakes_inflation(structural), 100, 0.45).
measurement(m31, urban_tenure_system, suppression(structural), 100, 0.00). % Beneficiary Logic
measurement(m32, urban_tenure_system, resistance(structural), 100, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(rent_regulation_cycle, urban_tenure_system, 'Means_Tested_Direct_Subsidy_Vouchers').
intent_alternative_rejected(rent_regulation_cycle, urban_tenure_system, 'Means_Tested_Direct_Subsidy_Vouchers').

intent_beneficiary_class(rent_regulation_cycle, housing_justice_coalition).
intent_power_change(rent_regulation_cycle, housing_justice_coalition, 0.65). % Massive structural gain in tenure security

intent_loser_class(rent_regulation_cycle, real_estate_investment_class).
intent_power_change(rent_regulation_cycle, real_estate_investment_class, -0.70). % Loss of asset liquidity and pricing power

intent_suppression_level(rent_regulation_cycle, housing_justice_coalition, structural, 0.0).
intent_resistance_level(rent_regulation_cycle, housing_justice_coalition, structural, 0.0).

intent_norm_strength(rent_regulation_cycle, 0, 0.40). % Law is emerging
intent_norm_strength(rent_regulation_cycle, 100, 0.98). % Regulation is absolute social standard
