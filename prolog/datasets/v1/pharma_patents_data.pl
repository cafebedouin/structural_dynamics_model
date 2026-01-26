% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Pharmaceutical Patents (TRIPS)
% Domain: Global Intellectual Property - Access to Essential Medicines
% ==========================================================

% --- 1. Entities & Intervals ---
entity(individual_patient, individual).
entity(generic_manufacturer, organizational).
entity(world_trade_organization, organizational).
entity(big_pharma_conglomerates, class).
entity(global_south_nations, class).
entity(intellectual_property_regime, structural).

interval(trips_standardization_cycle, 1995, 2025).

% --- 2. Events ---
event(ev01_trips_enactment, legal_initiation, 1995, [statute(agreement_on_trips), actor(world_trade_organization)]).
event(ev02_doha_declaration, parameter_setting, 2001, [logic(public_health_flexibility), result(limited_relief)]).
event(ev03_compulsory_licensing_trigger, systemic_conflict, 2012, [actor(global_south_nations), result(litigation_pressure)]).
event(ev04_patent_evergreening, state_stabilization, 2025, [mechanism(minor_reformulation), effect(protection_extension)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute physical and economic cost of drug R&D and clinical trials.
constraint_claim(rd_capital_barrier, mountain).
constraint_metric(rd_capital_barrier, accessibility_collapse, 0.95).

% Snare: The 20-year patent term mandate. A legal trap that prevents generic entry regardless of health emergency.
constraint_claim(patent_protection_mandate, snare).
constraint_metric(patent_protection_mandate, stakes_inflation, 0.92).

% Piton: Innovation Incentive Doctrine. The idea that only high-price monopolies generate life-saving research, which persists despite increasing public funding of early-stage discovery.
constraint_claim(monopoly_incentive_myth, piton).
constraint_metric(monopoly_incentive_myth, suppression, 0.85).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt a \'Medicine as a Global Public Good\' framework to bypass commercial patent lock-in.').
recommendation(rec02, 'Implement automatic compulsory licensing triggers for WHO-listed essential medicines.').

affects_constraint(rec01, monopoly_incentive_myth).
affects_constraint(rec02, patent_protection_mandate).

veto_actor(big_pharma_conglomerates).
veto_actor(world_trade_organization).

veto_exposed(big_pharma_conglomerates, rec01).
veto_exposed(world_trade_organization, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (1995: Pre-TRIPS Implementation for many Developing Nations)
measurement(m01, individual_patient, accessibility_collapse(individual), 1995, 0.40).
measurement(m02, individual_patient, stakes_inflation(individual), 1995, 0.60).
measurement(m03, individual_patient, suppression(individual), 1995, 0.20).
measurement(m04, individual_patient, resistance(individual), 1995, 0.10).

measurement(m05, generic_manufacturer, accessibility_collapse(organizational), 1995, 0.20).
measurement(m06, generic_manufacturer, stakes_inflation(organizational), 1995, 0.30).
measurement(m07, generic_manufacturer, suppression(organizational), 1995, 0.10).
measurement(m08, generic_manufacturer, resistance(organizational), 1995, 0.80).

measurement(m09, global_south_nations, accessibility_collapse(class), 1995, 0.30).
measurement(m10, global_south_nations, stakes_inflation(class), 1995, 0.40).
measurement(m11, global_south_nations, suppression(class), 1995, 0.15).
measurement(m12, global_south_nations, resistance(class), 1995, 0.70).

measurement(m13, intellectual_property_regime, accessibility_collapse(structural), 1995, 0.05).
measurement(m14, intellectual_property_regime, stakes_inflation(structural), 1995, 0.10).
measurement(m15, intellectual_property_regime, suppression(structural), 1995, 0.00). % Beneficiary Logic
measurement(m16, intellectual_property_regime, resistance(structural), 1995, 0.00). % Beneficiary Logic

% Time Tn (2025: Maturity of Global IP Enforcement)
measurement(m17, individual_patient, accessibility_collapse(individual), 2025, 0.85). % Life-saving meds priced out
measurement(m18, individual_patient, stakes_inflation(individual), 2025, 1.00). % Mortality as the stake
measurement(m19, individual_patient, suppression(individual), 2025, 0.90). % Agency restricted by cost
measurement(m20, individual_patient, resistance(individual), 2025, 0.05).

measurement(m21, generic_manufacturer, accessibility_collapse(organizational), 2025, 0.90).
measurement(m22, generic_manufacturer, stakes_inflation(organizational), 2025, 0.95).
measurement(m23, generic_manufacturer, suppression(organizational), 2025, 0.85).
measurement(m24, generic_manufacturer, resistance(organizational), 2025, 0.10).

measurement(m25, global_south_nations, accessibility_collapse(class), 2025, 0.75).
measurement(m26, global_south_nations, stakes_inflation(class), 2025, 0.98).
measurement(m27, global_south_nations, suppression(class), 2025, 0.80).
measurement(m28, global_south_nations, resistance(class), 2025, 0.40).

measurement(m29, intellectual_property_regime, accessibility_collapse(structural), 2025, 0.00).
measurement(m30, intellectual_property_regime, stakes_inflation(structural), 2025, 0.50).
measurement(m31, intellectual_property_regime, suppression(structural), 2025, 0.00). % Beneficiary Logic
measurement(m32, intellectual_property_regime, resistance(structural), 2025, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(trips_standardization_cycle, intellectual_property_regime, 'Open_Source_R&D_Pooling').
intent_alternative_rejected(trips_standardization_cycle, intellectual_property_regime, 'Open_Source_R&D_Pooling').

intent_beneficiary_class(trips_standardization_cycle, big_pharma_conglomerates).
intent_power_change(trips_standardization_cycle, big_pharma_conglomerates, 0.85). 

intent_loser_class(trips_standardization_cycle, global_south_nations).
intent_power_change(trips_standardization_cycle, global_south_nations, -0.65). 

intent_suppression_level(trips_standardization_cycle, big_pharma_conglomerates, structural, 0.0).
intent_resistance_level(trips_standardization_cycle, big_pharma_conglomerates, structural, 0.0).

intent_norm_strength(trips_standardization_cycle, 1995, 0.35). 
intent_norm_strength(trips_standardization_cycle, 2025, 0.98).
