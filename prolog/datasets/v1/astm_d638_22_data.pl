

% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: ASTM D638-22
% Domain: Materials Engineering - Tensile Properties of Plastics
% ==========================================================

% --- 1. Entities & Intervals ---
entity(astm_international, organizational).
entity(testing_laboratory, organizational).
entity(quality_assurance_engineer, individual).
entity(plastics_manufacturing_sector, class).
entity(regulatory_compliance_framework, structural).
entity(material_specimen, individual).

interval(tensile_test_procedure, 0, 100). % From setup (T0) to rupture (Tn)

% --- 2. Events ---
event(ev01_specimen_prep, measurement, 5, [standard(d638), type(dogbone), condition('23_deg_c')]).
event(ev02_grip_engagement, setup, 15, [alignment(vertical), slippage_check(required)]).
event(ev03_loading_sequence, operation, 40, [speed('5_mm_min'), monitoring(extensometer)]).
event(ev04_material_rupture, finality, 95, [result(tensile_strength), result(elongation)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute physical behavior of polymer molecular chains under stress.
constraint_claim(material_molecular_limits, mountain).
constraint_metric(material_molecular_limits, accessibility_collapse, 1.0).

% Noose: The 'Standard Requirement' of D638. It tightens as the industry demands interchangeable data.
constraint_claim(procedural_compliance_mandate, noose).
constraint_metric(procedural_compliance_mandate, stakes_inflation, 0.95).

% Zombie: The 'Nominal Strain.' A linearized calculation often treated as real despite localized necking.
constraint_claim(linearized_strain_abstraction, zombie).
constraint_metric(linearized_strain_abstraction, suppression, 0.80).

% Rope: Condition 23/50 (Standard Atmosphere). Ties all valid testing to specific ambient constraints.
constraint_claim(environmental_standardization, rope).
constraint_metric(environmental_standardization, suppression, 0.75).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt True Stress-Strain monitoring to bypass the errors of linearized strain abstractions.').
recommendation(rec02, 'Allow for variable speed testing in non-rigid plastics to reflect actual service conditions.').

affects_constraint(rec01, linearized_strain_abstraction).
affects_constraint(rec02, procedural_compliance_mandate).

veto_actor(astm_international).
veto_actor(regulatory_compliance_framework).

veto_exposed(astm_international, rec01).
veto_exposed(regulatory_compliance_framework, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Pre-Test / Standard Setup)
measurement(m01, quality_assurance_engineer, accessibility_collapse(individual), 0, 0.20).
measurement(m02, quality_assurance_engineer, stakes_inflation(individual), 0, 0.60).
measurement(m03, quality_assurance_engineer, suppression(individual), 0, 0.85).
measurement(m04, quality_assurance_engineer, resistance(individual), 0, 0.05).

measurement(m05, testing_laboratory, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, testing_laboratory, stakes_inflation(organizational), 0, 0.40).
measurement(m07, testing_laboratory, suppression(organizational), 0, 0.30).
measurement(m08, testing_laboratory, resistance(organizational), 0, 0.10).

measurement(m09, plastics_manufacturing_sector, accessibility_collapse(class), 0, 0.05).
measurement(m10, plastics_manufacturing_sector, stakes_inflation(class), 0, 0.30).
measurement(m11, plastics_manufacturing_sector, suppression(class), 0, 0.15).
measurement(m12, plastics_manufacturing_sector, resistance(class), 0, 0.90).

measurement(m13, regulatory_compliance_framework, accessibility_collapse(structural), 0, 0.00).
measurement(m14, regulatory_compliance_framework, stakes_inflation(structural), 0, 0.10).
measurement(m15, regulatory_compliance_framework, suppression(structural), 0, 0.00). % Beneficiary Rule
measurement(m16, regulatory_compliance_framework, resistance(structural), 0, 0.00). % Beneficiary Rule

% Time Tn (Material Rupture / Data Generation)
measurement(m17, quality_assurance_engineer, accessibility_collapse(individual), 100, 0.05).
measurement(m18, quality_assurance_engineer, stakes_inflation(individual), 100, 0.98).
measurement(m19, quality_assurance_engineer, suppression(individual), 100, 0.10).
measurement(m20, quality_assurance_engineer, resistance(individual), 100, 0.95).

measurement(m21, testing_laboratory, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, testing_laboratory, stakes_inflation(organizational), 100, 0.90).
measurement(m23, testing_laboratory, suppression(organizational), 100, 0.05).
measurement(m24, testing_laboratory, resistance(organizational), 100, 0.98).

measurement(m25, plastics_manufacturing_sector, accessibility_collapse(class), 100, 0.05).
measurement(m26, plastics_manufacturing_sector, stakes_inflation(class), 100, 0.95).
measurement(m27, plastics_manufacturing_sector, suppression(class), 100, 0.20).
measurement(m28, plastics_manufacturing_sector, resistance(class), 100, 0.85).

measurement(m29, regulatory_compliance_framework, accessibility_collapse(structural), 100, 0.00).
measurement(m30, regulatory_compliance_framework, stakes_inflation(structural), 100, 0.50).
measurement(m31, regulatory_compliance_framework, suppression(structural), 100, 0.00). % Beneficiary Rule
measurement(m32, regulatory_compliance_framework, resistance(structural), 100, 0.00). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(tensile_test_procedure, regulatory_compliance_framework, 'In-Situ_Ultrasonic_Stress_Mapping').
intent_alternative_rejected(tensile_test_procedure, regulatory_compliance_framework, 'In-Situ_Ultrasonic_Stress_Mapping').

intent_beneficiary_class(tensile_test_procedure, astm_international).
intent_power_change(tensile_test_procedure, astm_international, 0.90). 

intent_loser_class(tensile_test_procedure, plastics_manufacturing_sector). 
intent_power_change(tensile_test_procedure, plastics_manufacturing_sector, -0.25).

intent_suppression_level(tensile_test_procedure, astm_international, structural, 0.0).
intent_resistance_level(tensile_test_procedure, astm_international, structural, 0.0).

intent_norm_strength(tensile_test_procedure, 0, 0.90). 
intent_norm_strength(tensile_test_procedure, 100, 1.0).
