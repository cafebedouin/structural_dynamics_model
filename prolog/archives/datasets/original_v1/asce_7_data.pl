% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: ASCE 7-22
% Domain: Civil Engineering - Minimum Design Loads for Buildings
% ==========================================================

% --- 1. Entities & Intervals ---
entity(asce_committee, structural).
entity(building_code_officials, class).
entity(structural_engineers, class).
entity(aec_firms, organizational).
entity(individual_pe, individual).
entity(building_owner, individual).

interval(design_compliance_cycle, 0, 100).

% --- 2. Events ---
event(ev01_risk_cat_assignment, classification, 5, [actor(individual_pe), standard(asce_7_22), property(risk_category)]).
event(ev02_seismic_hazard_analysis, measurement, 30, [actor(individual_pe), method(mcer_ground_motion)]).
event(ev03_load_combination_sum, integration, 65, [actor(individual_pe), technique(lrfd_vs_asd)]).
event(ev04_permitting_approval, validation, 95, [actor(building_code_officials), result(certification)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The objective physical reality of seismic forces and gravity.
constraint_claim(seismic_physics_limit, mountain).
constraint_metric(seismic_physics_limit, accessibility_collapse, 0.95).

% Snare: The legal mandate of ASCE 7 compliance for liability and insurance.
constraint_claim(regulatory_life_safety_mandate, snare).
constraint_metric(regulatory_life_safety_mandate, stakes_inflation, 0.90).

% Piton: Legacy deterministic safety factors that persist despite reliability-based updates.
constraint_claim(legacy_safety_factor_bias, piton).
constraint_metric(legacy_safety_factor_bias, suppression, 0.70).

% Rope: Risk Category II standard. Ties average structures to specific rigid performance tiers.
constraint_claim(risk_category_standardization, rope).
constraint_metric(risk_category_standardization, suppression, 0.80).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt fully site-specific performance-based seismic design to bypass generic spectrum limits.').
recommendation(rec02, 'Automate load-path validation via BIM-integrated code-checking to reduce PE liability exposure.').

affects_constraint(rec01, risk_category_standardization).
affects_constraint(rec02, regulatory_life_safety_mandate).

veto_actor(asce_committee).
veto_actor(building_code_officials).

veto_exposed(asce_committee, rec01).
veto_exposed(building_code_officials, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial Site Assessment / Conceptual Design)
measurement(m01, individual_pe, accessibility_collapse(individual), 0, 0.25).
measurement(m02, individual_pe, stakes_inflation(individual), 0, 0.40).
measurement(m03, individual_pe, suppression(individual), 0, 0.15).
measurement(m04, individual_pe, resistance(individual), 0, 0.05).

measurement(m05, aec_firms, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, aec_firms, stakes_inflation(organizational), 0, 0.30).
measurement(m07, aec_firms, suppression(organizational), 0, 0.10).
measurement(m08, aec_firms, resistance(organizational), 0, 0.00).

measurement(m09, structural_engineers, accessibility_collapse(class), 0, 0.05).
measurement(m10, structural_engineers, stakes_inflation(class), 0, 0.20).
measurement(m11, structural_engineers, suppression(class), 0, 0.05).
measurement(m12, structural_engineers, resistance(class), 0, 0.95).

measurement(m13, asce_committee, accessibility_collapse(structural), 0, 0.00).
measurement(m14, asce_committee, stakes_inflation(structural), 0, 0.10).
measurement(m15, asce_committee, suppression(structural), 0, 0.00). % Beneficiary Logic
measurement(m16, asce_committee, resistance(structural), 0, 0.00). % Beneficiary Logic

% Time Tn (Final Permitting / Structural Certification)
measurement(m17, individual_pe, accessibility_collapse(individual), 100, 0.85). % Compliance locks choice
measurement(m18, individual_pe, stakes_inflation(individual), 100, 1.00). % Total professional liability
measurement(m19, individual_pe, suppression(individual), 100, 0.90). % Agency replaced by standard
measurement(m20, individual_pe, resistance(individual), 100, 0.00). % Submission to the code

measurement(m21, aec_firms, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, aec_firms, stakes_inflation(organizational), 100, 0.95).
measurement(m23, aec_firms, suppression(organizational), 100, 0.05).
measurement(m24, aec_firms, resistance(organizational), 100, 0.98).

measurement(m25, structural_engineers, accessibility_collapse(class), 100, 0.05).
measurement(m26, structural_engineers, stakes_inflation(class), 100, 0.90).
measurement(m27, structural_engineers, suppression(class), 100, 0.30).
measurement(m28, structural_engineers, resistance(class), 100, 0.70).

measurement(m29, asce_committee, accessibility_collapse(structural), 100, 0.00).
measurement(m30, asce_committee, stakes_inflation(structural), 100, 0.50).
measurement(m31, asce_committee, suppression(structural), 100, 0.00). % Beneficiary Logic
measurement(m32, asce_committee, resistance(structural), 100, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(design_compliance_cycle, structural_engineers, 'Generic_Probabilistic_Hazard_Agnosticism').
intent_alternative_rejected(design_compliance_cycle, structural_engineers, 'Generic_Probabilistic_Hazard_Agnosticism').

intent_beneficiary_class(design_compliance_cycle, asce_committee).
intent_power_change(design_compliance_cycle, asce_committee, 0.85). % Totalized institutional control

intent_loser_class(design_compliance_cycle, individual_pe). % Absolute liability with shrinking design autonomy
intent_power_change(design_compliance_cycle, individual_pe, -0.60).

intent_suppression_level(design_compliance_cycle, asce_committee, structural, 0.0).
intent_resistance_level(design_compliance_cycle, asce_committee, structural, 0.0).

intent_norm_strength(design_compliance_cycle, 0, 0.85). % Established code dominance
intent_norm_strength(design_compliance_cycle, 100, 1.00). % Absolute prerequisite for legal structure
