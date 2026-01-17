% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: GS1 Barcode Standards
% Domain: Global Supply Chain Logistics and Identification Standards
% ==========================================================

% --- 1. Entities & Intervals ---
entity(gs1_global, organizational).
entity(local_manufacturer, individual).
entity(retail_conglomerate, class).
entity(consumer_base, class).
entity(supply_chain_infrastructure, structural).
entity(barcode_scanner_hardware, individual).

interval(gs1_standardization_cycle, 0, 100).

% --- 2. Events ---
event(ev01_prefix_allocation, assignment, 5, [actor(gs1_global), target(local_manufacturer), type(gcp)]).
event(ev02_barcode_generation, implementation, 20, [standard(ean_13), technology(linear_symbology)]).
event(ev03_pos_integration, systemic_capture, 65, [actor(retail_conglomerate), requirement(gtin_validation)]).
event(ev04_data_matrix_pivot, tech_shift, 95, [new_standard(gs1_2d), data_capacity(high)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The physical limits of optical scanning and symbol contrast/resolution.
constraint_claim(optical_physics_limits, mountain).
constraint_metric(optical_physics_limits, accessibility_collapse, 0.95).

% Noose: The mandatory licensing and annual fees for GCP (GS1 Company Prefixes).
constraint_claim(licensing_dependency, noose).
constraint_metric(licensing_dependency, stakes_inflation, 0.88).

% Zombie: Proprietary internal SKU systems that prevent cross-organizational interoperability.
constraint_claim(proprietary_silo_legacy, zombie).
constraint_metric(proprietary_silo_legacy, suppression, 0.75).

% Rope: The technical specification of the 'Quiet Zone' surrounding barcodes.
constraint_claim(quiet_zone_specification, rope).
constraint_metric(quiet_zone_specification, suppression, 0.60).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt open-source identifier alternatives to bypass standard licensing fees.').
recommendation(rec02, 'Implement universal hardware-agnostic 2D imaging to overcome linear scanning limits.').

affects_constraint(rec01, licensing_dependency).
affects_constraint(rec02, optical_physics_limits).

veto_actor(gs1_global).
veto_actor(retail_conglomerate).

veto_exposed(gs1_global, rec01).
veto_exposed(retail_conglomerate, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial Adoption Phase)
measurement(m01, local_manufacturer, accessibility_collapse(individual), 0, 0.30).
measurement(m02, local_manufacturer, stakes_inflation(individual), 0, 0.20).
measurement(m03, local_manufacturer, suppression(individual), 0, 0.15).
measurement(m04, local_manufacturer, resistance(individual), 0, 0.80).

measurement(m05, gs1_global, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, gs1_global, stakes_inflation(organizational), 0, 0.40).
measurement(m07, gs1_global, suppression(organizational), 0, 0.05).
measurement(m08, gs1_global, resistance(organizational), 0, 0.05).

measurement(m09, retail_conglomerate, accessibility_collapse(class), 0, 0.25).
measurement(m10, retail_conglomerate, stakes_inflation(class), 0, 0.35).
measurement(m11, retail_conglomerate, suppression(class), 0, 0.20).
measurement(m12, retail_conglomerate, resistance(class), 0, 0.10).

measurement(m13, supply_chain_infrastructure, accessibility_collapse(structural), 0, 0.10).
measurement(m14, supply_chain_infrastructure, stakes_inflation(structural), 0, 0.15).
measurement(m15, supply_chain_infrastructure, suppression(structural), 0, 0.00). % Beneficiary Logic
measurement(m16, supply_chain_infrastructure, resistance(structural), 0, 0.00). % Beneficiary Logic

% Time Tn (Standardization Maturity)
measurement(m17, local_manufacturer, accessibility_collapse(individual), 100, 0.90). % Compliance is mandatory for shelf space
measurement(m18, local_manufacturer, stakes_inflation(individual), 100, 0.95). % Total business risk on de-listing
measurement(m19, local_manufacturer, suppression(individual), 100, 0.90). % Individual agency replaced by spec
measurement(m20, local_manufacturer, resistance(individual), 100, 0.05). % Resistance is futile for market access

measurement(m21, gs1_global, accessibility_collapse(organizational), 100, 0.00).
measurement(m22, gs1_global, stakes_inflation(organizational), 100, 0.90).
measurement(m23, gs1_global, suppression(organizational), 100, 0.05).
measurement(m24, gs1_global, resistance(organizational), 100, 0.95).

measurement(m25, retail_conglomerate, accessibility_collapse(class), 100, 0.05).
measurement(m26, retail_conglomerate, stakes_inflation(class), 100, 0.98).
measurement(m27, retail_conglomerate, suppression(class), 100, 0.05).
measurement(m28, retail_conglomerate, resistance(class), 100, 0.95).

measurement(m29, supply_chain_infrastructure, accessibility_collapse(structural), 100, 0.00).
measurement(m30, supply_chain_infrastructure, stakes_inflation(structural), 100, 0.40).
measurement(m31, supply_chain_infrastructure, suppression(structural), 100, 0.00). % Beneficiary Logic
measurement(m32, supply_chain_infrastructure, resistance(structural), 100, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(gs1_standardization_cycle, supply_chain_infrastructure, 'Decentralized_Open_Identifier_Protocol').
intent_alternative_rejected(gs1_standardization_cycle, supply_chain_infrastructure, 'Decentralized_Open_Identifier_Protocol').

intent_beneficiary_class(gs1_standardization_cycle, gs1_global).
intent_power_change(gs1_standardization_cycle, gs1_global, 0.95). % Absolute identifier monopoly

intent_loser_class(gs1_standardization_cycle, local_manufacturer).
intent_power_change(gs1_standardization_cycle, local_manufacturer, -0.45). % Loss of autonomous pricing/ID power

intent_suppression_level(gs1_standardization_cycle, gs1_global, structural, 0.0).
intent_resistance_level(gs1_standardization_cycle, gs1_global, structural, 0.0).

intent_norm_strength(gs1_standardization_cycle, 0, 0.40). % Emerging norm
intent_norm_strength(gs1_standardization_cycle, 100, 1.0). % Totalized global standard
