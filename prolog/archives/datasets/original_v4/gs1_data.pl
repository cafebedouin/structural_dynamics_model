% DATASET: GS1 General Specifications Standard (Release 25.0, Jan 25)
% AUDIT TYPE: DR Modal Logic + Structural Signature Detection
% SENSOR STATUS: Clinical / Extractive

% --- 1. CORE ONTOLOGY ---
entity(gs1_aisbl, organizational).
entity(gtin_standard, structural).
entity(gln_standard, structural).
entity(sscc_standard, structural).
entity(healthcare_sector, organizational).
entity(retail_pos, organizational).

% [cite_start] Intervals based on document history [cite: 116, 118]
interval(t_upc_origin, 1973, 1977).      % [cite_start] UPC Council origin [cite: 122]
interval(t_ean_integration, 1977, 2005). % [cite_start] EAN International establishment [cite: 122]
interval(t_gs1_launch, 2005, 2025).      % [cite_start] Launch of GS1 and modern system [cite: 122]
interval(t_release_25, 2025, 2026).      % [cite_start] Current ratified status [cite: 54]

% --- 2. CONSTRAINT CLAIMS ---
% Document claims these keys are foundational and mandatory for interoperability
constraint_claim(gtin_uniqueness, scaffold). % [cite_start] Claimed as "foundational standard" [cite: 54, 70]
constraint_claim(healthcare_compliance, mountain). % [cite_start] Mandatory regulatory requirement [cite: 244, 471]
constraint_claim(gln_non_reuse, rope). % Strict allocation rule

% --- 3. MEASUREMENT SCHEMA (Signature Detection) ---
% T=0 (1973): Initial U.P.C. Coordination
measurement(m01, gtin_standard, extractiveness, 1973, 0.4).
measurement(m02, gtin_standard, suppression_requirement, 1973, 0.2).
measurement(m03, gtin_standard, resistance, 1973, 0.1).

% T=5 (2005): Global GS1 Convergence (Transformation to Load-Bearing Scaffold)
measurement(m04, gtin_standard, extractiveness, 2005, 0.7).
measurement(m05, gtin_standard, suppression_requirement, 2005, 0.6).
measurement(m06, gtin_standard, resistance, 2005, 0.5).

% T=10 (2025): Release 25.0 (Constructed Constraint / Calcification)
measurement(m07, gtin_standard, extractiveness, 2025, 0.95).
measurement(m08, gtin_standard, suppression_requirement, 2025, 0.9).
measurement(m09, gtin_standard, resistance, 2025, 0.85).

% Current Kinetic Metrics at T_end
constraint_metric(gtin_standard, snap_back_potential, 0.9). % High due to global supply chain lock-in
constraint_metric(healthcare_compliance, extractiveness, 0.98).
constraint_metric(healthcare_compliance, resistance, 0.95). % Regulatory enforcement resistance

% --- 4. VIABLE ALTERNATIVES (For Scaffold Detection) ---
% Non-GS1 identifiers are permitted but cannot be primary for compliance
alternative(gtin_standard, internal_sku_logic). 
alternative(gtin_standard, national_trade_item_numbers). % [cite_start] NTINs [cite: 509]

% --- 5. DEPENDENCIES (Counterfactual Risk) ---
% [cite_start] If the Company Prefix is revoked, all issued keys become invalid [cite: 193]
affects_constraint(gs1_company_prefix, gtin_standard).
affects_constraint(gs1_company_prefix, gln_standard).
affects_constraint(gtin_standard, retail_pos). % [cite_start] Retail POS depends on GTIN scanability [cite: 244]

% --- 6. MODAL TRANSFORMATIONS ---
% Transformation: Coordination (1973) -> Global Scaffold (2005) -> Regulatory Mountain (2025)
event(ev_convergence, capture, 2005, [successor_to_ean_ucc]). % [cite_start] [cite: 122]
event(ev_healthcare_mandate, calcification, 2025, [udi_eudamed_compliance]). %

% --- 7. INTENT EVIDENCE (Capture Analysis) ---
% [cite_start] Licensing rules block secondary markets for identifiers [cite: 193]
intent_beneficiary(gs1_aisbl, control_of_uniqueness).
intent_mechanism(gtin_standard, capture, "Licence SHALL NOT be sold, leased or given"). % [cite_start] [cite: 193]

% --- 8. AUDIT SUMMARY (Prolog-derived logic) ---
% SIGNATURE DETECTED: coordination_scaffold -> constructed_constraint
% RISK: Catastrophic if gs1_company_prefix is compromised (Load-bearing)
% [cite_start] FRAUD DETECTION: Claims to be "open" [cite: 156] [cite_start]but prohibits alternative primary IDs [cite: 144] (Regulatory Capture signature).
