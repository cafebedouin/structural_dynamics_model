% ==========================================
% GS1 BARCODES DATA - CORRECTED v3.2.1
% ==========================================

% --- Section 1: Entities & Intervals ---
% Explicitly declare all measurement targets as entities
entity(gs1_system, organizational).
entity(ucc_council, organizational).
entity(ean_intl, organizational).
entity(gtin_key, structural).
entity(mandatory_id_policy, structural).

% NEW: Added missing declarations to satisfy Step 2 Validator
entity(c_unambiguous, structural).
entity(c_mandatory_exclusive, structural).
entity(c_licensing, structural).

interval(main_scope, 1973, 2025).
interval(i_origin, 1973, 1977).
interval(i_expansion, 1977, 2005).
interval(i_consolidation, 2005, 2025).

% --- Section 2: Events ---
event(e1, discovery, 1973, [name='UCC_Launch']).
event(e2, discovery, 1977, [name='EAN_Launch']).
event(e3, capture, 2005, [name='GS1_Consolidation']).

% --- Section 3: Constraint Claims & Metrics ---
constraint_claim(c_unambiguous, rope).
constraint_metric(c_unambiguous, extractiveness, 0.85).
constraint_metric(c_unambiguous, suppression_requirement, 0.80).
constraint_metric(c_unambiguous, snap_back_potential, 0.40).

constraint_claim(c_mandatory_exclusive, mountain).
constraint_metric(c_mandatory_exclusive, extractiveness, 0.90).
constraint_metric(c_mandatory_exclusive, suppression_requirement, 0.95).
constraint_metric(c_mandatory_exclusive, snap_back_potential, 0.98).

constraint_claim(c_licensing, rope).
constraint_metric(c_licensing, extractiveness, 0.70).
constraint_metric(c_licensing, suppression_requirement, 0.75).

% --- Section 4: Temporal Measurements (PAIRED) ---
measurement(m1_x, c_unambiguous, extractiveness, 1973, 0.10).
measurement(m1_e, c_unambiguous, suppression_requirement, 1973, 0.05).
measurement(m2_x, c_unambiguous, extractiveness, 2005, 0.45).
measurement(m2_e, c_unambiguous, suppression_requirement, 2005, 0.40).
measurement(m3_x, c_unambiguous, extractiveness, 2025, 0.85).
measurement(m3_e, c_unambiguous, suppression_requirement, 2025, 0.80).

measurement(m4_x, c_mandatory_exclusive, extractiveness, 1973, 0.05).
measurement(m4_e, c_mandatory_exclusive, suppression_requirement, 1973, 0.20).
measurement(m5_x, c_mandatory_exclusive, extractiveness, 2005, 0.60).
measurement(m5_e, c_mandatory_exclusive, suppression_requirement, 2005, 0.75).
measurement(m6_x, c_mandatory_exclusive, extractiveness, 2025, 0.90).
measurement(m6_e, c_mandatory_exclusive, suppression_requirement, 2025, 0.95).

% --- Section 5: Dependencies ---
affects_constraint(c_mandatory_exclusive, gtin_key).
affects_constraint(gs1_system, c_licensing).

% --- Section 6: Intent Evidence ---
intent_beneficiary_class(i_consolidation, gs1_system).
intent_power_change(i_consolidation, gs1_system, 0.85).

% --- Section 7: Recommendations ---
recommendation(rec_1, 'Introduce decentralized GTIN scaffold to mitigate capture').
affects_constraint(rec_1, c_mandatory_exclusive).
