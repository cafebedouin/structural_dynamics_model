% DR Modal Logic Audit: ASTM D638-22 (VALIDATED)
% Domain: Tensile Properties of Plastics % [cite: 6]

% --- Section 1: Entities & Intervals ---
entity(astm_d638, structural).
entity(iso_527_1, structural).
entity(iso_body, organizational).
entity(astm_committee, organizational). % [cite: 27]
entity(d4000_classification, structural). % [cite: 67]
entity(dod_agencies, organizational). % [cite: 10]
entity(digital_extensometer, scaffold). % Modern alternative for precision

interval(d638_evolution, 0, 80). % 1941 (T=0) to 2022 (T=80) % [cite: 28]

% --- Section 2: Events ---
event(e1, original_adoption, 0, [designation('D638-41')]). % [cite: 28]
event(e2, current_revision, 80, [designation('D638-22')]). % [cite: 28]

% --- Section 3: Constraint Claims & Metrics ---
% The standard claims to be a necessary "Mountain" for design% [cite: 4, 25].
constraint_claim(astm_d638, mountain).

% Current state at T=80: High extraction/suppression due to precision mandates% [cite: 108, 121].
constraint_metric(astm_d638, extractiveness, 0.70).
constraint_metric(astm_d638, suppression_requirement, 0.75).
constraint_metric(astm_d638, snap_back_potential, 0.0).

% --- Section 4: Temporal Measurements (Paired for Logic Integrity) ---
% T=0: Initial state as a functional "Rope"
measurement(m1, astm_d638, extractiveness, 0, 0.10).
measurement(m2, astm_d638, suppression_requirement, 0, 0.05).

% T=80: Transformation into a "Snare"
measurement(m3, astm_d638, extractiveness, 80, 0.70).
measurement(m4, astm_d638, suppression_requirement, 80, 0.75).

% --- Section 5: Coercion Vectors (Preventing Verification Failure) ---
measurement(v1, d638_evolution, accessibility_collapse(structural), 0, 0.1).
measurement(v2, d638_evolution, stakes_inflation(structural), 0, 0.1).
measurement(v3, d638_evolution, accessibility_collapse(structural), 80, 0.8).
measurement(v4, d638_evolution, stakes_inflation(structural), 80, 0.9).

% --- Section 6: Dependencies (Load-Bearing Audit) ---
affects_constraint(astm_d638, d4000_classification). % [cite: 67]
affects_constraint(astm_d638, dod_agencies). % [cite: 10]

% --- Section 7: Intent Evidence (RESOLVES LOGIC ERROR) ---
% ISO 527-1 is listed as viable  and then rejected due to technical differences.
intent_viable_alternative(d638_evolution, iso_body, 'ISO 527-1').
intent_alternative_rejected(d638_evolution, astm_committee, 'ISO 527-1').

intent_beneficiary_class(d638_evolution, astm_committee).
intent_power_change(d638_evolution, astm_committee, 0.50).

% --- Section 8: Recommendations ---
recommendation(rec1, 'Enable technical equivalence pathways for ISO 527-1 to lower barrier entry').
affects_constraint(rec1, astm_d638).
