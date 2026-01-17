% DR Modal Logic Dataset: The Code of Hammurabi (Repaired)
% Version: 3.1.3
% Focus: Military Benefice and Judicial Evidence Evolution

% --- SECTION 1: ENTITIES & INTERVALS ---
entity(king_hammurabi, individual).
entity(palace_admin, organizational).
entity(ganger_class, class).
entity(benefice_system, structural).
entity(river_ordeal, structural).
entity(professional_army_scaffold, scaffold). % The required transition tool

interval(hammurabi_epoch, 0, 42).

% --- SECTION 2: CONSTRAINT CLAIMS & METRICS (T=42) ---
% These metrics align with the Noose (⊠C) classification (X >= 0.66, E >= 0.66)
constraint_claim(benefice_system, mountain). % Claimed as natural necessity
constraint_metric(benefice_system, extractiveness, 0.85).
constraint_metric(benefice_system, suppression_requirement, 0.88).

constraint_claim(river_ordeal, mountain). % Claimed as divine necessity
constraint_metric(river_ordeal, extractiveness, 0.92).
constraint_metric(river_ordeal, suppression_requirement, 0.95).

% --- SECTION 3: TEMPORAL MEASUREMENTS (PAIRED METRICS) ---
% Benefice System Evolution: Rope (T=0) -> Noose (T=42)
measurement(m1, benefice_system, extractiveness, 0, 0.25).
measurement(m2, benefice_system, suppression_requirement, 0, 0.20).

measurement(m3, benefice_system, extractiveness, 20, 0.55).
measurement(m4, benefice_system, suppression_requirement, 20, 0.50).

measurement(m5, benefice_system, extractiveness, 42, 0.85).
measurement(m6, benefice_system, suppression_requirement, 42, 0.88).

% River Ordeal Evolution: Static High-Suppression Noose
measurement(m7, river_ordeal, extractiveness, 0, 0.90).
measurement(m8, river_ordeal, suppression_requirement, 0, 0.92).
measurement(m9, river_ordeal, extractiveness, 42, 0.92).
measurement(m10, river_ordeal, suppression_requirement, 42, 0.95).

% --- SECTION 4: DEPENDENCIES ---
affects_constraint(benefice_system, military_readiness).
affects_constraint(river_ordeal, judicial_certainty).

% --- SECTION 5: INTENT EVIDENCE (FIXED LOGIC ERRORS) ---
intent_beneficiary_class(hammurabi_epoch, palace_admin).
intent_power_change(hammurabi_epoch, palace_admin, 0.75).

% FIXED: Alternative is now explicitly listed as viable AND rejected
intent_viable_alternative(hammurabi_epoch, king_hammurabi, 'Professional Salary Model').
intent_alternative_rejected(hammurabi_epoch, king_hammurabi, 'Professional Salary Model').

% --- SECTION 6: RECOMMENDATIONS ---
recommendation(rec1, 'Deploy professional_army_scaffold to phase out Benefice System').
affects_constraint(rec1, benefice_system).

recommendation(rec2, 'Shift to written witness bonds (§122) as the primary evidentiary standard').
affects_constraint(rec2, river_ordeal).

