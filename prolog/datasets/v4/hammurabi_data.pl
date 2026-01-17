% =============================================================================
% DR MODAL LOGIC + STRUCTURAL SIGNATURES AUDIT: CODE OF HAMMURABI
% Version: 3.2
% Date: 2026-01-16
% Subject: The Code of Laws Promulgated by Hammurabi, King of Babylon
% =============================================================================

% -----------------------------------------------------------------------------
% SECTION 1: ENTITIES & INTERVALS
% -----------------------------------------------------------------------------
entity(hammurabi_king, individual).
entity(gentleman_class, class).
entity(poor_man_class, class).
entity(slave_class, class).
entity(temple_palace_institution, organizational).
entity(code_of_hammurabi, structural).
entity(lex_talionis_protocol, structural).
entity(marriage_bond_standard, scaffold).
entity(river_ordeal_protocol, structural).

% Interval representing the duration of the Code's established influence
interval(babylonian_reign, 0, 43).

% -----------------------------------------------------------------------------
% SECTION 2: CONSTRAINT CLAIMS & CURRENT METRICS (T=43)
% -----------------------------------------------------------------------------
% The Code claims to be "the judgments of righteousness" (Mountain)
constraint_claim(code_of_hammurabi, mountain).
constraint_metric(code_of_hammurabi, extractiveness, 0.88).
constraint_metric(code_of_hammurabi, suppression_requirement, 0.92).
constraint_metric(code_of_hammurabi, resistance, 0.75).

% Lex Talionis (Eye for an Eye) claims to be natural justice (Mountain)
constraint_claim(lex_talionis_protocol, mountain).
constraint_metric(lex_talionis_protocol, extractiveness, 0.45).
constraint_metric(lex_talionis_protocol, suppression_requirement, 0.35).
constraint_metric(lex_talionis_protocol, resistance, 0.40).

% Marriage bonds (§128) function as a coordination scaffold
constraint_claim(marriage_bond_standard, scaffold).
constraint_metric(marriage_bond_standard, extractiveness, 0.25).
constraint_metric(marriage_bond_standard, suppression_requirement, 0.15).
constraint_metric(marriage_bond_standard, resistance, 0.10).

% -----------------------------------------------------------------------------
% SECTION 3: TEMPORAL MEASUREMENTS (EVOLUTION ANALYSIS)
% -----------------------------------------------------------------------------

% --- Evolution of the Code (Constructed Constraint Signature) ---
% T=0: Pre-codification (Judicial decisions of past ages)
measurement(m1, code_of_hammurabi, extractiveness, 0, 0.30).
measurement(m2, code_of_hammurabi, suppression_requirement, 0, 0.25).
measurement(m3, code_of_hammurabi, resistance, 0, 0.20).

% T=20: Mid-reign expansion
measurement(m4, code_of_hammurabi, extractiveness, 20, 0.65).
measurement(m5, code_of_hammurabi, suppression_requirement, 20, 0.70).
measurement(m6, code_of_hammurabi, resistance, 20, 0.55).

% T=43: Full institutionalization (Noose Transformation)
measurement(m7, code_of_hammurabi, extractiveness, 43, 0.88).
measurement(m8, code_of_hammurabi, suppression_requirement, 43, 0.92).
measurement(m9, code_of_hammurabi, resistance, 43, 0.75).

% --- Evolution of Marriage Bonds (Coordination Scaffold Signature) ---
% Shows high stability and low enforcement need
measurement(m10, marriage_bond_standard, extractiveness, 0, 0.20).
measurement(m11, marriage_bond_standard, suppression_requirement, 0, 0.10).
measurement(m12, marriage_bond_standard, resistance, 0, 0.05).

measurement(m13, marriage_bond_standard, extractiveness, 43, 0.25).
measurement(m14, marriage_bond_standard, suppression_requirement, 43, 0.15).
measurement(m15, marriage_bond_standard, resistance, 43, 0.10).

% -----------------------------------------------------------------------------
% SECTION 4: VIABLE ALTERNATIVES (SIGNATURE DIFFERENTIATION)
% -----------------------------------------------------------------------------

% Coordination Scaffold: Marriage Bond had alternatives (informal cohabitation)
intent_viable_alternative(babylonian_reign, marriage_bond_standard, 'Informal/unwritten cohabitation').
intent_alternative_rejected(babylonian_reign, marriage_bond_standard, 'Informal/unwritten cohabitation').

% Constructed Constraint: The Code displaced alternative tribal/local systems
intent_viable_alternative(babylonian_reign, code_of_hammurabi, 'Local tribal custom and uncodified arbitration').
intent_alternative_rejected(babylonian_reign, code_of_hammurabi, 'Local tribal custom and uncodified arbitration').

% Note: river_ordeal_protocol has no alternatives listed, mimicking its 
% claim to be a Natural Law (Signatures 1 candidate), despite metrics.

% -----------------------------------------------------------------------------
% SECTION 5: DEPENDENCIES & INTENT EVIDENCE
% -----------------------------------------------------------------------------

% Load-bearing dependencies
affects_constraint(code_of_hammurabi, temple_palace_institution).
affects_constraint(marriage_bond_standard, gentleman_class).

% Asymmetric outcomes (Evidence of Constructed Constraint)
intent_beneficiary_class(babylonian_reign, hammurabi_king).
intent_power_change(babylonian_reign, hammurabi_king, 0.95).

intent_beneficiary_class(babylonian_reign, gentleman_class).
intent_power_change(babylonian_reign, gentleman_class, 0.40).

intent_beneficiary_class(babylonian_reign, poor_man_class).
intent_power_change(babylonian_reign, poor_man_class, -0.30).

intent_beneficiary_class(babylonian_reign, slave_class).
intent_power_change(babylonian_reign, slave_class, -0.80).

% -----------------------------------------------------------------------------
% SECTION 6: RECOMMENDATIONS & AUDIT FINDINGS
% -----------------------------------------------------------------------------

% Audit detects: Code of Hammurabi -> Transformation: Rope to Noose
% Reason: §6-§10 (Death for theft), §210 (Vicarious daughter execution)
recommendation(rec_1, 'Cut high-extraction clauses in property and capital law').
affects_constraint(rec_1, code_of_hammurabi).

% Audit detects: Marriage Bond -> Signature: Coordination Scaffold
% Reason: Voluntary adoption (low suppression) + alternatives existed.
recommendation(rec_2, 'Maintain standard; it supports social cohesion without excessive extraction').
affects_constraint(rec_2, marriage_bond_standard).

% Audit detects: Meta-logical Fraud
% Claimed: Mountain (Judgments of Righteousness)
% Measured: Noose (High extraction/suppression + asymmetric beneficiaries)
veto_actor(hammurabi_king).
veto_exposed(hammurabi_king, rec_1).

% Omega Variables
omega_variable(om_1, conceptual, 'Degree of actual enforcement vs theoretical severity in rural districts').
