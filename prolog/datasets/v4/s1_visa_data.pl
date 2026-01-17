% entities and time
entity(visa_inc, organizational).
entity(sec, organizational).
entity(joseph_saunders, individual).
entity(byron_pollitt, individual).
entity(white_and_case_llp, organizational).
entity(davis_polk_and_wardwell, organizational).
entity(amendment_no_6, structural).
entity(exhibit_3_1, structural).
entity(public_market, class).
entity(legacy_member_banks, class).

% Interval defining the filing period of this amendment
% T=0 (Initial S-1 filing, Nov 2007 context) 
% T=10 (Amendment No. 6, March 13, 2008)
interval(ipo_registration_period, 0, 10).

% Events
event(filing_amendment_6, policy_change, 10, [registrant(visa_inc), filed_date('2008-03-13')]).

% 1. Constraint: SEC Form S-1 Registration Requirement
% Signature: Constructed Constraint (Institutionally enforced, asymmetric outcomes)
constraint_claim(s1_registration, rope).
constraint_metric(s1_registration, extractiveness, 0.25).
constraint_metric(s1_registration, suppression_requirement, 0.20).
constraint_metric(s1_registration, resistance, 0.15).

% Temporal measurements for S1 Registration
measurement(m1, s1_registration, extractiveness, 0, 0.20).
measurement(m2, s1_registration, suppression_requirement, 0, 0.20).
measurement(m3, s1_registration, resistance, 0, 0.15).

measurement(m4, s1_registration, extractiveness, 10, 0.25).
measurement(m5, s1_registration, suppression_requirement, 10, 0.20).
measurement(m6, s1_registration, resistance, 10, 0.15).

% 2. Constraint: Exhibit 3.1 (Amended and Restated Certificate of Incorporation)
% This is a structural "Rope" necessary for the transition to a public entity.
constraint_claim(exhibit_3_1, rope).
constraint_metric(exhibit_3_1, extractiveness, 0.10).
constraint_metric(exhibit_3_1, suppression_requirement, 0.05).
constraint_metric(exhibit_3_1, resistance, 0.05).

% Temporal measurements for Exhibit 3.1
% At T=0, it was not yet filed in this specific amendment context.
measurement(m7, exhibit_3_1, extractiveness, 0, 0.05).
measurement(m8, exhibit_3_1, suppression_requirement, 0, 0.05).
measurement(m9, exhibit_3_1, resistance, 0, 0.05).

measurement(m10, exhibit_3_1, extractiveness, 10, 0.10).
measurement(m11, exhibit_3_1, suppression_requirement, 10, 0.05).
measurement(m12, exhibit_3_1, resistance, 10, 0.05).

% 3. Constraint: Litigation Management/Loss Sharing Agreements (Exhibits 10.13 - 10.18)
% Signature: Constructed Constraint (High complexity, high suppression of internal disputes)
constraint_claim(litigation_agreements, tangled_rope).
constraint_metric(litigation_agreements, extractiveness, 0.45).
constraint_metric(litigation_agreements, suppression_requirement, 0.50).
constraint_metric(litigation_agreements, resistance, 0.40).

measurement(m13, litigation_agreements, extractiveness, 0, 0.40).
measurement(m14, litigation_agreements, suppression_requirement, 0, 0.45).
measurement(m15, litigation_agreements, resistance, 0, 0.35).

measurement(m16, litigation_agreements, extractiveness, 10, 0.45).
measurement(m17, litigation_agreements, suppression_requirement, 10, 0.50).
measurement(m18, litigation_agreements, resistance, 10, 0.40).

% Viable Alternatives (For Coordination/Constructed analysis)
% The transition from a member-owned association to a public company had alternatives.
intent_viable_alternative(ipo_registration_period, member_association, 'Continue as a private member-owned association').
intent_alternative_rejected(ipo_registration_period, member_association, 'Private association structure limits capital access and liquidity').

% Dependencies
% The IPO (s1_registration) depends on the filing of governing documents (exhibit_3_1).
affects_constraint(exhibit_3_1, s1_registration).
% The litigation agreements are load-bearing for the restructuring process.
affects_constraint(litigation_agreements, s1_registration).

% Intent Evidence (Beneficiaries of the Constructed Constraint)
intent_beneficiary_class(ipo_registration_period, legacy_member_banks).
intent_power_change(ipo_registration_period, legacy_member_banks, 0.70). % Liquidity via IPO

intent_beneficiary_class(ipo_registration_period, public_market).
intent_power_change(ipo_registration_period, public_market, 0.50). % Equity access

% Recommendations
recommendation(rec1, 'Execute effectiveness of registration statement once exhibit 3.1 is verified').
affects_constraint(rec1, s1_registration).

% Omega Variables
omega_variable(om1, empirical, 'Exact impact of pending Interchange Litigation Sharing on post-IPO valuation').
