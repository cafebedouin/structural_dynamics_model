entity(visa_inc, corporation).
entity(sec, regulator).
entity(scaffold_offering, scaffold).

interval(ipo_process, 0, 10).

event(filing_amendment_6, regulatory_action, 1, [purpose, file_exhibit_3_1]).
event(board_certification, oversight, 5, [actor, joseph_saunders, sign_date, 2008_03_13]).

omega_variable(omega_market_utility, empirical, missing_observable_data_on_post_ipo_valuation).
omega_variable(omega_regulatory_intent, conceptual, ambiguous_interpretation_of_confidential_treatment_rule_406).

constraint_claim(registration_statement, mountain).
constraint_claim(underwriting_agreement, rope).
constraint_claim(executive_compensation_plans, tangled_rope).
constraint_claim(confidential_omissions, snare).

constraint_metric(registration_statement, intensity, 0.90).
constraint_metric(registration_statement, suppression_requirement, 0.05).
constraint_metric(registration_statement, snap_back_potential, 0.0).

constraint_metric(underwriting_agreement, extractiveness, 0.30).
constraint_metric(underwriting_agreement, suppression_requirement, 0.12).

constraint_metric(executive_compensation_plans, extractiveness, 0.55).
constraint_metric(executive_compensation_plans, suppression_requirement, 0.35).

constraint_metric(confidential_omissions, extractiveness, 0.85).
constraint_metric(confidential_omissions, suppression_requirement, 0.60).

recommendation(rec_001, maintain_underwriting_terms).
recommendation(rec_002, reform_bonus_plan_guidelines).
recommendation(rec_003, cut_confidential_treatment_excess).

affects_constraint(rec_001, underwriting_agreement).
affects_constraint(rec_002, executive_compensation_plans).
affects_constraint(rec_003, confidential_omissions).

veto_actor(sec_commissioners).
veto_exposed(sec_commissioners, rec_003).

measurement(0, [0.40, 0.80, 0.50, 0.70]).
measurement(10, [0.85, 0.90, 0.95, 0.90]).

% Intent Evidence:
% Alternatives: Continued private cooperative structure.
% Beneficiaries: Visa Inc. shareholders and executive leadership.
% Power Deltas: Shift from member-bank control to public equity governance.
