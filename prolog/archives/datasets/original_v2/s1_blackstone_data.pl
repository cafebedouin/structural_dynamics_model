entity(blackstone_group_lp, partnership).
entity(blackstone_holdings, scaffold).
entity(general_partner_mgmt_llc, management_entity).
entity(existing_owners, owners_group).
entity(state_investment_company_china, investor).
entity(public_unitholders, investor_group).
entity(us_congress, regulatory_body).

interval(pre_ipo_phase, 0, 1).
interval(reorganization_event, 1, 2).
interval(post_ipo_phase, 2, 3).

event(reorganization_formation, structural_shift, 1, [type(holding_partnership), mechanism(contribution_of_interests)]).
event(ipo_issuance, liquidity_event, 2, [common_units(133333334), price_per_unit(30.00)]).
event(sic_purchase, state_investment, 2, [investment_amount(3000000000), voting_rights(none)]).
event(fiduciary_waiver, legal_modification, 1, [scope(elimination_of_duties), status(active)]).

omega_variable(fiduciary_ambiguity, conceptual, interpretation_of_best_interests_under_contractual_standards).
omega_variable(tax_legislation_outcome, empirical, measurement_of_actual_liability_if_publicly_traded_partnership_rules_change).
omega_variable(investor_risk_tolerance, preference, decision_to_accept_limited_governance_for_asset_class_exposure).

constraint_claim(voting_power_concentration, mountain).
constraint_claim(fiduciary_duty_elimination, snare).
constraint_claim(carried_interest_taxation_knot, tangled_rope).
constraint_claim(founder_dependence, rope).

constraint_metric(voting_power_concentration, intensity, 0.86).
constraint_metric(voting_power_concentration, extractiveness, 0.78).
constraint_metric(fiduciary_duty_elimination, extractiveness, 0.90).
constraint_metric(fiduciary_duty_elimination, suppression_requirement, 0.95).
constraint_metric(carried_interest_taxation_knot, snap_back_potential, 0.75).
constraint_metric(founder_dependence, intensity, 0.70).

recommendation(rec_001, establish_independent_audit_committee_for_related_person_transactions).
recommendation(rec_002, define_concrete_performance_benchmarks_to_resolve_fiduciary_ambiguity).

affects_constraint(rec_001, voting_power_concentration).
affects_constraint(rec_002, fiduciary_duty_elimination).

veto_actor(general_partner_mgmt_llc).
veto_exposed(general_partner_mgmt_llc, rec_001).
veto_exposed(general_partner_mgmt_llc, rec_002).

measurement(0, [0.95, 0.80, 0.90, 0.85]).
measurement(3, [0.15, 0.75, 0.65, 0.55]).

intent_evidence(alternatives, private_funding_rounds_or_debt_recapitalization).
intent_evidence(beneficiaries, existing_owners_receiving_3_point_9_billion_cash).
intent_evidence(power_delta, public_unitholders_acquire_12_point_3_percent_equity_but_near_zero_governance).
