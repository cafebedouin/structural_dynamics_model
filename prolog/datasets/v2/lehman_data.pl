entity(lehman_brothers, firm).
entity(richard_fuld, executive).
entity(christopher_omeara, executive).
entity(erin_callan, executive).
entity(ian_lowitt, executive).
entity(ernst_young, auditor).
entity(sec, regulator).
entity(frbny, regulator).
entity(matthew_lee, whistleblower).
entity(valukas_examiner, scaffold).

interval(audit_period, 0, 10). % 0 = Jan 2008, 10 = Sept 15 2008 bankruptcy.

event(e1, principal_investment_pivot, 0, [strategy(storage), risk_profile(aggressive)]).
event(e2, repo_105_spike, 3, [volume(50_billion), purpose(balance_sheet_manipulation)]).
event(e3, matthew_lee_letter, 5, [date(may_2008), subject(accounting_impropriety)]).
event(e4, ey_audit_committee_meeting, 6, [date(june_13_2008), omission(repo_105_allegations)]).
event(e5, liquidity_pool_encumbrance, 8, [liquid_value(2_billion), reported_value(41_billion)]).
event(e6, bankruptcy_filing, 10, [case(08-13555), jurisdiction(sdny)]).

omega_variable(omega_e_1, empirical, actual_monetizability_of_comfort_deposits_at_clearing_banks).
omega_variable(omega_c_1, conceptual, definition_of_unencumbered_liquidity_within_cse_framework).
omega_variable(omega_p_1, preference, prioritization_of_reported_leverage_ratios_over_actual_asset_transparency).

constraint_claim(repo_105_practice, noose).
constraint_metric(repo_105_practice, intensity, 0.88).
constraint_metric(repo_105_practice, suppression_requirement, 0.95).
constraint_metric(repo_105_practice, extractiveness, 0.92).
constraint_metric(repo_105_practice, snap_back_potential, 0.15).

constraint_claim(liquidity_pool_overstatement, zombie).
constraint_metric(liquidity_pool_overstatement, intensity, 0.90).
constraint_metric(liquidity_pool_overstatement, extractiveness, 0.85).

recommendation(rec_1, full_disclosure_of_repo_accounting_to_board_and_regulators).
affects_constraint(rec_1, repo_105_practice).
veto_actor(richard_fuld).
veto_exposed(richard_fuld, rec_1).

measurement(0, [0.85, 0.75, 0.90, 0.80]). % Start: High agency/utility, stable optics.
measurement(10, [0.10, 0.02, 0.01, 0.00]). % End: Total loss of agency, stability, and resilience.

intent_evidence(repo_105_practice, [
alternatives(sell_illiquid_assets_at_loss),
beneficiaries(senior_executives_performance_metrics),
power_deltas(reported_leverage_12_1_vs_actual_13_9)
]).

intent_evidence(audit_omission, [
alternatives(informing_audit_committee_of_whistleblower_concerns),
beneficiaries(ernst_young_retention),
power_deltas(suppression_of_board_oversight_authority)
]).
