% 1. Entities & Intervals
entity(google_inc, corporation).
entity(larry_page, founder).
entity(sergey_brin, founder).
entity(eric_schmidt, executive).
entity(dual_class_structure, scaffold).
entity(microsoft_corp, competitor).
entity(yahoo_inc, competitor).
entity(class_a_common, security).
entity(class_b_common, security).

interval(inception_to_ipo, 1998, 2004).
interval(ipo_phase, 2004, 2005).
interval(patent_protection_period, 1998, 2017).

% 2. Events & Omega Variables
event(incorporation_ca, administrative, 1998, [state(california)]).
event(adwords_launch, operational, 2000, [model(self_service)]).
event(profitability_reached, financial, 2001, [driver(adwords)]).
event(reincorporation_de, administrative, 2003, [state(delaware)]).
event(ipo_filing_amendment_5, regulatory, 2004, [price_low(108), price_high(135), clearing_method(auction)]).
event(yahoo_settlement, litigation, 2004, [shares_issued(2700000), charge_low(260000000), charge_high(290000000)]).

omega_variable(omega_e_auction_demand, empirical, 'Actual market demand and clearing price post-Dutch auction').
omega_variable(omega_c_evil_standard, conceptual, 'Audit criteria for adherence to Don\'t be evil principle').
omega_variable(omega_p_growth_vs_margin, preference, 'Investor tolerance for declining operating margins due to Network expansion').

% 3. Constraint Claims & Kinetic Metrics
constraint_claim(founder_voting_lock, snare).
constraint_metric(founder_voting_lock, intensity, 0.83).
constraint_metric(founder_voting_lock, extractiveness, 0.83).
constraint_metric(founder_voting_lock, suppression_requirement, 0.60).
constraint_metric(founder_voting_lock, snap_back_potential, 0.15).

constraint_claim(ad_revenue_monoculture, rope).
constraint_metric(ad_revenue_monoculture, intensity, 0.98).
constraint_metric(ad_revenue_monoculture, extractiveness, 0.40).
constraint_metric(ad_revenue_monoculture, suppression_requirement, 0.10).
constraint_metric(ad_revenue_monoculture, snap_back_potential, 0.90).

constraint_claim(unregistered_securities_liability, piton).
constraint_metric(unregistered_securities_liability, intensity, 0.12).
constraint_metric(unregistered_securities_liability, snap_back_potential, 0.25).
constraint_metric(unregistered_securities_liability, extractiveness, 0.05).
constraint_metric(unregistered_securities_liability, suppression_requirement, 0.05).

constraint_claim(search_engine_competition, mountain).
constraint_metric(search_engine_competition, intensity, 0.75).
constraint_metric(search_engine_competition, extractiveness, 0.10).
constraint_metric(search_engine_competition, suppression_requirement, 0.80).
constraint_metric(search_engine_competition, snap_back_potential, 0.30).

% 4. Recommendations & Veto Points
recommendation(rec_01, 'Phase out dual-class voting structure once market capitalization exceeds defined threshold to restore Agency').
affects_constraint(rec_01, founder_voting_lock).

recommendation(rec_02, 'Diversify revenue streams into enterprise and non-advertising services to reduce intensity of rope constraint').
affects_constraint(rec_02, ad_revenue_monoculture).

veto_actor(larry_page).
veto_actor(sergey_brin).
veto_actor(eric_schmidt).

veto_exposed(larry_page, rec_01).
veto_exposed(sergey_brin, rec_01).
veto_exposed(eric_schmidt, rec_01).

% 5. Measurements
% Vector: [Agency, Stability, Utility, Resilience]
measurement(1998, [0.90, 0.10, 0.05, 0.05]).
measurement(2001, [0.85, 0.40, 0.30, 0.20]).
measurement(2003, [0.80, 0.70, 0.90, 0.60]).
measurement(2004, [0.83, 0.85, 0.95, 0.80]).

% 6. Intent Evidence
intent_evidence(founder_voting_lock, [standard_single_class_ipo, limited_voting_rights], [larry_page, sergey_brin, eric_schmidt], 0.83).
intent_evidence(ad_revenue_monoculture, [subscription_models, enterprise_licensing], [ad_network_partners, internal_sales_org], 0.98).
intent_evidence(unregistered_securities_liability, [proper_sec_registration_pre_grant], [option_holders], 0.02).
