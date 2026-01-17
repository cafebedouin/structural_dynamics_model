entity(cloudflare, organization).
entity(internet, environment).
entity(legacy_hardware, technology_barrier).
entity(baidu, strategic_partner).
entity(ipo_scaffold, scaffold).
entity(independent_governance, scaffold).

interval(financial_audit_period, 0, 3). % 0=2016, 1=2017, 2=2018, 3=2019_H1

event(e1, bug_identification, 1, [name(cloudbleed), risk(data_exposure)]).
event(e2, regulatory_shift, 1, [name(tax_cuts_and_jobs_act), impact(deferred_tax_assets)]).
event(e3, route_leak_incident, 3, [origin(major_telco), duration(temporary)]).
event(e4, ipo_filing, 3, [venue(nyse), symbol(net)]).

omega_variable(om_1, empirical, uncertainty_of_ccpa_compliance_costs).
omega_variable(om_2, conceptual, definition_of_internet_properties_vs_nodes).
omega_variable(om_3, preference, investor_threshold_for_long_term_net_losses).

constraint_claim(net_loss_burn, rope).
constraint_claim(baidu_china_dependency, tangled_rope).
constraint_claim(dual_class_voting_structure, noose).
constraint_claim(legacy_on_premise_boxes, zombie).

constraint_metric(net_loss_burn, intensity, 0.87).
constraint_metric(net_loss_burn, suppression_requirement, 0.45).
constraint_metric(net_loss_burn, snap_back_potential, 0.30).
constraint_metric(net_loss_burn, extractiveness, 0.10).

constraint_metric(baidu_china_dependency, intensity, 0.65).
constraint_metric(baidu_china_dependency, suppression_requirement, 0.55).
constraint_metric(baidu_china_dependency, snap_back_potential, 0.82).
constraint_metric(baidu_china_dependency, extractiveness, 0.25).

constraint_metric(dual_class_voting_structure, intensity, 0.90).
constraint_metric(dual_class_voting_structure, suppression_requirement, 0.85).
constraint_metric(dual_class_voting_structure, snap_back_potential, 0.15).
constraint_metric(dual_class_voting_structure, extractiveness, 0.96).

recommendation(rec_1, implement_sunset_provision_on_class_b_shares).
affects_constraint(rec_1, dual_class_voting_structure).

veto_actor(matthew_prince).
veto_actor(michelle_zatlyn).
veto_exposed(matthew_prince, rec_1).
veto_exposed(michelle_zatlyn, rec_1).

measurement(0, [0.70, 0.45, 0.80, 0.55]). % 2016 [Agency, Stability, Utility, Resilience]
measurement(3, [0.85, 0.35, 0.92, 0.62]). % 2019 [Agency, Stability, Utility, Resilience]

intent_evidence(dual_class_voting_structure, single_class_equity, founders, high_voting_delta).
intent_evidence(net_loss_burn, profitability_focus, growth_acceleration, capital_market_entry).
intent_evidence(baidu_china_dependency, local_joint_venture, market_access, regulatory_compliance).
