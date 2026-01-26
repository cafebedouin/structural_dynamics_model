% 1. Entities & Intervals
entity(airbnb_inc, company).
entity(brian_chesky, founder).
entity(joe_gebbia, founder).
entity(nathan_blecharczyk, founder).
entity(class_b_holders, power_bloc).
entity(irs_tax_claim, liability_source).
entity(host_endowment_fund, scaffold).
entity(covid_19_pandemic, exogenous_barrier).

interval(operational_history, 0, 13). % 2007 (0) to 2020 (13)
interval(audit_period, 12, 13).      % 2019-2020 heavy disruption

% 2. Events & Omega Variables
event(founding, creation, 0, [site, airbedandbreakfast_com]).
event(workforce_reduction, restructuring, 13, [headcount_loss, 1800_employees]).
event(term_loan_funding, capital_injection, 13, [amount, 2_billion_usd]).
event(irs_notice, liability_assertion, 13, [proposed_increase, 1_35_billion_usd]).

omega_variable(covid_duration, empirical, unknown_persistence_of_travel_restrictions).
omega_variable(active_listing_metric, conceptual, exclusion_of_hotel_tonight_inventory_from_core_count).
omega_variable(host_loyalty_post_cancellation, preference, host_reaction_to_extenuating_circumstances_refunds).

% 3. Constraint Claims & Kinetic Metrics
constraint_claim(voting_concentration, snare).
constraint_metric(voting_concentration, extractiveness, 0.82).
constraint_metric(voting_concentration, suppression_requirement, 0.58).
constraint_metric(voting_concentration, snap_back_potential, 0.91).

constraint_claim(regulatory_fragmentation, tangled_rope).
constraint_metric(regulatory_fragmentation, intensity, 0.88).
constraint_metric(regulatory_fragmentation, extraction_cost, 0.72).

constraint_claim(irs_back_tax_adjustment, piton).
constraint_metric(irs_back_tax_adjustment, intensity, 0.94).

constraint_claim(pandemic_market_collapse, mountain).
constraint_metric(pandemic_market_collapse, intensity, 0.97).

% 4. Recommendations & Veto Points
recommendation(democratize_governance, convert_class_b_to_class_a_parity).
affects_constraint(democratize_governance, voting_concentration).

recommendation(deploy_scaffold_support, utilize_host_endowment_fund_for_governance_transition).
affects_constraint(deploy_scaffold_support, voting_concentration).

veto_actor(brian_chesky).
veto_actor(joe_gebbia).
veto_actor(nathan_blecharczyk).

veto_exposed(brian_chesky, democratize_governance).
veto_exposed(joe_gebbia, democratize_governance).
veto_exposed(nathan_blecharczyk, democratize_governance).

% 5. Measurements
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.05, 0.12, 0.08, 0.30]).
measurement(12, [0.92, 0.85, 0.90, 0.88]).
measurement(13, [0.84, 0.32, 0.75, 0.52]). % Post-COVID measurement

% 6. Intent Evidence
intent_delta(founders, power, 0.99). % Voting power delta vs common shareholders
intent_delta(host_community, resource, 0.25). % 25 percent refund support vs total expected earnings
intent_delta(underwriters, resource, 0.07). % 7 percent directed share program allocation
alternative_path(direct_listing, bypass_underwriting_fees_and_lockups).
alternative_path(long_term_pivot, prioritize_28_plus_day_stays_over_short_term_tourism).
