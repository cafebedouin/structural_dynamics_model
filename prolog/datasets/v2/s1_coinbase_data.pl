% 1. Entities & Intervals
entity(coinbase_global, corporation).
entity(retail_segment, user_base).
entity(institutional_segment, user_base).
entity(ecosystem_partners, developer_base).
entity(class_b_holders, power_actor).
entity(independent_governance_scaffold, scaffold).

interval(operational_history, 0, 8). % 2012 to 2020 audit period

% 2. Events & Omega Variables
event(founding_event, creation, 0, [jurisdiction(delaware), founder(brian_armstrong)]).
event(remote_first_transition, structure_shift, 8, [trigger(covid_19), status(no_headquarters)]).
event(financial_close_2020, measurement, 8, [total_revenue(1277481), net_income(322317)]).

omega_variable(omega_asset_classification, conceptual, 'Uncertainty of tokens as securities under Howey test').
omega_variable(omega_volatility_impact, empirical, 'Correlation between bitcoin price and transaction revenue').
omega_variable(omega_governance_preference, preference, 'Founder control vs public market accountability').

% 3. Constraint Claims & Kinetic Metrics
constraint_claim(revenue_volatility, tangled_rope).
constraint_metric(revenue_volatility, intensity, 0.88).
constraint_metric(revenue_volatility, snap_back_potential, 0.92).

constraint_claim(regulatory_uncertainty, mountain).
constraint_metric(regulatory_uncertainty, intensity, 0.75).
constraint_metric(regulatory_uncertainty, suppression_requirement, 0.82).

constraint_claim(voting_power_concentration, noose).
constraint_metric(voting_power_concentration, extractiveness, 0.72). % Exceeds 0.66 requirement
constraint_metric(voting_power_concentration, suppression_requirement, 0.55). % Exceeds 0.46 requirement

% 4. Recommendations & Veto Points
recommendation(rec_001, 'Implement sunset provisions on Class B voting rights to align power delta with capital risk').
affects_constraint(rec_001, voting_power_concentration).
veto_actor(brian_armstrong).
veto_exposed(brian_armstrong, rec_001).

% 5. Measurements [Agency, Stability, Utility, Resilience]
measurement(0, [0.15, 0.30, 0.05, 0.20]).
measurement(8, [0.85, 0.55, 0.98, 0.78]).

% 6. Intent Evidence
intent_evidence(rec_001, [status_quo, dual_class_conversion, weighted_voting], common_stockholders, 0.605).
