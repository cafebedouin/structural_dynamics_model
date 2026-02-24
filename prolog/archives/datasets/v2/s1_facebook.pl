entity(fb, corporation).
entity(mz, person).
entity(ss, person).
entity(ig, scaffold).
entity(zy, partner).

interval(life_cycle, 0, 8).

event(inc, incorporation, 0, [jurisdiction:delaware]).
event(ig_deal, acquisition_agreement, 8, [target:instagram, cash:300_million, stock:22_999_412_shares]).
event(audit_settlement, ftc_agreement, 7, [duration:20_years, audit_frequency:bi_annual]).

constraint_claim(user_retention, piton).
constraint_claim(mobile_monetization, tangled_rope).
constraint_claim(founder_control, snare).

constraint_metric(user_retention, intensity, 0.33).
constraint_metric(mobile_monetization, extractiveness, 0.15).
constraint_metric(founder_control, suppression_requirement, 0.55).
constraint_metric(founder_control, extractiveness, 0.66).

omega_variable(mobile_revenue, empirical, missing_observable_data_on_sponsored_story_yield_in_news_feed).
omega_variable(social_value, conceptual, ambiguous_metrics_for_open_and_connected_utility).

recommendation(rec_01, remove_noose_via_independent_nominating_function).
affects_constraint(rec_01, founder_control).
veto_actor(mz).
veto_exposed(mz, rec_01).

measurement(0, [0.1, 0.2, 0.5, 0.3]).
measurement(8, [0.9, 0.7, 0.9, 0.6]).

intent_evidence(ipo, liquidity_for_employees, beneficiary(employees), power_delta(public_capital_vs_founder_control)).
