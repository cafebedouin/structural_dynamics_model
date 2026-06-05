% Entity and Interval Definitions
entity(sec_469, class).
entity(irs, organizational).
entity(passive_investors, class).
entity(real_estate_pros, class).
entity(closely_held_c_corps, class).
entity(tax_shelter_regime_pre_1986, scaffold).

interval(tax_era_1986_2000, 1986, 2000).

% Events
event(tax_reform_act_1986, policy_change, 1986, [origin(pub_l_99_514)]).
event(phase_in_complete, transition, 1990, [applicable_percentage(10)]).
event(real_estate_amendment, policy_change, 1993, [provision(c_7), status(exception_added)]).
event(small_business_act_amendment, policy_change, 1996, [provision(g_1_a), status(disposition_clarified)]).

% Constraint Claims
% The code is presented as a necessary structural standard (rope) to prevent tax avoidance.
constraint_claim(sec_469_limit, rope).

% Current State Metrics (at T=2000)
% Highly extractive for passive investors, high suppression (IRS audit/reporting), faces significant lobby resistance.
constraint_metric(sec_469_limit, extractiveness, 0.72).
constraint_metric(sec_469_limit, suppression_requirement, 0.85).
constraint_metric(sec_469_limit, resistance, 0.55).

% Temporal Measurements (Triple Metrics Required for v3.2)

% T=1986: Enactment - Initial shock, but phase-in allows 65% in 1987.
measurement(m1, sec_469_limit, extractiveness, 1986, 0.15).
measurement(m2, sec_469_limit, suppression_requirement, 1986, 0.30).
measurement(m3, sec_469_limit, resistance, 1986, 0.20).

% T=1990: Post Phase-In - Disallowance reaches peak applicable percentage.
measurement(m4, sec_469_limit, extractiveness, 1990, 0.70).
measurement(m5, sec_469_limit, suppression_requirement, 1990, 0.65).
measurement(m6, sec_469_limit, resistance, 1990, 0.40).

% T=1993: Real Property Exception - Carve-out for professionals reduces extractiveness for that class but increases complexity.
measurement(m7, sec_469_limit, extractiveness, 1993, 0.65).
measurement(m8, sec_469_limit, suppression_requirement, 1993, 0.75).
measurement(m9, sec_469_limit, resistance, 1993, 0.50).

% T=2000: Stabilized Regime - Mature snare/tangled rope status.
measurement(m10, sec_469_limit, extractiveness, 2000, 0.72).
measurement(m11, sec_469_limit, suppression_requirement, 2000, 0.85).
measurement(m12, sec_469_limit, resistance, 2000, 0.55).

% Viable Alternatives (Distinguishes Coordination Scaffolds/Constructed from Natural Laws)
intent_viable_alternative(tax_era_1986_2000, tax_shelter_regime_pre_1986, 'Unrestricted deduction of passive losses against active income').
intent_alternative_rejected(tax_era_1986_2000, tax_shelter_regime_pre_1986, 'Closed the tax gap but created asymmetric compliance costs').

% Intent Evidence and Beneficiaries
% Shows Signature 3: Constructed Constraint pattern
intent_beneficiary_class(tax_era_1986_2000, real_estate_pros).
intent_power_change(tax_era_1986_2000, real_estate_pros, 0.65). % Exception in 469(c)(7)

intent_beneficiary_class(tax_era_1986_2000, irs).
intent_power_change(tax_era_1986_2000, irs, 0.45). % Revenue protection through disallowance

% Dependency Modeling
% Corporate status changes (f)(2) and distributions (j)(12) create load-bearing dependencies for entity structure.
affects_constraint(sec_469_limit, closely_held_c_corps).
affects_constraint(sec_469_limit, passive_investors).

% Recommendations
recommendation(rec1, 'Identify if 469(c)(7) material participation requirements have calcified into a barrier for mid-sized entities').
affects_constraint(rec1, sec_469_limit).

% Omega Variables
omega_variable(om1, conceptual, 'Exact delta of compliance burden vs. tax revenue gain in 2000 vs 1986').
