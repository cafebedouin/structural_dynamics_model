% Section 1: Entities & Intervals
% The interval represents the duration of the "Cyclops" episode in Barney Kiernan’s pub.
entity(citizen_nationalism, structural).
entity(bloom_universalism, individual).
entity(imperial_maritime_rule, organizational).
entity(anti_semitic_bias, structural).
entity(rumbold_execution_protocol, structural).
entity(legal_debt_collection, structural).
entity(the_law, structural).
entity(scaffold_humane_logic, scaffold).

interval(cyclops_pub_interval, 1630, 1745). % 4:30 PM to 5:45 PM

% Section 2: Events
event(e1, arrival_at_pub, 1630, [actor(narrator, joe_hynes)]).
event(e2, rumbold_letter_reading, 1700, [content(execution_technique)]).
event(e3, throwaway_misunderstanding, 1715, [source(lenehan)]).
event(e4, nationalist_climax, 1740, [actor(the_citizen), target(bloom)]).
event(e5, bloom_ascent, 1745, [trajectory(forty_five_degrees)]).

% Section 3: Constraint Claims & Metrics
% Current state metrics at T=1745 (Conclusion of the episode)

% The Citizen's Nationalism claims to be a Mountain (Natural Law of Irish identity)
constraint_claim(citizen_nationalism, mountain).
constraint_metric(citizen_nationalism, extractiveness, 0.92).
constraint_metric(citizen_nationalism, suppression_requirement, 0.95).
constraint_metric(citizen_nationalism, resistance, 0.85).

% Anti-Semitic Bias functions as a Noose in this environment
constraint_claim(anti_semitic_bias, noose).
constraint_metric(anti_semitic_bias, extractiveness, 0.98).
constraint_metric(anti_semitic_bias, suppression_requirement, 0.90).
constraint_metric(anti_semitic_bias, resistance, 0.92).

% Imperial Rule is a Constructed Constraint claiming necessity
constraint_claim(imperial_maritime_rule, mountain).
constraint_metric(imperial_maritime_rule, extractiveness, 0.88).
constraint_metric(imperial_maritime_rule, suppression_requirement, 0.85).
constraint_metric(imperial_maritime_rule, resistance, 0.70).

% Section 4: Temporal Measurements
% Detecting the transformation from social "Rope" to "Noose" as the argument escalates.

% Time T=1630 (Arrival/Initial discussion)
measurement(m1, citizen_nationalism, extractiveness, 1630, 0.30).
measurement(m2, citizen_nationalism, suppression_requirement, 1630, 0.25).
measurement(m3, citizen_nationalism, resistance, 1630, 0.20).

measurement(m4, anti_semitic_bias, extractiveness, 1630, 0.40).
measurement(m5, anti_semitic_bias, suppression_requirement, 1630, 0.35).
measurement(m6, anti_semitic_bias, resistance, 1630, 0.30).

% Time T=1700 (Rumbold discussion/Intensification)
measurement(m7, citizen_nationalism, extractiveness, 1700, 0.55).
measurement(m8, citizen_nationalism, suppression_requirement, 1700, 0.60).
measurement(m9, citizen_nationalism, resistance, 1700, 0.50).

% Time T=1745 (Bloom's departure/Physical violence attempt)
measurement(m10, citizen_nationalism, extractiveness, 1745, 0.92).
measurement(m11, citizen_nationalism, suppression_requirement, 1745, 0.95).
measurement(m12, citizen_nationalism, resistance, 1745, 0.85).

measurement(m13, anti_semitic_bias, extractiveness, 1745, 0.98).
measurement(m14, anti_semitic_bias, suppression_requirement, 1745, 0.90).
measurement(m15, anti_semitic_bias, resistance, 1745, 0.92).

% Section 5: Viable Alternatives (Signature Detection)
% Distinguishing "Constructed Constraints" (Bias) from "Coordination Scaffolds".

% Citizen's Nationalism: Alternatives exist and are explicitly rejected
intent_viable_alternative(cyclops_pub_interval, citizen_nationalism, 'Universal humanism/Universal Love').
intent_alternative_rejected(cyclops_pub_interval, citizen_nationalism, 'Universal humanism/Universal Love').

intent_viable_alternative(cyclops_pub_interval, citizen_nationalism, 'European cultural integration').
intent_alternative_rejected(cyclops_pub_interval, citizen_nationalism, 'European cultural integration').

% Imperial Rule: Alternatives discussed (Irish Navy/Trade)
intent_viable_alternative(cyclops_pub_interval, imperial_maritime_rule, 'Irish sovereign fleet').
intent_alternative_rejected(cyclops_pub_interval, imperial_maritime_rule, 'Irish sovereign fleet').

% Section 6: Dependencies
% Modeling how the Citizen's nationalism is a reaction to (and depends on) Imperial Rule.
affects_constraint(imperial_maritime_rule, citizen_nationalism).
affects_constraint(citizen_nationalism, anti_semitic_bias).
affects_constraint(anti_semitic_bias, bloom_universalism).

% Section 7: Intent Evidence
% The Irish nationalist class seeks to regain power (Delta > 0), while the British state extracts.
intent_beneficiary_class(cyclops_pub_interval, irish_nationalist_agitators).
intent_power_change(cyclops_pub_interval, irish_nationalist_agitators, 0.65).

intent_beneficiary_class(cyclops_pub_interval, british_imperial_administration).
intent_power_change(cyclops_pub_interval, british_imperial_administration, 0.80).

% Section 8: Recommendations
recommendation(rec1, 'Cut the Noose of reactionary bias to allow for humane coordination').
affects_constraint(rec1, anti_semitic_bias).

recommendation(rec2, 'Deploy universalism as a scaffold to bridge nationalist/imperialist extraction').
affects_constraint(rec2, scaffold_humane_logic).

% Section 9: Omega Variables
omega_variable(om1, conceptual, 'Exact boundary between valid cultural preservation and exclusionary nationalism').
omega_variable(om2, empirical, 'Final velocity of the biscuit box projectile').
