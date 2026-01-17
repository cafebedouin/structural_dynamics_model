% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Bay of Pigs
% Domain: Geopolitical Conflict - The Bay of Pigs Invasion (1961)
% ==========================================================

% --- 1. Entities & Intervals ---
entity(john_f_kennedy, individual).
entity(cia, organizational).
entity(cuban_exile_community, class).
entity(cuban_revolutionary_government, class).
entity(cold_war_geopolitics, structural).

interval(bay_of_pigs_invasion, 0, 100). % T0: April 17, 1961; Tn: April 20, 1961

% --- 2. Events ---
event(ev01_initial_air_strikes, theoretical_initiation, 0, [actor(cia), target(cuban_airfields), result(partial_destruction)]).
event(ev02_playa_giron_landings, military_landing, 5, [actor(brigade_2506), location(bay_of_pigs), result(beachhead_established)]).
event(ev03_air_support_withdrawal, parameter_setting, 40, [actor(john_f_kennedy), decision(cancel_air_strikes), effect(strategic_collapse)]).
event(ev04_brigade_surrender, final_verification, 95, [actor(brigade_2506), target(fidel_castro), result(defeat)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The physical geography of the Zapata Peninsula (swamps and coral reefs) which created tactical isolation.
constraint_claim(geographic_tactical_isolation, mountain).
constraint_metric(geographic_tactical_isolation, accessibility_collapse, 0.95).

% Noose: The doctrine of Plausible Deniability. It constricted operational options (air support) until failure was inevitable.
constraint_claim(plausible_deniability_policy, noose).
constraint_metric(plausible_deniability_policy, stakes_inflation, 0.90).

% Zombie: The assumption of a Spontaneous Popular Uprising. A dead strategic concept that continued to drive the invasion plan.
constraint_claim(internal_uprising_myth, zombie).
constraint_metric(internal_uprising_myth, suppression, 0.85).

% Rope: Covert Logistic Dependency. Ties the agency of the Brigade directly to US-controlled supply lines.
constraint_claim(covert_logistic_dependency, rope).
constraint_metric(covert_logistic_dependency, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Authorize direct US Air Force combat sorties to achieve total air superiority over the beachhead.').
recommendation(rec02, 'Implement pre-invasion political mobilization of internal Cuban resistance to validate the uprising assumption.').

affects_constraint(rec01, plausible_deniability_policy).
affects_constraint(rec02, internal_uprising_myth).

veto_actor(john_f_kennedy).
veto_actor(cia).

veto_exposed(john_f_kennedy, rec01). % Vetoed to avoid direct US fingerprints and risk of nuclear escalation.
veto_exposed(cia, rec02). % Vetoed due to the requirement for total secrecy in clandestine planning.

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Invasion Landing Phase)
measurement(m01, john_f_kennedy, accessibility_collapse(individual), 0, 0.40).
measurement(m02, john_f_kennedy, stakes_inflation(individual), 0, 0.70).
measurement(m03, john_f_kennedy, suppression(individual), 0, 0.30).
measurement(m04, john_f_kennedy, resistance(individual), 0, 0.20).

measurement(m05, cia, accessibility_collapse(organizational), 0, 0.20).
measurement(m06, cia, stakes_inflation(organizational), 0, 0.50).
measurement(m07, cia, suppression(organizational), 0, 0.10).
measurement(m08, cia, resistance(organizational), 0, 0.10).

measurement(m09, cuban_exile_community, accessibility_collapse(class), 0, 0.10).
measurement(m10, cuban_exile_community, stakes_inflation(class), 0, 0.95).
measurement(m11, cuban_exile_community, suppression(class), 0, 0.20).
measurement(m12, cuban_exile_community, resistance(class), 0, 0.90).

measurement(m13, cuban_revolutionary_government, accessibility_collapse(structural), 0, 0.10).
measurement(m14, cuban_revolutionary_government, stakes_inflation(structural), 0, 0.80).
measurement(m15, cuban_revolutionary_government, suppression(structural), 0, 0.00). % Beneficiary Rule
measurement(m16, cuban_revolutionary_government, resistance(structural), 0, 0.00). % Beneficiary Rule

% Time Tn (Post-Surrender Phase)
measurement(m17, john_f_kennedy, accessibility_collapse(individual), 100, 0.90).
measurement(m18, john_f_kennedy, stakes_inflation(individual), 100, 0.95).
measurement(m19, john_f_kennedy, suppression(individual), 100, 0.85).
measurement(m20, john_f_kennedy, resistance(individual), 100, 0.10).

measurement(m21, cia, accessibility_collapse(organizational), 100, 0.80).
measurement(m22, cia, stakes_inflation(organizational), 100, 0.90).
measurement(m23, cia, suppression(organizational), 100, 0.95).
measurement(m24, cia, resistance(organizational), 100, 0.10).

measurement(m25, cuban_exile_community, accessibility_collapse(class), 100, 1.00).
measurement(m26, cuban_exile_community, stakes_inflation(class), 100, 1.00).
measurement(m27, cuban_exile_community, suppression(class), 100, 1.00).
measurement(m28, cuban_exile_community, resistance(class), 100, 0.00).

measurement(m29, cuban_revolutionary_government, accessibility_collapse(structural), 100, 0.00).
measurement(m30, cuban_revolutionary_government, stakes_inflation(structural), 100, 0.40).
measurement(m31, cuban_revolutionary_government, suppression(structural), 100, 0.00). % Beneficiary Rule
measurement(m32, cuban_revolutionary_government, resistance(structural), 100, 0.00). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(bay_of_pigs_invasion, cold_war_geopolitics, 'Full_US_Military_Invasion_and_Occupation').
intent_alternative_rejected(bay_of_pigs_invasion, cold_war_geopolitics, 'Full_US_Military_Invasion_and_Occupation').

intent_beneficiary_class(bay_of_pigs_invasion, cuban_revolutionary_government).
intent_power_change(bay_of_pigs_invasion, cuban_revolutionary_government, 0.85). % Massive structural gain and internal consolidation

intent_loser_class(bay_of_pigs_invasion, cuban_exile_community).
intent_power_change(bay_of_pigs_invasion, cuban_exile_community, -0.95). % Total liquidation of political and physical agency

intent_suppression_level(bay_of_pigs_invasion, cuban_revolutionary_government, structural, 0.0).
intent_resistance_level(bay_of_pigs_invasion, cuban_revolutionary_government, structural, 0.0).

intent_norm_strength(bay_of_pigs_invasion, 0, 0.40). % Covert containment norm
intent_norm_strength(bay_of_pigs_invasion, 100, 0.98). % Shift to open Soviet alliance norm
