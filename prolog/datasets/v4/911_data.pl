% DR Modal Logic + Structural Signatures Audit
% Target: The 9/11 Commission Report (v3.2 Update)
% Mode: CLINICAL SENSOR - Measurements Only

% --- 1. ENTITIES & INTERVALS ---
entity(al_qaeda, organizational).
entity(faa_norad_interface, structural).
entity(us_aviation_security, structural).
entity(intelligence_wall, structural).
entity(nine_eleven_commission, scaffold).

% Viable Alternatives (Required for Coordination Signature Detection)
entity(unified_watchlist, scaffold).
entity(hardened_cockpit_doors, scaffold).
entity(integrated_radar_system, scaffold).

% Time Mapping: T=0 (1993), T=5 (1998), T=10 (2001-09-11)
interval(pre_war_period, 0, 10).
interval(attack_window, 10, 11).

% --- 2. EVENTS ---
event(wtc_93, kinetic, 0, [impact_type(bombing)]).
event(embassy_bombings, kinetic, 5, [location(east_africa)]).
event(uss_cole, kinetic, 8, [location(yemen)]).
event(nine_eleven, catastrophic, 10, [method(suicide_hijack)]).

% --- 3. CONSTRAINT CLAIMS & CURRENT METRICS (T=10) ---

% Claims of necessity
constraint_claim(us_aviation_security, mountain).
constraint_claim(faa_norad_interface, mountain).

% Current State (T=10) - Required Triple Metrics
constraint_metric(us_aviation_security, extractiveness, 1.00).
constraint_metric(us_aviation_security, suppression_requirement, 0.95).
constraint_metric(us_aviation_security, resistance, 0.05).

constraint_metric(intelligence_wall, extractiveness, 0.70).
constraint_metric(intelligence_wall, suppression_requirement, 0.80).
constraint_metric(intelligence_wall, resistance, 0.98).

% --- 4. TEMPORAL MEASUREMENTS (Triple Metrics per Point) ---

% US Aviation Security: Transformation from Alert to Collapse
measurement(m1, us_aviation_security, extractiveness, 0, 0.10).
measurement(m2, us_aviation_security, suppression_requirement, 0, 0.40).
measurement(m3, us_aviation_security, resistance, 0, 0.85).

measurement(m4, us_aviation_security, extractiveness, 5, 0.30).
measurement(m5, us_aviation_security, suppression_requirement, 5, 0.70).
measurement(m6, us_aviation_security, resistance, 5, 0.60).

measurement(m7, us_aviation_security, extractiveness, 10, 1.00).
measurement(m8, us_aviation_security, suppression_requirement, 10, 0.95).
measurement(m9, us_aviation_security, resistance, 10, 0.05).

% Intelligence Wall: Calcification of Structural Failure
measurement(m10, intelligence_wall, extractiveness, 0, 0.05).
measurement(m11, intelligence_wall, suppression_requirement, 0, 0.10).
measurement(m12, intelligence_wall, resistance, 0, 0.90).

measurement(m13, intelligence_wall, extractiveness, 10, 0.70).
measurement(m14, intelligence_wall, suppression_requirement, 10, 0.80).
measurement(m15, intelligence_wall, resistance, 10, 0.98).

% --- 5. VIABLE ALTERNATIVES (Discriminator for Signatures) ---

% Presence of these proves aviation security was a Choice/Scaffold, not a Natural Law
intent_viable_alternative(pre_war_period, us_aviation_security, 'Hardened cockpit doors').
intent_alternative_rejected(pre_war_period, us_aviation_security, 'Hardened cockpit doors').

intent_viable_alternative(pre_war_period, us_aviation_security, 'Unified terrorist watchlist').
intent_alternative_rejected(pre_war_period, us_aviation_security, 'Unified terrorist watchlist').

intent_viable_alternative(pre_war_period, faa_norad_interface, 'Integrated FAA/NORAD radar tracking').
intent_alternative_rejected(pre_war_period, faa_norad_interface, 'Integrated FAA/NORAD radar tracking').

% --- 6. DEPENDENCIES & INTENT ---

% The wall blocks the security system
affects_constraint(intelligence_wall, us_aviation_security).

% Beneficiaries of the Status Quo (Constructed Constraint Signature)
intent_power_change(pre_war_period, bureaucratic_silos, 0.40).

% --- 7. RECOMMENDATIONS ---
recommendation(rec_1, 'Dismantle Intelligence Wall via National Intelligence Director scaffold').
affects_constraint(rec_1, intelligence_wall).

recommendation(rec_2, 'Deploy Hardened Cockpit Doors as a physical scaffold').
affects_constraint(rec_2, us_aviation_security).
