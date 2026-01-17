% Clinical Sensor Measurement: The Calm (Antarctic Traverse Incident)
% Updated for v3.1.2 DR Modal Logic Compliance - Fixed Unpaired Measurements

% Section 1: Entities & Intervals
entity(traversal_crew, organizational).
entity(petrov, individual).
entity(soh, individual).
entity(khatri, individual).
entity(antarctic_treaty_protocol, structural).
entity(the_calm_anomaly, structural).
entity(the_story_transmission, structural).
entity(collective_silence, scaffold). 

% Time range: Day 0 to Day 400
interval(traverse_and_aftermath, 0, 400).

% Section 2: Events
event(e1, bird_fatality, 7, [actor(petrov), tool(ice_axe), species(stercorarius_antarcticus)]).
event(e2, inversion_onset, 8, [type(meteorological_halt)]).
event(e3, inversion_break, 13, [trigger(perceived_sacrifice)]).
event(e4, mission_completion, 40, [location(mcmurdo_station)]).

% Section 3: Constraint Claims & Metrics (State at T=400)
% Treaty Protocol: Claims to be Mountain; Actual is Noose (High X and E)
constraint_claim(antarctic_treaty_protocol, mountain).
constraint_metric(antarctic_treaty_protocol, extractiveness, 0.85). 
constraint_metric(antarctic_treaty_protocol, suppression_requirement, 0.90). 

% The Calm Anomaly: Claims to be Mountain; Actual is Mountain (Dissipated at T=400)
constraint_claim(the_calm_anomaly, mountain).
constraint_metric(the_calm_anomaly, extractiveness, 0.01). 
constraint_metric(the_calm_anomaly, suppression_requirement, 0.01).

% Collective Silence: Claims to be Rope; Actual is Noose (Calcified)
constraint_claim(collective_silence, rope).
constraint_metric(collective_silence, extractiveness, 0.88). 
constraint_metric(collective_silence, suppression_requirement, 0.85).

% Section 4: Temporal Measurements (Evolution for Modal Analysis)

% Evolution: antarctic_treaty_protocol (Rope -> Noose)
measurement(m1, antarctic_treaty_protocol, extractiveness, 0, 0.10).    
measurement(m2, antarctic_treaty_protocol, suppression_requirement, 0, 0.05). % Paired
measurement(m3, antarctic_treaty_protocol, extractiveness, 7, 0.85).    
measurement(m4, antarctic_treaty_protocol, suppression_requirement, 7, 0.95). % Paired
measurement(m5, antarctic_treaty_protocol, extractiveness, 400, 0.85).
measurement(m6, antarctic_treaty_protocol, suppression_requirement, 400, 0.90). % Paired

% Evolution: the_calm_anomaly (Mountain -> Discovery/High-Stakes -> Dissipation)
measurement(m7, the_calm_anomaly, extractiveness, 0, 0.00).
measurement(m8, the_calm_anomaly, suppression_requirement, 0, 0.00). % Paired
measurement(m9, the_calm_anomaly, extractiveness, 8, 0.01).            
measurement(m10, the_calm_anomaly, suppression_requirement, 8, 0.01). % Paired
measurement(m11, the_calm_anomaly, extractiveness, 13, 0.75).           
measurement(m12, the_calm_anomaly, suppression_requirement, 13, 0.70). % Paired (Temporary Noose)
measurement(m13, the_calm_anomaly, extractiveness, 400, 0.01).          
measurement(m14, the_calm_anomaly, suppression_requirement, 400, 0.01). % Paired (Return to Mountain)

% Evolution: collective_silence (Scaffold -> Noose)
measurement(m15, collective_silence, extractiveness, 13, 0.20).         
measurement(m16, collective_silence, suppression_requirement, 13, 0.15). % Paired
measurement(m17, collective_silence, extractiveness, 40, 0.50).         
measurement(m18, collective_silence, suppression_requirement, 40, 0.45). % Paired 
measurement(m19, collective_silence, extractiveness, 400, 0.88).
measurement(m20, collective_silence, suppression_requirement, 400, 0.85). % Paired

% Section 5: Dependencies
affects_constraint(antarctic_treaty_protocol, traversal_crew).
affects_constraint(collective_silence, the_story_transmission).
affects_constraint(the_calm_anomaly, collective_silence).

% Section 6: Intent Evidence
intent_beneficiary_class(traverse_and_aftermath, organizational).
intent_power_change(traverse_and_aftermath, organizational, 0.40). 

intent_viable_alternative(traverse_and_aftermath, soh, 'File criminal report immediately').
intent_viable_alternative(traverse_and_aftermath, khatri, 'Season ends, no one gets paid').

intent_alternative_rejected(traverse_and_aftermath, khatri, 'Season ends, no one gets paid').

% Section 7: Recommendations
recommendation(rec1, 'Externalize the narrative weight via formal report').
affects_constraint(rec1, collective_silence).

recommendation(rec2, 'Decouple mission funding from self-reporting penalties').
affects_constraint(rec2, antarctic_treaty_protocol).

% Section 8: Omega Variables
omega_variable(om1, conceptual, 'Causal link between bird death and thermal inversion').
omega_variable(om2, empirical, 'Current mental status of Petrov in St. Petersburg').
