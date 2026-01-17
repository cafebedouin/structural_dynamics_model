% Section 1: Entities & Intervals
% Entities defined from ASCE 7-22 metadata and structural role
entity(asce_7_22, structural).
entity(asce_org, organizational).
entity(asce_standard_committee, organizational).
entity(public_safety, class).
entity(building_codes, structural).
entity(asce_7_hazard_tool, scaffold). % Digital support for required data

% Interval representing the lifecycle from the 2016 edition (T=0) 
% through 2022 release (T=6) to the current year 2026 (T=10)
interval(standard_evolution, 0, 10). 

% Section 2: Events
% Key milestones in the standard development process
event(e1, rule_revision_approval, 0, [authority(board_of_direction)]).
event(e2, standard_publication, 6, [edition('7-22'), publisher(asce)]).
event(e3, digital_data_access, 10, [platform(asce7_hazard_tool)]).

% Section 3: Constraint Claims & Metrics
% The standard provisions claim to be natural safety requirements (Mountain)
constraint_claim(asce_7_22, mountain).

% Current state metrics at T=10
constraint_metric(asce_7_22, extractiveness, 0.04).
constraint_metric(asce_7_22, suppression_requirement, 0.05).
constraint_metric(asce_7_22, snap_back_potential, 0.0).

% Section 4: Temporal Measurements (For Modal Logic Transformation Analysis)
% T=0: ASCE 7-16 Baseline
measurement(m1, asce_7_22, extractiveness, 0, 0.02).
measurement(m2, asce_7_22, suppression_requirement, 0, 0.03).

% T=6: ASCE 7-22 Implementation (Technical expansion: Tornado, Tsunami updates)
measurement(m3, asce_7_22, extractiveness, 6, 0.04).
measurement(m4, asce_7_22, suppression_requirement, 6, 0.05).

% T=10: 2026 Maintenance (Mandatory digital data dependency)
measurement(m5, asce_7_22, extractiveness, 10, 0.04).
measurement(m6, asce_7_22, suppression_requirement, 10, 0.05).

% Section 5: Dependencies
% Global building codes depend on the loading criteria established
affects_constraint(asce_7_22, building_codes).
% Mandatory tsunami, snow, and seismic design requires the hazard tool
affects_constraint(asce_7_hazard_tool, asce_7_22).

% Section 6: Intent Evidence
% Primary beneficiary is public health, safety, and welfare
intent_beneficiary_class(standard_evolution, public_safety).
intent_power_change(standard_evolution, public_safety, 0.20).

% Section 7: Recommendations
recommendation(rec1, 'Adopt ASCE 7-22 to coordinate with current structural material standards').
affects_constraint(rec1, asce_7_22).

% Section 8: Omega Variables
% Genuine uncertainty regarding independent assessment of utility
omega_variable(om1, conceptual, 'ASCE does not warrant accuracy or suitability of processes discussed').
