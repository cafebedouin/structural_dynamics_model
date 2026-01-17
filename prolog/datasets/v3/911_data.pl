% ==============================================================================
% DR MODAL LOGIC AUDIT: 9/11 INSTITUTIONAL COLLAPSE (REPAIRED v3.2.1)
% FILENAME: 911_data.pl
% STATUS: FIXED - Missing Entity Declarations
% ==============================================================================

% Section 1: Entities & Intervals
entity(faa, organizational).
entity(norad, organizational).
entity(fbi, organizational).
entity(the_wall, structural).
entity(hijack_strategy, structural).

% NEW: Added missing declarations to satisfy Step 2 Validator
entity(c_hijack_protocol, structural).
entity(c_info_sharing_wall, structural).

% Intervals (T=0: 1993, T=10: 1998, T=20: 2000, T=30: 2001 Pre-Impact, T=35: Active Attack)
interval(pre_911_era, 0, 30).
interval(active_engagement, 30, 35).

% Section 2: Events
event(e1_wtc_1993, individual, 0, [type(bombing), location(ny)]). 
event(e2_embassy_1998, individual, 10, [type(bombing), location(africa)]). 
event(e3_uss_cole, individual, 20, [type(bombing), location(yemen)]). 
event(e4_911_impact, individual, 31, [type(kinetic_attack)]). 

% Section 3: Constraint Claims & Metrics (Current State at T=30)
% Claimed as "Mountain" (Immutable safety/legal requirements)
constraint_claim(c_hijack_protocol, mountain). 
constraint_metric(c_hijack_protocol, extractiveness, 0.95).
constraint_metric(c_hijack_protocol, suppression_requirement, 0.90).

constraint_claim(c_info_sharing_wall, mountain).
constraint_metric(c_info_sharing_wall, extractiveness, 0.85).
constraint_metric(c_info_sharing_wall, suppression_requirement, 0.80).

% Section 4: Temporal Measurements (Evolution History - PAIRED)

% Hijack Protocol Evolution: Originally protective, became a capture mechanism
measurement(m1, c_hijack_protocol, extractiveness, 0, 0.10). 
measurement(m2, c_hijack_protocol, suppression_requirement, 0, 0.05). 

measurement(m3, c_hijack_protocol, extractiveness, 10, 0.40). 
measurement(m4, c_hijack_protocol, suppression_requirement, 10, 0.35). 

measurement(m5, c_hijack_protocol, extractiveness, 20, 0.60).
measurement(m6, c_hijack_protocol, suppression_requirement, 20, 0.55). 

measurement(m7, c_hijack_protocol, extractiveness, 30, 0.95). 
measurement(m8, c_hijack_protocol, suppression_requirement, 30, 0.90). 

% The Wall Evolution: Legal guidelines calcifying into information death-zones
measurement(m9, c_info_sharing_wall, extractiveness, 10, 0.30).
measurement(m10, c_info_sharing_wall, suppression_requirement, 10, 0.25). 

measurement(m11, c_info_sharing_wall, extractiveness, 30, 0.85). 
measurement(m12, c_info_sharing_wall, suppression_requirement, 30, 0.80). 

% Section 5: Dependency Links
% Barrier blocked threat updates, preventing protocol reform
affects_constraint(c_info_sharing_wall, c_hijack_protocol). 

% Section 6: Recommendations
recommendation(rec1, 'Dissolve intelligence sharing barriers immediately').
affects_constraint(rec1, c_info_sharing_wall).

recommendation(rec2, 'Update hijack protocols for suicide-kinetic threats').
affects_constraint(rec2, c_hijack_protocol).
