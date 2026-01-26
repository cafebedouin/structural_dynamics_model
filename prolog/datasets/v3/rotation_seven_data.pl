% Clinical Sensor Measurement: Rotation Seven (Orbital Facility Incident)
% Updated for v3.1.2 DR Modal Logic Compliance - Fixed Unpaired Measurements

% Section 1: Entities & Intervals
entity(rina, individual).
entity(anna, individual).
entity(supervisor_kwan, organizational).
entity(system_kubo, structural).
entity(medical_protocol, structural).
entity(scaffold_friendship, scaffold).

interval(rotation_seven_period, 0, 30).

% Section 2: Events
event(e1, soil_exposure, 5, [actor(anna), type(black_soil)]).
event(e2, medical_flag, 15, [actor(nurse_chen), type(isolation)]).
event(e3, credit_drop, 28, [actor(rina), type(autonomy)]).
event(e4, orbital_transfer, 30, [actor(administration), type(permanent_separation)]).

% Section 3: Constraint Claims & Metrics (Current State at T=30)
% Kubo system claims to be a Mountain (Necessity); Measured as Snare
constraint_claim(system_kubo, mountain).
constraint_metric(system_kubo, extractiveness, 0.95).
constraint_metric(system_kubo, suppression_requirement, 0.80).
constraint_metric(system_kubo, snap_back_potential, 0.0).

% Medical protocol claims to be a Rope (Safety); Measured as Snare
constraint_claim(medical_protocol, rope).
constraint_metric(medical_protocol, extractiveness, 0.85).
constraint_metric(medical_protocol, suppression_requirement, 0.90).

% Section 4: Temporal Measurements (Evolution for Modal Analysis)

% Evolution of system_kubo: Rope -> Snare (Capture detected)
measurement(m1, system_kubo, extractiveness, 0, 0.20).
measurement(m2, system_kubo, suppression_requirement, 0, 0.15). % Paired
measurement(m3, system_kubo, extractiveness, 15, 0.60).
measurement(m4, system_kubo, suppression_requirement, 15, 0.55). % Paired
measurement(m5, system_kubo, extractiveness, 30, 0.95).
measurement(m6, system_kubo, suppression_requirement, 30, 0.80). % Paired

% Evolution of medical_protocol: Rope -> Snare (Capture detected)
measurement(m7, medical_protocol, extractiveness, 0, 0.25).
measurement(m8, medical_protocol, suppression_requirement, 0, 0.20). % Paired
measurement(m9, medical_protocol, extractiveness, 30, 0.85).
measurement(m10, medical_protocol, suppression_requirement, 30, 0.90). % Paired

% Coercion Vectors
measurement(m11, rotation_seven_period, accessibility_collapse(individual), 0, 0.10).
measurement(m12, rotation_seven_period, accessibility_collapse(individual), 30, 0.95).
measurement(m13, rotation_seven_period, stakes_inflation, 0, 0.20).
measurement(m14, rotation_seven_period, stakes_inflation, 30, 1.0).

% Section 5: Dependencies
affects_constraint(system_kubo, social_hierarchy).
affects_constraint(medical_protocol, anna).
affects_constraint(scaffold_friendship, rina).

% Section 6: Intent Evidence
intent_beneficiary_class(rotation_seven_period, orbital_administration).
intent_power_change(rotation_seven_period, orbital_administration, 0.85).

% Rejection of transparency by the system controller
intent_viable_alternative(rotation_seven_period, system_kubo, 'transparency_regarding_exposure').
intent_alternative_rejected(rotation_seven_period, system_kubo, 'transparency_regarding_exposure').

% Rina's rejection of rank-based protocols
intent_viable_alternative(rotation_seven_period, rina, 'strict_adherence_to_rank').
intent_alternative_rejected(rotation_seven_period, rina, 'strict_adherence_to_rank').

% Section 7: Recommendations
recommendation(rec1, 'Divest from the credit hierarchy to reclaim individual autonomy').
affects_constraint(rec1, system_kubo).

recommendation(rec2, 'Identify medical isolation as an extractive snare').
affects_constraint(rec2, medical_protocol).

% Section 8: Omega Variables
omega_variable(omega1, empirical, 'Actual survival rate of orbital kidney transfers').
