entity(nasa, organization).
entity(morton_thiokol, contractor).
entity(presidential_commission, investigative_body).
entity(challenger_orbiter, spacecraft).
entity(right_srb, hardware).
entity(o_ring_seals, hardware).
entity(joint_redesign_team, scaffold).

interval(design_phase, 1970, 1985).
interval(accident_sequence, 0, 74).

event(ev_01, ignition, 0, [srm_ignition_command, met_0_000]).
event(ev_02, joint_failure, 1, [puff_of_gray_smoke, aft_field_joint, met_0_678]).
event(ev_03, flame_plume, 59, [continuous_well_defined_plume, right_srb, met_59_262]).
event(ev_04, tank_breach, 64, [external_tank_leak_detected, met_64_660]).
event(ev_05, structural_breakup, 73, [explosive_burn, vehicle_destruction, met_73_124]).

omega_variable(ov_01, empirical, true_probability_of_srb_failure_given_historical_anomaly_data).
omega_variable(ov_02, conceptual, operational_meaning_of_safety_factor_in_non_understood_failure_modes).
omega_variable(ov_03, preference, management_weighting_of_launch_schedule_adherence_versus_engineering_dissent).

constraint_claim(faulty_joint_design, noose).
constraint_metric(faulty_joint_design, extractiveness, 0.88).
constraint_metric(faulty_joint_design, suppression_requirement, 0.72).
constraint_metric(faulty_joint_design, snap_back_potential, 0.10).

constraint_claim(normalization_of_deviance, tangled_rope).
constraint_metric(normalization_of_deviance, extractiveness, 0.55).
constraint_metric(normalization_of_deviance, suppression_requirement, 0.38).
constraint_metric(normalization_of_deviance, snap_back_potential, 0.45).

constraint_claim(political_pressure_to_launch, tangled_rope).
constraint_metric(political_pressure_to_launch, extractiveness, 0.62).
constraint_metric(political_pressure_to_launch, suppression_requirement, 0.42).
constraint_metric(political_pressure_to_launch, snap_back_potential, 0.50).

constraint_claim(management_isolation, noose).
constraint_metric(management_isolation, extractiveness, 0.75).
constraint_metric(management_isolation, suppression_requirement, 0.65).
constraint_metric(management_isolation, snap_back_potential, 0.05).

recommendation(rec_01, redesign_srb_joint_and_seal_to_be_insensitive_to_environmental_effects).
affects_constraint(rec_01, faulty_joint_design).

recommendation(rec_02, establish_independent_safety_reliability_and_quality_assurance_office).
affects_constraint(rec_02, management_isolation).

recommendation(rec_03, reform_flight_readiness_review_to_include_astronaut_and_safety_representatives).
affects_constraint(rec_03, normalization_of_deviance).

veto_actor(nasa_management).
veto_exposed(nasa_management, rec_01).

measurement(0, [0.85, 0.90, 0.95, 0.40]).
measurement(74, [0.00, 0.00, 0.00, 0.00]).

% Intent Evidence
% Primary Objective: Maintenance of high-frequency launch schedule (24 flights/year goal).
% Beneficiaries: NASA management (budgetary support), satellite customers, Teacher in Space Project.
% Power Delta: Shift from engineering-led safety culture (Apollo era) to schedule-driven management culture.
% Alternatives: Grounding the fleet for joint redesign (rejected due to cost/schedule), horizontal testing (insufficient).
