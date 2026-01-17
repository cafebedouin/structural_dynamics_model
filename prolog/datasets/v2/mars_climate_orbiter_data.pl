entity(mco_spacecraft, spacecraft).
entity(nasa_jpl, organization).
entity(lockheed_martin_developer, organization).
entity(navigation_team, organization).
entity(sm_forces_software, software).
entity(amd_interface, interface).
entity(metric_conversion_scaffold, scaffold).

interval(mission_duration, 0, 9).
interval(cruise_phase, 0, 8).
interval(insertion_maneuver, 9, 9).
interval(audit_period, 0, 10).

event(launch, vehicle_launch, 0, [mass(638_kg), vehicle(delta_7427)]).
event(modeling_informal, software_modeling, 1, [method(email), file(amd_timing)]).
event(modeling_formal, software_modeling, 5, [file(amd_file), error(format_prevented_use)]).
event(anomaly_detection, observation, 6, [type(data_discrepancy), status(unreported)]).
event(signal_loss, vehicle_loss, 9, [time(09_04_52_utc), location(mars_occultation)]).
event(root_cause_discovery, forensic_audit, 10, [cause(unit_mismatch), units_found(english_lbf_s)]).

omega_variable(omega_e_1, empirical, actual_impulse_delta_v).
omega_variable(omega_c_1, conceptual, unit_standard_for_sm_forces_output).
omega_variable(omega_p_1, preference, risk_tolerance_for_unvalidated_amd_data).

constraint_claim(mission_safety_standard, mountain).
constraint_claim(software_interface_spec, rope).
constraint_claim(developer_quality_process, tangled_rope).
constraint_claim(unit_consistency_requirement, noose).

constraint_metric(mission_safety_standard, intensity, 0.9).
constraint_metric(mission_safety_standard, suppression_requirement, 0.04).
constraint_metric(mission_safety_standard, snap_back_potential, 0.0).

constraint_metric(software_interface_spec, intensity, 0.5).
constraint_metric(software_interface_spec, extractiveness, 0.30).
constraint_metric(software_interface_spec, suppression_requirement, 0.12).

constraint_metric(developer_quality_process, intensity, 0.7).
constraint_metric(developer_quality_process, extractiveness, 0.55).
constraint_metric(developer_quality_process, suppression_requirement, 0.35).

constraint_metric(unit_consistency_requirement, intensity, 0.95).
constraint_metric(unit_consistency_requirement, extractiveness, 0.85).
constraint_metric(unit_consistency_requirement, suppression_requirement, 0.75).

recommendation(rec_001, perform_system_integration_testing_on_interfacing_components).
recommendation(rec_002, reform_quality_review_to_enforce_interface_specifications).
recommendation(rec_003, cut_reliance_on_english_units_and_implement_metric_validation_scaffold).

affects_constraint(rec_001, software_interface_spec).
affects_constraint(rec_002, developer_quality_process).
affects_constraint(rec_003, unit_consistency_requirement).

veto_actor(project_management).
veto_actor(operations_team).
veto_exposed(project_management, rec_001).
veto_exposed(operations_team, rec_002).

measurement(0, [0.9, 0.8, 1.0, 0.7]).
measurement(5, [0.6, 0.4, 0.5, 0.3]).
measurement(9, [0.0, 0.0, 0.0, 0.0]).

intent_evidence(alternatives, [final_trajectory_correction_maneuver, formal_error_reporting]).
intent_evidence(beneficiaries, [mars_surveyor_98_program, future_robotic_probes]).
intent_evidence(power_deltas, [developer_vs_operations_knowledge_gap, management_vs_technical_dissent]).
