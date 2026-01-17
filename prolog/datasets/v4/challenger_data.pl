% Entity and Interval Definitions
entity(srb_joint_seal, structural). % The aft field joint O-rings [cite: 385, 386]
entity(launch_decision_process, organizational). % Decision-making structure [cite: 467, 471]
entity(launch_schedule_pressure, structural). % Accelerated flight schedule [cite: 551, 555]
entity(safety_program, organizational). % NASA's safety and quality assurance staff [cite: 527, 534]
entity(srm_redesign_team, scaffold). % Post-accident corrective action team [cite: 653]
entity(cold_temperature_physics, class). % Physical laws governing material resiliency [cite: 416]

interval(challenger_program, 0, 100). % From genesis (1960s) to post-accident response (1986)

% Event Log
event(e_genesis, space_shuttle_concept, 0, [origin('1960s')]). % [cite: 35]
event(e_warning_august_1985, o_ring_distress_briefing, 85, [level(nasa_headquarters)]). % [cite: 522]
event(e_teleconference, launch_opposition, 98, [actors(thiokol_engineers)]). % [cite: 530]
event(e_accident, structural_breakup, 99, [time_seconds(73.124)]). % [cite: 105, 110]
event(e_report, commission_findings, 100, [chairman(william_rogers)]). % [cite: 871]

% Constraint Claims and Metrics (T_end = 99, Time of Accident)
constraint_claim(srb_joint_seal, mountain). % Claimed as safe/reliable despite erosion [cite: 502, 503]
constraint_metric(srb_joint_seal, extractiveness, 0.95). % Extreme risk of loss of life/vehicle [cite: 385, 393]
constraint_metric(srb_joint_seal, suppression_requirement, 0.85). % Management ignoring engineer warnings [cite: 467, 509]
constraint_metric(srb_joint_seal, resistance, 0.70). % Thiokol engineers opposing the launch [cite: 467, 479]

constraint_claim(launch_schedule_pressure, rope). % Claimed as necessary program management [cite: 551, 553]
constraint_metric(launch_schedule_pressure, extractiveness, 0.80). % Straining system capabilities/resources [cite: 555, 557]
constraint_metric(launch_schedule_pressure, suppression_requirement, 0.75). % Reducing safety workforce while flight rate increased [cite: 547]
constraint_metric(launch_schedule_pressure, resistance, 0.40). % NASA internal downward revisions of flight rate [cite: 552]

% Temporal Measurements for Signature Detection and Modal Transformation
% Signature: Natural Law (Thermodynamics/Material Science)
measurement(m_physics_ext_0, cold_temperature_physics, extractiveness, 0, 0.01). % Physical reality has no extractiveness [cite: 416]
measurement(m_physics_sup_0, cold_temperature_physics, suppression_requirement, 0, 0.05). % No enforcement needed for physics [cite: 416]
measurement(m_physics_res_0, cold_temperature_physics, resistance, 0, 0.05). % Cannot be resisted by policy [cite: 591]
measurement(m_physics_ext_99, cold_temperature_physics, extractiveness, 99, 0.01). % Constant
measurement(m_physics_sup_99, cold_temperature_physics, suppression_requirement, 99, 0.05). % Constant
measurement(m_physics_res_99, cold_temperature_physics, resistance, 99, 0.05). % Constant

% Transformation: srb_joint_seal (Rope -> Noose)
measurement(m_seal_ext_10, srb_joint_seal, extractiveness, 10, 0.15). % Early design phase [cite: 501]
measurement(m_seal_sup_10, srb_joint_seal, suppression_requirement, 10, 0.10).
measurement(m_seal_res_10, srb_joint_seal, resistance, 10, 0.05).

measurement(m_seal_ext_85, srb_joint_seal, extractiveness, 85, 0.60). % Erosion history emerging [cite: 522]
measurement(m_seal_sup_85, srb_joint_seal, suppression_requirement, 85, 0.55). % Risk treated as "acceptable" [cite: 505, 512]
measurement(m_seal_res_85, srb_joint_seal, resistance, 85, 0.50). % Engineer warnings beginning [cite: 509]

measurement(m_seal_ext_99, srb_joint_seal, extractiveness, 99, 0.95). % Critical failure [cite: 462]
measurement(m_seal_sup_99, srb_joint_seal, suppression_requirement, 99, 0.85). % Total communication breakdown [cite: 536]
measurement(m_seal_res_99, srb_joint_seal, resistance, 99, 0.80). % Direct contractor opposition [cite: 467, 479]

% Signature 2: Coordination Scaffold (UTC/NASA Standards)
% Alternatives existed but standards were established for synchronization
intent_viable_alternative(challenger_program, coordination_protocols, 'Independent Center-based safety reporting'). % [cite: 797, 824]
intent_alternative_rejected(challenger_program, coordination_protocols, 'Inconsistent safety standards across NASA centers'). % [cite: 825]

% Dependencies (Counterfactual Analysis)
affects_constraint(srb_joint_seal, launch_decision_process). % Faulty seal risk should have blocked launch [cite: 467]
affects_constraint(launch_schedule_pressure, safety_program). % Pressure reduced safety workforce effectiveness [cite: 532, 547]
affects_constraint(srb_joint_seal, challenger_destruction). % Direct cause-effect [cite: 385, 462]
affects_constraint(srm_redesign_team, srb_joint_seal). % Scaffold allows for safe return to flight [cite: 657, 772]

% Intent Evidence (Political/Capture Analysis)
intent_beneficiary_class(challenger_program, program_management). % Maintaining schedule/funding [cite: 1303]
intent_power_change(challenger_program, program_management, 0.70). % High pressure to meet 24 flights/year [cite: 553]
intent_power_change(challenger_program, thiokol_engineers, -0.60). % Loss of decision-making authority [cite: 479]

% Recommendations
recommendation(rec_redesign, 'Change the faulty Solid Rocket Motor joint and seal design'). % [cite: 772]
affects_constraint(rec_redesign, srb_joint_seal).

recommendation(rec_restructure, 'Redefine Program Manager authority and restructure safety organization'). % [cite: 799, 816]
affects_constraint(rec_restructure, launch_decision_process).

recommendation(rec_flight_rate, 'Establish a flight rate consistent with resources'). % [cite: 852]
affects_constraint(rec_flight_rate, launch_schedule_pressure).

% Omega Variables (Uncertainties)
omega_variable(om_ice_effect, empirical, 'Precise effect of ice on the pad during ignition remains an unknown condition'). % [cite: 487, 488]
omega_variable(om_water_clevis, empirical, 'Presence of frozen water in the joint clevis during launch'). % [cite: 442, 443]
