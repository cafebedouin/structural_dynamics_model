% 1. Entities & Intervals
entity(cia, intelligence_agency).
entity(brigade_2506, paramilitary_force).
entity(cuban_revolutionary_armed_forces, state_military).
entity(revolutionary_council, scaffold).
entity(soviet_union, external_power).
interval(bay_of_pigs_operation, 0, 10).

% 2. Events & Omega Variables
event(ev_01, operation_puma, 2, [airstrike_failure, false_flag_exposure, partial_far_destruction]).
event(ev_02, operation_zapata, 5, [amphibious_landing, coral_reef_obstruction, loss_of_houston]).
event(ev_03, operation_falcon, 6, [paratrooper_dispersal, swamp_equipment_loss]).
event(ev_04, operation_mad_dog, 9, [cancellation_of_air_support, american_pilot_kia]).
event(ev_05, surrender, 10, [capture_of_brigade, far_victory]).

omega_variable(ov_01, empirical, actual_density_of_active_internal_resistance_cells_ready_for_insurrection).
omega_variable(ov_02, conceptual, operational_definition_of_plausible_deniability_under_mass_transport_conditions).
omega_variable(ov_03, preference, kennedy_priority_weighting_of_global_diplomatic_optics_versus_tactical_mission_success).

% 3. Constraint Claims & Kinetic Metrics
% Tangled Rope (⊠T): The assumption of spontaneous internal uprising (Reform required)
constraint_claim(internal_insurrection_logic, tangled_rope).
constraint_metric(internal_insurrection_logic, extractiveness, 0.52).
constraint_metric(internal_insurrection_logic, suppression_requirement, 0.35).
constraint_metric(internal_insurrection_logic, snap_back_potential, 0.60).

% Noose (⊠C): The restriction of US air cover/deniability constraints (Cut required)
constraint_claim(air_support_restriction, noose).
constraint_metric(air_support_restriction, extractiveness, 0.85).
constraint_metric(air_support_restriction, suppression_requirement, 0.75).
constraint_metric(air_support_restriction, snap_back_potential, 0.15).

% Mountain (■C): Stability of Castro government control post-invasion
constraint_claim(castro_consolidation, mountain).
constraint_metric(castro_consolidation, intensity, 0.95).
constraint_metric(castro_consolidation, suppression_requirement, 0.05).
constraint_metric(castro_consolidation, snap_back_potential, 0.0).

% 4. Recommendations & Veto Points
recommendation(rec_01, reform_intelligence_assessment_standards_for_civilian_loyalty).
affects_constraint(rec_01, internal_insurrection_logic).

recommendation(rec_02, cut_operation_or_remove_deniability_constraints_to_permit_carrier_strikes).
affects_constraint(rec_02, air_support_restriction).

veto_actor(john_f_kennedy).
veto_exposed(john_f_kennedy, rec_02).

% 5. Measurements
% Vector: [Agency, Stability, Utility, Resilience]
% t=0 (Initiation/Airstrikes)
measurement(0, [0.75, 0.80, 0.50, 0.65]).
% t=10 (Surrender/Collapse of Operation)
measurement(10, [0.05, 0.15, 0.00, 0.10]).

% 6. Intent Evidence
% Primary Objective: Removal of Castro regime and replacement with pro-US administration.
% Beneficiaries: Cuban exiles (restoration of property), US corporations (re-nationalization reversal), US State Dept (containment).
% Power Delta: Transition from Soviet-aligned revolutionary state to US-aligned democracy.
% Alternatives: Continued sabotage of sugar industry (rejected), full military invasion (vetoed).
