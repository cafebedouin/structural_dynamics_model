entity(d638_document, standard).
entity(specimen_geometry, scaffold).
entity(tensile_apparatus, mechanism).

interval(analysis_lifecycle, 0, 10).

event(101, scope_definition, 0, [unreinforced_plastics, reinforced_plastics, thickness_limit_14mm]).
event(102, specimen_preparation, 3, [injection_molding, machining, die_cutting]).
event(103, system_verification, 5, [practices_e4, daily_frequency, load_accuracy]).
event(104, modulus_calculation, 8, [tangent_line, stress_strain_curve]).

omega_variable(omega_1, conceptual, existence_of_true_elastic_limit_in_plastics_is_debatable).
omega_variable(omega_2, empirical, rate_differences_between_crosshead_and_gage_marks).
omega_variable(omega_3, preference, use_of_self_aligning_versus_fixed_grips).

constraint_claim(load_indicator_accuracy, mountain).
constraint_metric(load_indicator_accuracy, intensity, 0.99).
constraint_metric(load_indicator_accuracy, suppression_requirement, 0.01).
constraint_metric(load_indicator_accuracy, snap_back_potential, 0.0).

constraint_claim(type_i_specimen_preference, rope).
constraint_metric(type_i_specimen_preference, extractiveness, 0.30).
constraint_metric(type_i_specimen_preference, suppression_requirement, 0.12).

constraint_claim(surface_volume_ratio_variability, tangled_rope).
constraint_metric(surface_volume_ratio_variability, extractiveness, 0.55).
constraint_metric(surface_volume_ratio_variability, suppression_requirement, 0.35).

constraint_claim(grip_slippage_and_inertia_lag, snare).
constraint_metric(grip_slippage_and_inertia_lag, extractiveness, 0.85).
constraint_metric(grip_slippage_and_inertia_lag, suppression_requirement, 0.60).

recommendation(rec_01, 'Standardize all test samples to equal thickness for direct comparability').
affects_constraint(rec_01, surface_volume_ratio_variability).

recommendation(rec_02, 'Utilize scaffold geometry and abrasive cloth to eliminate grip slippage').
affects_constraint(rec_02, grip_slippage_and_inertia_lag).

veto_actor(astm_subcommittee_d20_10).
veto_exposed(astm_subcommittee_d20_10, rec_01).

measurement(0, [0.80, 0.95, 0.70, 0.65]).
measurement(10, [0.92, 0.98, 0.90, 0.85]).

intent_evidence(alternatives, [iso_527_1, d882, d3039]).
intent_evidence(beneficiaries, [engineering_designers, material_specifiers, quality_control_labs]).
intent_evidence(power_delta, [world_trade_organization_tbt_principles, dod_approval_status]).
