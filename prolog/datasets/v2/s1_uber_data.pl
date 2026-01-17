% 1. Entities & Intervals
entity(uber_technologies, corporation).
entity(dara_khosrowshahi, executive).
entity(independent_drivers, scaffold).
entity(careem_inc, acquisition_target).
entity(atg_investors, stakeholder).

interval(fy_2016, 2016, 2016).
interval(fy_2017, 2017, 2017).
interval(fy_2018, 2018, 2018).
interval(q1_2019_est, 2019, 2019).

% 2. Events & Omega Variables
event(ceo_transition, leadership_shift, 2017, [actor(dara_khosrowshahi), focus(culture_reform)]).
event(careem_purchase_agreement, m_and_a, 2019, [value(3100000000), structure(cash_and_notes)]).
event(atg_investment, funding, 2019, [value(1000000000), source(softbank_toyota_denso)]).
event(divestiture_southeast_asia, restructuring, 2018, [recipient(grab), stake(23.2_percent)]).

omega_variable(worker_classification_outcome, empirical, resolution_requires_judicial_ruling_on_misclassification_claims).
omega_variable(autonomous_safety_threshold, conceptual, ambiguous_safety_parity_metrics_for_hybrid_deployment).
omega_variable(regulatory_arbitrage_viability, preference, governmental_decisions_on_ridesharing_caps_in_major_metros).

% 3. Constraint Claims & Kinetic Metrics
constraint_claim(driver_misclassification, noose).
constraint_metric(driver_misclassification, intensity, 0.88).
constraint_metric(driver_misclassification, suppression_requirement, 0.75).
constraint_metric(driver_misclassification, snap_back_potential, 0.91).
constraint_metric(driver_misclassification, extractiveness, 0.68).

constraint_claim(perpetual_operating_loss, mountain).
constraint_metric(perpetual_operating_loss, intensity, 0.92).
constraint_metric(perpetual_operating_loss, suppression_requirement, 0.85).

constraint_claim(autonomous_technical_inferiority, tangled_rope).
constraint_metric(autonomous_technical_inferiority, intensity, 0.74).

% 4. Recommendations & Veto Points
recommendation(rec_001, shift_to_hybrid_managed_fleet_to_mitigate_legal_exposure).
affects_constraint(rec_001, driver_misclassification).
veto_actor(california_supreme_court).
veto_exposed(california_supreme_court, rec_001).

recommendation(rec_002, aggressive_capital_allocation_to_uber_eats_to_offset_ridesharing_saturation).
affects_constraint(rec_002, perpetual_operating_loss).

% 5. Measurements
% Vector: [Agency, Stability, Utility, Resilience]
measurement(2016, [0.65, 0.40, 0.70, 0.45]).
measurement(2017, [0.55, 0.35, 0.75, 0.38]).
measurement(2018, [0.68, 0.45, 0.82, 0.42]).
measurement(2019, [0.72, 0.42, 0.85, 0.40]).

% 6. Intent Evidence
intent_evidence(alternative, market_exit_via_minority_stakes_in_didi_and_grab).
intent_evidence(beneficiary, institutional_shareholders_and_executive_leadership).
intent_evidence(power_delta, transition_from_founder_centralization_to_independent_board_oversight).
