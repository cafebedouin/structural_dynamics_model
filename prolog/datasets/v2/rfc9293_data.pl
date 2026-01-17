% 1. Entities & Intervals
entity(ietf, standards_organization).
entity(rfc_9293, specification_document).
entity(tcp, transmission_control_protocol).
entity(endpoint, network_node).
entity(segment, data_unit).

interval(protocol_evolution, 1981, 2022). % From RFC 793 to RFC 9293
interval(three_way_handshake, 0, 3). % Relative time units for connection establishment

% 2. Events & Omega Variables
event(ev_01, rfc_9293_publication, 2022, [consolidation_of_errata, deprecation_of_rfc_793, protocol_modernization]).
event(ev_02, connection_initiation, 1, [syn_sent, syn_received, established]).
event(ev_03, data_transmission, 2, [sequence_number_management, acknowledgment_loop, window_adjustment]).
event(ev_04, connection_termination, 3, [fin_wait, closing, time_wait]).

omega_variable(ov_01, empirical, actual_latency_thresholds_for_retransmission_timeouts).
omega_variable(ov_02, conceptual, definition_of_robustness_principle_in_modern_security_contexts).
omega_variable(ov_03, preference, vendor_priority_between_protocol_strictness_and_backward_compatibility).

% 3. Constraint Claims & Kinetic Metrics
% Tangled Rope (⊠T): Legacy compatibility requirements across diverse hardware (Reform required)
constraint_claim(legacy_interoperability, tangled_rope).
constraint_metric(legacy_interoperability, extractiveness, 0.45).
constraint_metric(legacy_interoperability, suppression_requirement, 0.30).
constraint_metric(legacy_interoperability, snap_back_potential, 0.70).

% Noose (⊠C): Strict sequence number validation to prevent blind injection (Cut required/Tightened)
constraint_claim(security_validation_logic, noose).
constraint_metric(security_validation_logic, extractiveness, 0.85).
constraint_metric(security_validation_logic, suppression_requirement, 0.60).
constraint_metric(security_validation_logic, snap_back_potential, 0.10).

% Mountain (■C): Stability of the Transmission Control Block (TCB) state machine
constraint_claim(tcb_state_stability, mountain).
constraint_metric(tcb_state_stability, intensity, 0.98).
constraint_metric(tcb_state_stability, suppression_requirement, 0.02).
constraint_metric(tcb_state_stability, snap_back_potential, 0.0).

% 4. Recommendations & Veto Points
recommendation(rec_01, update_implementations_to_consolidated_error_handling_standards).
affects_constraint(rec_01, security_validation_logic).

recommendation(rec_02, formalize_congestion_control_interactions_within_base_spec).
affects_constraint(rec_02, legacy_interoperability).

veto_actor(internet_engineering_steering_group).
veto_exposed(internet_engineering_steering_group, rec_02).

% 5. Measurements
% Vector: [Reliability, Interoperability, Security, Efficiency]
% t=1981 (RFC 793 Baseline)
measurement(1981, [0.80, 0.90, 0.30, 0.70]).
% t=2022 (RFC 9293 Modernization)
measurement(2022, [0.95, 0.98, 0.85, 0.90]).

% 6. Intent Evidence
% Primary Objective: Consolidate 40+ years of TCP updates into a single, coherent, and secure specification.
% Beneficiaries: Network engineers (clarity), OS developers (standardization), Global internet stability.
% Power Delta: Transition from a fragmented set of protocol "patches" to a unified modern standard.
% Alternatives: Maintaining RFC 793 with continued external errata (rejected due to operational complexity).
