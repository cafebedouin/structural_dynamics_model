% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: RFC 9293 (TCP)
% Domain: Network Protocols - Transmission Control Protocol
% ==========================================================

% --- 1. Entities & Intervals ---
entity(ietf_working_group, organizational).
entity(tcp_protocol_engine, individual).
entity(network_application_developers, class).
entity(internet_infrastructure, structural).
entity(sending_host, individual).
entity(receiving_host, individual).

interval(tcp_connection_lifecycle, 0, 100). % From Listen/Syn-Sent (T0) to Closed (Tn)

% --- 2. Events ---
event(ev01_three_way_handshake, setup, 5, [actor(sending_host), target(receiving_host), state(syn_sent)]).
event(ev02_sequence_number_tracking, measurement, 35, [subject(data_integrity), logic(reliable_delivery)]).
event(ev03_congestion_control, operation, 70, [mechanism(window_management), goal(network_stability)]).
event(ev04_connection_termination, finality, 95, [state(fin_wait), result(closed)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The physical limits of channel bandwidth and propagation delay.
constraint_claim(physical_network_limits, mountain).
constraint_metric(physical_network_limits, accessibility_collapse, 0.95).

% Noose: The 'Reliability Requirement' of TCP. It tightens as packet loss increases.
constraint_claim(retransmission_coercion, noose).
constraint_metric(retransmission_coercion, stakes_inflation, 0.90).

% Zombie: Legacy Window Scaling limits. Parameters formerly sufficient but now functionally inadequate for modern speeds.
constraint_claim(legacy_window_scaling_bias, zombie).
constraint_metric(legacy_window_scaling_bias, suppression, 0.75).

% Rope: TCP State Machine. Ties all host behavior to a rigid sequence of protocol states.
constraint_claim(protocol_state_machine_binding, rope).
constraint_metric(protocol_state_machine_binding, suppression, 0.85).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt multipath TCP to bypass single-path congestion constraints.').
recommendation(rec02, 'Implement selective acknowledgment (SACK) to reduce unnecessary retransmission overhead.').

affects_constraint(rec01, physical_network_limits).
affects_constraint(rec02, retransmission_coercion).

veto_actor(ietf_working_group).
veto_actor(internet_infrastructure).

veto_exposed(ietf_working_group, rec01).
veto_exposed(internet_infrastructure, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Connection Initiation / Handshake Phase)
measurement(m01, tcp_protocol_engine, accessibility_collapse(individual), 0, 0.15).
measurement(m02, tcp_protocol_engine, stakes_inflation(individual), 0, 0.30).
measurement(m03, tcp_protocol_engine, suppression(individual), 0, 0.10).
measurement(m04, tcp_protocol_engine, resistance(individual), 0, 0.05).

measurement(m05, ietf_working_group, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, ietf_working_group, stakes_inflation(organizational), 0, 0.20).
measurement(m07, ietf_working_group, suppression(organizational), 0, 0.05).
measurement(m08, ietf_working_group, resistance(organizational), 0, 0.00).

measurement(m09, network_application_developers, accessibility_collapse(class), 0, 0.25).
measurement(m10, network_application_developers, stakes_inflation(class), 0, 0.40).
measurement(m11, network_application_developers, suppression(class), 0, 0.20).
measurement(m12, network_application_developers, resistance(class), 0, 0.90).

measurement(m13, internet_infrastructure, accessibility_collapse(structural), 0, 0.00).
measurement(m14, internet_infrastructure, stakes_inflation(structural), 0, 0.10).
measurement(m15, internet_infrastructure, suppression(structural), 0, 0.00). % Beneficiary logic
measurement(m16, internet_infrastructure, resistance(structural), 0, 0.00). % Beneficiary logic

% Time Tn (Connection Termination / Closed State)
measurement(m17, tcp_protocol_engine, accessibility_collapse(individual), 100, 0.90). % Resource deallocation
measurement(m18, tcp_protocol_engine, stakes_inflation(individual), 100, 0.95). % Data delivery finalized
measurement(m19, tcp_protocol_engine, suppression(individual), 100, 0.98). % Rigid state adherence
measurement(m20, tcp_protocol_engine, resistance(individual), 100, 0.02). % Submission to closing sequence

measurement(m21, ietf_working_group, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, ietf_working_group, stakes_inflation(organizational), 100, 0.90).
measurement(m23, ietf_working_group, suppression(organizational), 100, 0.05).
measurement(m24, ietf_working_group, resistance(organizational), 100, 0.95).

measurement(m25, network_application_developers, accessibility_collapse(class), 100, 0.10).
measurement(m26, network_application_developers, stakes_inflation(class), 100, 0.95).
measurement(m27, network_application_developers, suppression(class), 100, 0.30).
measurement(m28, network_application_developers, resistance(class), 100, 0.85).

measurement(m29, internet_infrastructure, accessibility_collapse(structural), 100, 0.00).
measurement(m30, internet_infrastructure, stakes_inflation(structural), 100, 0.50).
measurement(m31, internet_infrastructure, suppression(structural), 100, 0.00). % Beneficiary logic
measurement(m32, internet_infrastructure, resistance(structural), 100, 0.00). % Beneficiary logic

% --- 6. Intent Evidence ---
intent_viable_alternative(tcp_connection_lifecycle, internet_infrastructure, 'Unreliable_Stateless_Transport_Standard').
intent_alternative_rejected(tcp_connection_lifecycle, internet_infrastructure, 'Unreliable_Stateless_Transport_Standard').

intent_beneficiary_class(tcp_connection_lifecycle, ietf_working_group).
intent_power_change(tcp_connection_lifecycle, ietf_working_group, 0.95). % Absolute protocol authority

intent_loser_class(tcp_connection_lifecycle, network_application_developers). % Loss of autonomous congestion handling
intent_power_change(tcp_connection_lifecycle, network_application_developers, -0.40).

intent_suppression_level(tcp_connection_lifecycle, ietf_working_group, structural, 0.0).
intent_resistance_level(tcp_connection_lifecycle, ietf_working_group, structural, 0.0).

intent_norm_strength(tcp_connection_lifecycle, 0, 0.90). % Established RFC dominance
intent_norm_strength(tcp_connection_lifecycle, 100, 1.00). % Absolute prerequisite for reliable internet
