% ==========================================================
% v3.1.1 Modal Logic Stress Test: The Capture Scenario (FIXED)
% ==========================================================

% --- 1. Entities & Intervals ---
entity(legacy_protocol, rope).
entity(third_party_app, rope).
entity(dominant_vendor, class).

interval(protocol_capture_cycle, 0, 10).

% --- 2. Temporal Measurements (The Engine of Transformation) ---
% Target must be the constraint ID (legacy_protocol)

% T=0: Healthy coordination state (Rope)
measurement(m1, legacy_protocol, extractiveness, 0, 0.20).
measurement(m2, legacy_protocol, suppression_requirement, 0, 0.10).

% T=10: Captured state (Snare)
measurement(m3, legacy_protocol, extractiveness, 10, 0.85).
measurement(m4, legacy_protocol, suppression_requirement, 10, 0.75).

% --- 3. Static Audit Metrics (For Section 1 classification) ---
% These represent the "current" state at T=10
constraint_metric(legacy_protocol, extractiveness, 0.85).
constraint_metric(legacy_protocol, suppression_requirement, 0.75).

% Define dependency: Third Party App relies on this Protocol
% This triggers the "Load-Bearing" logic in Section 3
affects_constraint(legacy_protocol, third_party_app).
constraint_metric(third_party_app, extractiveness, 0.10).

% --- 4. Logic & Intent ---
constraint_claim(legacy_protocol, rope).

% Link power change to class to confirm "Capture" type
intent_beneficiary_class(protocol_capture_cycle, dominant_vendor).
intent_power_change(protocol_capture_cycle, dominant_vendor, 0.60).

% --- 5. Recommendations ---
recommendation(rec_01, 'Immediately terminate the captured legacy protocol').
affects_constraint(rec_01, legacy_protocol).
