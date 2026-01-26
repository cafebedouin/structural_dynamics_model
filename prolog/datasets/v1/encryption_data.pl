% 1. Entities & Intervals
entity(state_intelligence_apparatus, organizational).
entity(citizenry_privacy_advocates, class).
entity(encryption_service_providers, organizational).
entity(tech_infrastructure_backbone, structural).
interval(encryption_regulation_epoch, 2020, 2026).

% 2. Events
event(backdoor_mandate_proposal, legislative_action, 2021, [scope(national), tech_impact(high)]).
event(secure_protocol_deprecation, technical_enforcement, 2023, [type(forced_update), visibility(low)]).
event(encrypted_comm_criminalization, legal_shift, 2025, [sanction_level(severe)]).

% 3. Constraint Claims & Metrics
constraint_claim(computation_barrier, mountain).
constraint_metric(computation_barrier, complexity, 0.95).

constraint_claim(state_access_mandate, snare).
constraint_metric(state_access_mandate, leverage, 0.88).

constraint_claim(legacy_unencrypted_vulnerability, piton).
constraint_metric(legacy_unencrypted_vulnerability, persistence, 0.72).

% 4. Recommendations & Veto Structure
recommendation(rec_open_source_sovereignty, 'Deployment of end-to-end encrypted protocols without central escrow.').
recommendation(rec_state_licensed_encryption, 'Use of government-certified encryption modules with lawful access recovery.').

affects_constraint(rec_open_source_sovereignty, computation_barrier).
affects_constraint(rec_state_licensed_encryption, state_access_mandate).

veto_actor(state_intelligence_apparatus).
veto_exposed(citizenry_privacy_advocates, rec_state_licensed_encryption).

% 5. Measurements (A, S, U, R Vectors for T0: 2020)
measurement(m1, state_intelligence_apparatus, accessibility_collapse(individual), 2020, 0.20).
measurement(m2, state_intelligence_apparatus, stakes_inflation(individual), 2020, 0.30).
measurement(m3, state_intelligence_apparatus, suppression(individual), 2020, 0.15).
measurement(m4, state_intelligence_apparatus, resistance(individual), 2020, 0.10).

measurement(m5, state_intelligence_apparatus, accessibility_collapse(organizational), 2020, 0.25).
measurement(m6, state_intelligence_apparatus, stakes_inflation(organizational), 2020, 0.35).
measurement(m7, state_intelligence_apparatus, suppression(organizational), 2020, 0.20).
measurement(m8, state_intelligence_apparatus, resistance(organizational), 2020, 0.15).

measurement(m9, state_intelligence_apparatus, accessibility_collapse(class), 2020, 0.30).
measurement(m10, state_intelligence_apparatus, stakes_inflation(class), 2020, 0.40).
measurement(m11, state_intelligence_apparatus, suppression(class), 2020, 0.25).
measurement(m12, state_intelligence_apparatus, resistance(class), 2020, 0.20).

measurement(m13, state_intelligence_apparatus, accessibility_collapse(structural), 2020, 0.40).
measurement(m14, state_intelligence_apparatus, stakes_inflation(structural), 2020, 0.50).
measurement(m15, state_intelligence_apparatus, suppression(structural), 2020, 0.30).
measurement(m16, state_intelligence_apparatus, resistance(structural), 2020, 0.25).

% 5. Measurements (A, S, U, R Vectors for Tn: 2026)
measurement(m17, state_intelligence_apparatus, accessibility_collapse(individual), 2026, 0.85).
measurement(m18, state_intelligence_apparatus, stakes_inflation(individual), 2026, 0.90).
measurement(m19, state_intelligence_apparatus, suppression(individual), 2026, 0.88).
measurement(m20, state_intelligence_apparatus, resistance(individual), 2026, 0.70).

measurement(m21, state_intelligence_apparatus, accessibility_collapse(organizational), 2026, 0.88).
measurement(m22, state_intelligence_apparatus, stakes_inflation(organizational), 2026, 0.92).
measurement(m23, state_intelligence_apparatus, suppression(organizational), 2026, 0.85).
measurement(m24, state_intelligence_apparatus, resistance(organizational), 2026, 0.75).

measurement(m25, state_intelligence_apparatus, accessibility_collapse(class), 2026, 0.90).
measurement(m26, state_intelligence_apparatus, stakes_inflation(class), 2026, 0.95).
measurement(m27, state_intelligence_apparatus, suppression(class), 2026, 0.82).
measurement(m28, state_intelligence_apparatus, resistance(class), 2026, 0.60).

measurement(m29, state_intelligence_apparatus, accessibility_collapse(structural), 2026, 0.95).
measurement(m30, state_intelligence_apparatus, stakes_inflation(structural), 2026, 0.98).
measurement(m31, state_intelligence_apparatus, suppression(structural), 2026, 0.90).
measurement(m32, state_intelligence_apparatus, resistance(structural), 2026, 0.55).

% 6. Intent Evidence
intent_viable_alternative(encryption_regulation_epoch, system_governance, decentralised_identity_verification).
intent_alternative_rejected(encryption_regulation_epoch, system_governance, decentralised_identity_verification).

intent_beneficiary_class(encryption_regulation_epoch, state_intelligence_apparatus).
intent_power_change(encryption_regulation_epoch, state_intelligence_apparatus, 0.85).

intent_loser_class(encryption_regulation_epoch, citizenry_privacy_advocates).
intent_power_change(encryption_regulation_epoch, citizenry_privacy_advocates, -0.92).

intent_suppression_level(encryption_regulation_epoch, state_intelligence_apparatus, structural, 0.0).
intent_resistance_level(encryption_regulation_epoch, state_intelligence_apparatus, structural, 0.0).

intent_norm_strength(encryption_regulation_epoch, 2020, 0.75).
intent_norm_strength(encryption_regulation_epoch, 2026, 0.20).
