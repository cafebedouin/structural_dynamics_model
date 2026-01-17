% Section 1: Entities & Intervals
entity(viceregal_cavalcade, organizational).
entity(ormond_musical_scaffold, scaffold).
entity(sexual_desire_bloom, individual).
entity(nationalist_pathos, class).
entity(bloom_leopold, individual).
entity(boylan_blazes, individual).
entity(blind_tuner, individual).

interval(ormond_sequence, 1600, 1700). % Time approx 4:00 PM to 5:00 PM

% Section 2: Events
event(e1, viceregal_passage, 1605, [witness(douce), quality(steelyringing)]).
event(e2, boylan_departure, 1615, [target(eccles_street), mode(jaunting_car)]).
event(e3, m_appari_performance, 1630, [vocalist(simon_dedalus), listener(bloom)]).
event(e4, letter_composition, 1640, [author(bloom), recipient(martha_clifford)]).
event(e5, tuner_return, 1655, [item(tuning_fork)]).

% Section 3: Schema Alignment (Explicit scaffolding)
constraint_claim(viceregal_cavalcade, mountain).
% Explicitly declare the type to satisfy the v3.1 audit
scaffold_type(ormond_musical_scaffold, coordination_bridge).

constraint_metric(viceregal_cavalcade, extractiveness, 0.85).
constraint_metric(viceregal_cavalcade, suppression_requirement, 0.80).
constraint_metric(viceregal_cavalcade, resistance, 0.65).
constraint_metric(viceregal_cavalcade, snap_back_potential, 0.90).

% The Music functions as a Scaffold for communal cohesion
constraint_claim(ormond_musical_scaffold, scaffold).
constraint_metric(ormond_musical_scaffold, extractiveness, 0.15).
constraint_metric(ormond_musical_scaffold, suppression_requirement, 0.10).
constraint_metric(ormond_musical_scaffold, resistance, 0.05).
constraint_metric(ormond_musical_scaffold, snap_back_potential, 0.20).

% Section 4: Temporal Measurements (Triple Measurements Required)
% Viceregal Cavalcade: Temporally stable high-suppression (Constructed Constraint)
measurement(m1, viceregal_cavalcade, extractiveness, 1600, 0.85).
measurement(m2, viceregal_cavalcade, suppression_requirement, 1600, 0.80).
measurement(m3, viceregal_cavalcade, resistance, 1600, 0.65).

measurement(m4, viceregal_cavalcade, extractiveness, 1700, 0.85).
measurement(m5, viceregal_cavalcade, suppression_requirement, 1700, 0.80).
measurement(m6, viceregal_cavalcade, resistance, 1700, 0.70).

% Ormond Music: Evolution of aesthetic capture
measurement(m7, ormond_musical_scaffold, extractiveness, 1600, 0.10).
measurement(m8, ormond_musical_scaffold, suppression_requirement, 1600, 0.05).
measurement(m9, ormond_musical_scaffold, resistance, 1600, 0.05).

measurement(m10, ormond_musical_scaffold, extractiveness, 1630, 0.25).
measurement(m11, ormond_musical_scaffold, suppression_requirement, 1630, 0.10).
measurement(m12, ormond_musical_scaffold, resistance, 1630, 0.10).

measurement(m13, ormond_musical_scaffold, extractiveness, 1700, 0.15).
measurement(m14, ormond_musical_scaffold, suppression_requirement, 1700, 0.08).
measurement(m15, ormond_musical_scaffold, resistance, 1700, 0.05).

% Section 5: Viable Alternatives (Signature: Coordination vs Constructed)
% For Music: Alternatives existed (Choice of tradition)
intent_viable_alternative(ormond_sequence, ormond_musical_scaffold, 'Native Doric/Nationalist Ballads').
intent_alternative_rejected(ormond_sequence, ormond_musical_scaffold, 'Italian/Florid Opera (Mappari)').

% For Viceregal: Alternatives exist but are institutionally suppressed
intent_viable_alternative(ormond_sequence, viceregal_cavalcade, 'Irish Republic/Sovereignty').
intent_alternative_rejected(ormond_sequence, viceregal_cavalcade, 'Administrative Submission').

% Section 6: Dependencies
affects_constraint(viceregal_cavalcade, nationalist_pathos).
affects_constraint(ormond_musical_scaffold, sexual_desire_bloom).
affects_constraint(blind_tuner, ormond_musical_scaffold). % Resolves the dissonance/mechanical failure

% Section 7: Intent Evidence
intent_beneficiary_class(ormond_sequence, british_administration).
intent_power_change(ormond_sequence, british_administration, 0.80).

intent_beneficiary_class(ormond_sequence, dublin_bourgeoisie).
intent_power_change(ormond_sequence, dublin_bourgeoisie, 0.30). % Aesthetic benefit

% Section 8: Recommendations
recommendation(rec1, 'Sever the extractiveness of the viceregal noose via non-participation').
affects_constraint(rec1, viceregal_cavalcade).

recommendation(rec2, 'Utilize the musical scaffold to mitigate the isolation of individual desire').
affects_constraint(rec2, sexual_desire_bloom).

% Section 9: Omega Variables
omega_variable(om1, conceptual, 'Exact boundary between aesthetic transport and ideological capture').
