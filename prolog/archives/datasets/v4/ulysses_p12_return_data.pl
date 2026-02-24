% Section 1: Entities & Intervals
entity(leopold_bloom, individual).
entity(stephen_dedalus, individual).
entity(lord_john_corley, individual).
entity(db_murphy_sailor, individual).
entity(skin_the_goat_shelter, structural).
entity(parnell_mythology, structural).
entity(samaritan_support_scaffold, scaffold).
entity(economic_despair_corley, snare).

interval(eumaeus_interval, 0100, 0200). % 1:00 AM to 2:00 AM (approximate)

% Section 2: Events
event(e1, physical_brushing, 0105, [actor(bloom), target(stephen), action(removing_shavings)]).
event(e2, corley_extraction, 0115, [actor(corley), target(stephen), amount(half_crown)]).
event(e3, shelter_entry, 0125, [location(butt_bridge_shelter)]).
event(e4, sailor_narrative_onset, 0135, [actor(db_murphy), topic(circumnavigation)]).
event(e5, parnell_debate, 0150, [topic(return_of_the_leader), focus(myth_vs_reality)]).
event(e6, departure_arm_in_arm, 0200, [actors([bloom, stephen]), direction(gardiner_street)]).

% Section 3: Constraint Claims & Metrics (Current State at T=0200)
% Parnell's Ghost/Mythology claims to be an immovable Mountain (National Necessity)
constraint_claim(parnell_mythology, mountain).
constraint_metric(parnell_mythology, extractiveness, 0.75).
constraint_metric(parnell_mythology, suppression_requirement, 0.70).
constraint_metric(parnell_mythology, resistance, 0.65).
constraint_metric(parnell_mythology, snap_back_potential, 0.80).

% Bloom's Samaritan support acts as a protective Scaffold for Stephen
constraint_claim(samaritan_support_scaffold, scaffold).
constraint_metric(samaritan_support_scaffold, extractiveness, 0.12).
constraint_metric(samaritan_support_scaffold, suppression_requirement, 0.08).
constraint_metric(samaritan_support_scaffold, resistance, 0.05).

% Corley's situation is a terminal Snare (Economic Despair)
constraint_claim(economic_despair_corley, snare).
constraint_metric(economic_despair_corley, extractiveness, 0.95).
constraint_metric(economic_despair_corley, suppression_requirement, 0.85).
constraint_metric(economic_despair_corley, resistance, 0.90).

% Section 4: Temporal Measurements (Triple Measurements Required)
% Samaritan Scaffold: Stabilizing Stephen's transition from drunken collapse
measurement(m1, samaritan_support_scaffold, extractiveness, 0100, 0.35).
measurement(m2, samaritan_support_scaffold, suppression_requirement, 0100, 0.30).
measurement(m3, samaritan_support_scaffold, resistance, 0100, 0.25).

measurement(m4, samaritan_support_scaffold, extractiveness, 0200, 0.12).
measurement(m5, samaritan_support_scaffold, suppression_requirement, 0200, 0.08).
measurement(m6, samaritan_support_scaffold, resistance, 0200, 0.05).

% Parnell Mythology: Persistently high extraction (Constructed Constraint)
measurement(m7, parnell_mythology, extractiveness, 0100, 0.70).
measurement(m8, parnell_mythology, suppression_requirement, 0100, 0.65).
measurement(m9, parnell_mythology, resistance, 0100, 0.60).

measurement(m10, parnell_mythology, extractiveness, 0200, 0.75).
measurement(m11, parnell_mythology, suppression_requirement, 0200, 0.70).
measurement(m12, parnell_mythology, resistance, 0200, 0.65).

% Sailor's Tale: Functioning as a Coordination Scaffold for the group
measurement(m13, db_murphy_sailor, extractiveness, 0135, 0.15).
measurement(m14, db_murphy_sailor, suppression_requirement, 0135, 0.10).
measurement(m15, db_murphy_sailor, resistance, 0135, 0.10).

measurement(m16, db_murphy_sailor, extractiveness, 0155, 0.25). % Increasing suspicion
measurement(m17, db_murphy_sailor, suppression_requirement, 0155, 0.15).
measurement(m18, db_murphy_sailor, resistance, 0155, 0.20).

% Section 5: Viable Alternatives (Signature Detection)
% For Samaritan Scaffold: Alternatives existed (Leaving Stephen in Nighttown)
intent_viable_alternative(eumaeus_interval, samaritan_support_scaffold, 'Leaving Stephen to the Nightwatch/Police').
intent_alternative_rejected(eumaeus_interval, samaritan_support_scaffold, 'Leaving Stephen to the Nightwatch/Police').

% For Sailor's Narrative: Alternatives existed (Silence or actual news)
intent_viable_alternative(eumaeus_interval, db_murphy_sailor, 'Reading the actual news in silence').
intent_alternative_rejected(eumaeus_interval, db_murphy_sailor, 'Reading the actual news in silence').

% Section 6: Dependencies
affects_constraint(samaritan_support_scaffold, stephen_dedalus). % Vital support for mobility
affects_constraint(parnell_mythology, economic_despair_corley). % Failed political promises lead to destitution
affects_constraint(db_murphy_sailor, parnell_mythology). % Myth of return mirrors the "Enoch Arden" sailor archetype

% Section 7: Intent Evidence
% The Irish political classes benefit from the maintenance of the Parnell Myth
intent_beneficiary_class(eumaeus_interval, irish_parliamentary_party).
intent_power_change(eumaeus_interval, irish_parliamentary_party, 0.65).

% Bloom benefits from the Scaffold by establishing a paternal role
intent_beneficiary_class(eumaeus_interval, bloom_ego_stability).
intent_power_change(eumaeus_interval, bloom_ego_stability, 0.40).

% Section 8: Recommendations
recommendation(rec1, 'Maintain the Samaritan Scaffold to bridge Stephen to a safe physical environment').
affects_constraint(rec1, samaritan_support_scaffold).

recommendation(rec2, 'Sever the extractiveness of the Parnell Myth via rational skepticism').
affects_constraint(rec2, parnell_mythology).

% Section 9: Omega Variables
omega_variable(om1, empirical, 'Identity of the Sailor (Actual A.B.S. or deceptive drifter)').
omega_variable(om2, conceptual, 'Exact degree of Stephen recognition of the Samaritan intent').
