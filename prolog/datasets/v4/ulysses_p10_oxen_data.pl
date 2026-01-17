% Section 1: Entities & Intervals
entity(mina_purefoy_labor, structural).
entity(catholic_maternity_doctrine, structural).
entity(medical_student_fellowship, class).
entity(oxen_linguistic_scaffold, scaffold).
entity(bloom_leopold, individual).
entity(stephen_dedalus, individual).

interval(oxen_of_the_sun_interval, 2200, 2315). % 10:00 PM to 11:15 PM

% Section 2: Events
event(e1, bloom_arrival, 2200, [location(holles_street_hospital), motive(inquiry_purefoy)]).
event(e2, medical_debate, 2230, [topics([sex_determination, infant_mortality])]).
event(e3, successful_accouchement, 2245, [patient(mina_purefoy), outcome(male_heir)]).
event(e4, departure_to_burkes, 2300, [destination(burkes_pub), state(drunken_revelry)]).
event(e5, dowie_invocation, 2310, [style(american_evangelism)]).

% Section 3: Constraint Claims & Metrics (Current State at T=2315)
% Biological reproduction claims to be an immovable Mountain (Natural Law)
constraint_claim(mina_purefoy_labor, mountain).
constraint_metric(mina_purefoy_labor, extractiveness, 0.02).
constraint_metric(mina_purefoy_labor, suppression_requirement, 0.01).
constraint_metric(mina_purefoy_labor, resistance, 0.01).

% Catholic doctrine on maternity functions as a Constructed Constraint (Noose)
constraint_claim(catholic_maternity_doctrine, noose).
constraint_metric(catholic_maternity_doctrine, extractiveness, 0.88).
constraint_metric(catholic_maternity_doctrine, suppression_requirement, 0.92).
constraint_metric(catholic_maternity_doctrine, resistance, 0.75).

% Linguistic evolution acts as a narrative Scaffold
constraint_claim(oxen_linguistic_scaffold, scaffold).
constraint_metric(oxen_linguistic_scaffold, extractiveness, 0.15).
constraint_metric(oxen_linguistic_scaffold, suppression_requirement, 0.10).
constraint_metric(oxen_linguistic_scaffold, resistance, 0.05).

% Section 4: Temporal Measurements (Triple Measurements Required)
% Mina Purefoy's Labor: Stable biological reality (Signature: Natural Law)
measurement(m1, mina_purefoy_labor, extractiveness, 2200, 0.02).
measurement(m2, mina_purefoy_labor, suppression_requirement, 2200, 0.01).
measurement(m3, mina_purefoy_labor, resistance, 2200, 0.01).

measurement(m4, mina_purefoy_labor, extractiveness, 2245, 0.02).
measurement(m5, mina_purefoy_labor, suppression_requirement, 2245, 0.01).
measurement(m6, mina_purefoy_labor, resistance, 2245, 0.01).

% Catholic Doctrine: High extractiveness/suppression throughout
measurement(m7, catholic_maternity_doctrine, extractiveness, 2200, 0.85).
measurement(m8, catholic_maternity_doctrine, suppression_requirement, 2200, 0.90).
measurement(m9, catholic_maternity_doctrine, resistance, 2200, 0.70).

measurement(m10, catholic_maternity_doctrine, extractiveness, 2300, 0.88).
measurement(m11, catholic_maternity_doctrine, suppression_requirement, 2300, 0.92).
measurement(m12, catholic_maternity_doctrine, resistance, 2300, 0.75).

% Linguistic Scaffold: Evolution of styles (Signature: Coordination Scaffold)
measurement(m13, oxen_linguistic_scaffold, extractiveness, 2200, 0.10).
measurement(m14, oxen_linguistic_scaffold, suppression_requirement, 2200, 0.05).
measurement(m15, oxen_linguistic_scaffold, resistance, 2200, 0.05).

measurement(m16, oxen_linguistic_scaffold, extractiveness, 2315, 0.20).
measurement(m17, oxen_linguistic_scaffold, suppression_requirement, 2315, 0.15).
measurement(m18, oxen_linguistic_scaffold, resistance, 2315, 0.10).

% Section 5: Viable Alternatives (Signature Detection)
% For Linguistic Scaffold: Alternatives existed (Historical choice of styles)
intent_viable_alternative(oxen_of_the_sun_interval, oxen_linguistic_scaffold, 'Standard objective realism').
intent_alternative_rejected(oxen_of_the_sun_interval, oxen_linguistic_scaffold, 'Standard objective realism').

% For Catholic Doctrine: Alternatives exist but are institutionally suppressed
intent_viable_alternative(oxen_of_the_sun_interval, catholic_maternity_doctrine, 'Secular medical ethics/Contraception').
intent_alternative_rejected(oxen_of_the_sun_interval, catholic_maternity_doctrine, 'Secular medical ethics/Contraception').

% For Biological Labor: No viable alternatives (Signature: Natural Law)
% (Section purposefully empty for mina_purefoy_labor)

% Section 6: Dependencies
affects_constraint(oxen_linguistic_scaffold, mina_purefoy_labor). % Language mirrors biological growth
affects_constraint(catholic_maternity_doctrine, stephen_dedalus). % Theological angst

% Section 7: Intent Evidence
% The Church benefits from the maintenance of procreative mandates
intent_beneficiary_class(oxen_of_the_sun_interval, catholic_ecclesiastical_hierarchy).
intent_power_change(oxen_of_the_sun_interval, catholic_ecclesiastical_hierarchy, 0.65).

% Section 8: Recommendations
recommendation(rec1, 'Deployment of secular medical scaffolds to mitigate doctrinal extractiveness').
affects_constraint(rec1, catholic_maternity_doctrine).

recommendation(rec2, 'Utilize the linguistic scaffold to bridge the transition between biological trauma and social revelry').
affects_constraint(rec2, oxen_linguistic_scaffold).

% Section 9: Omega Variables
omega_variable(om1, conceptual, 'Exact boundary between medical instruction and rowdy mockery in the commons hall').
