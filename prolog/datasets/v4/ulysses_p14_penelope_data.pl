% Section 1: Entities & Intervals
entity(leopold_bloom, individual).
entity(marion_bloom, individual).
entity(blazes_boylan, individual).
entity(menstrual_cycle_biological, structural).
entity(domestic_monogamy_construct, structural).
entity(howth_affirmation_memory, scaffold).
entity(stream_of_consciousness, scaffold).

interval(penelope_interval, 0200, 0400). % Approximate hours from 2:00 AM to 4:00 AM

% Section 2: Events
event(e1, boylan_affair_recap, 0215, [intensity(high), modality(physical)]).
event(e2, menstruation_onset, 0300, [trigger(physical_discomfort), consequence(linen_soiling)]).
event(e3, howth_recollection, 0345, [context(proposal), location(howth_head)]).
event(e4, final_affirmation, 0400, [word('Yes')]).

% Section 3: Constraint Claims & Metrics (Current State at T=0400)
% Menstruation claims to be and IS an immovable Mountain (Natural Law)
constraint_claim(menstrual_cycle_biological, mountain).
constraint_metric(menstrual_cycle_biological, extractiveness, 0.02).
constraint_metric(menstrual_cycle_biological, suppression_requirement, 0.01).
constraint_metric(menstrual_cycle_biological, resistance, 0.01).

% Domestic Monogamy functions as a Tangled Rope/Noose
constraint_claim(domestic_monogamy_construct, tangled_rope).
constraint_metric(domestic_monogamy_construct, extractiveness, 0.72).
constraint_metric(domestic_monogamy_construct, suppression_requirement, 0.68).
constraint_metric(domestic_monogamy_construct, resistance, 0.75).

% The memory of Howth acts as a load-bearing Coordination Scaffold
constraint_claim(howth_affirmation_memory, scaffold).
constraint_metric(howth_affirmation_memory, extractiveness, 0.15).
constraint_metric(howth_affirmation_memory, suppression_requirement, 0.10).
constraint_metric(howth_affirmation_memory, resistance, 0.05).

% Section 4: Temporal Measurements (Triple Measurements Required)
% Menstrual Cycle: Stable biological reality (Signature: Natural Law)
measurement(m1, menstrual_cycle_biological, extractiveness, 0200, 0.02).
measurement(m2, menstrual_cycle_biological, suppression_requirement, 0200, 0.01).
measurement(m3, menstrual_cycle_biological, resistance, 0200, 0.01).

measurement(m4, menstrual_cycle_biological, extractiveness, 0400, 0.02).
measurement(m5, menstrual_cycle_biological, suppression_requirement, 0400, 0.01).
measurement(m6, menstrual_cycle_biological, resistance, 0400, 0.01).

% Domestic Construct: Fluctuating extractiveness (Signature: Constructed Constraint)
measurement(m7, domestic_monogamy_construct, extractiveness, 0200, 0.85). % Peak resentment
measurement(m8, domestic_monogamy_construct, suppression_requirement, 0200, 0.80).
measurement(m9, domestic_monogamy_construct, resistance, 0200, 0.70).

measurement(m10, domestic_monogamy_construct, extractiveness, 0400, 0.72). % Moderated by memory
measurement(m11, domestic_monogamy_construct, suppression_requirement, 0400, 0.68).
measurement(m12, domestic_monogamy_construct, resistance, 0400, 0.75).

% Howth Memory: Escalating value (Signature: Coordination Scaffold)
measurement(m13, howth_affirmation_memory, extractiveness, 0200, 0.40).
measurement(m14, howth_affirmation_memory, suppression_requirement, 0200, 0.30).
measurement(m15, howth_affirmation_memory, resistance, 0200, 0.20).

measurement(m16, howth_affirmation_memory, extractiveness, 0400, 0.15).
measurement(m17, howth_affirmation_memory, suppression_requirement, 0400, 0.10).
measurement(m18, howth_affirmation_memory, resistance, 0400, 0.05).

% Section 5: Viable Alternatives (Signature Detection)
% For Howth memory: Rejection was a viable alternative (Choice existed)
intent_viable_alternative(penelope_interval, howth_affirmation_memory, 'Refusal of Bloom proposal/Marriage to another').
intent_alternative_rejected(penelope_interval, howth_affirmation_memory, 'Refusal of Bloom proposal/Marriage to another').

% For Monogamy: Adultery/Affair is the viable alternative currently active
intent_viable_alternative(penelope_interval, domestic_monogamy_construct, 'Open sexual bypass/Boylan').
intent_alternative_rejected(penelope_interval, domestic_monogamy_construct, 'Open sexual bypass/Boylan').

% Section 6: Dependencies
affects_constraint(howth_affirmation_memory, domestic_monogamy_construct). % Stabilizes the noose
affects_constraint(menstrual_cycle_biological, marion_bloom). % Physical driver of stream

% Section 7: Intent Evidence
% Boylan benefits from the extraction of sexual energy/pleasure
intent_beneficiary_class(penelope_interval, adultery_participants).
intent_power_change(penelope_interval, adultery_participants, 0.85).

% Bloom benefits from the eventual "Yes" scaffold
intent_beneficiary_class(penelope_interval, marriage_preservation).
intent_power_change(penelope_interval, marriage_preservation, 0.60).

% Section 8: Recommendations
recommendation(rec1, 'Accept the biological mountain of time and cycles as the base layer of experience').
affects_constraint(rec1, menstrual_cycle_biological).

recommendation(rec2, 'Deploy the Howth Coordination Scaffold to mitigate the terminal extractiveness of the domestic noose').
affects_constraint(rec2, howth_affirmation_memory).

% Section 9: Omega Variables
omega_variable(om1, conceptual, 'Exact boundary between involuntary thought and voluntary affirmation in the final "Yes"').
