% Section 1: Entities & Intervals
entity(gerty_macdowell, individual).
entity(leopold_bloom, individual).
entity(cissy_caffrey, individual).
entity(edy_boardman, individual).
entity(catholic_temperance_retreat, organizational).
entity(gerty_fantasy_scaffold, scaffold).
entity(patriarchal_gender_norms, structural).
entity(gerty_lameness, structural).
entity(novelette_consumer_culture, structural).

interval(nausicaa_evening, 2000, 2100). % 8:00 PM to 9:00 PM

% Section 2: Events
event(e1, strand_social_gathering, 2000, [actors([gerty, cissy, edy]), location(sandymount_strand)]).
event(e2, retreat_benediction, 2015, [source(church), ritual(rosary)]).
event(e3, visual_exchange_onset, 2035, [subject(bloom), object(gerty)]).
event(e4, bazaar_fireworks, 2045, [visual_stimulus(roman_candle), trigger(sexual_climax)]).
event(e5, revelation_of_defect, 2100, [action(limping), observer(bloom)]).

% Section 3: Constraint Claims & Metrics (Current State at T=2100)
% Gerty's romantic fantasy claims to be a protective Scaffold
constraint_claim(gerty_fantasy_scaffold, scaffold).
constraint_metric(gerty_fantasy_scaffold, extractiveness, 0.68).
constraint_metric(gerty_fantasy_scaffold, suppression_requirement, 0.55).
constraint_metric(gerty_fantasy_scaffold, resistance, 0.40).
constraint_metric(gerty_fantasy_scaffold, snap_back_potential, 0.75).

% Gender norms claim to be an immovable Mountain (Natural Law)
constraint_claim(patriarchal_gender_norms, mountain).
constraint_metric(patriarchal_gender_norms, extractiveness, 0.78).
constraint_metric(patriarchal_gender_norms, suppression_requirement, 0.70).
constraint_metric(patriarchal_gender_norms, resistance, 0.60).

% Gerty's lameness is a True Mountain (Natural Law - physical reality)
constraint_claim(gerty_lameness, mountain).
constraint_metric(gerty_lameness, extractiveness, 0.02).
constraint_metric(gerty_lameness, suppression_requirement, 0.01).
constraint_metric(gerty_lameness, resistance, 0.01).

% Section 4: Temporal Measurements (Triple Measurements Required)
% Gerty's Fantasy: Progressively calcifying from Scaffold toward Noose
measurement(m1, gerty_fantasy_scaffold, extractiveness, 2000, 0.25).
measurement(m2, gerty_fantasy_scaffold, suppression_requirement, 2000, 0.15).
measurement(m3, gerty_fantasy_scaffold, resistance, 2000, 0.10).

measurement(m4, gerty_fantasy_scaffold, extractiveness, 2045, 0.55).
measurement(m5, gerty_fantasy_scaffold, suppression_requirement, 2045, 0.35).
measurement(m6, gerty_fantasy_scaffold, resistance, 2045, 0.25).

measurement(m7, gerty_fantasy_scaffold, extractiveness, 2100, 0.68).
measurement(m8, gerty_fantasy_scaffold, suppression_requirement, 2100, 0.55).
measurement(m9, gerty_fantasy_scaffold, resistance, 2100, 0.40).

% Gender Norms: Highly extractive and suppressive throughout
measurement(m10, patriarchal_gender_norms, extractiveness, 2000, 0.75).
measurement(m11, patriarchal_gender_norms, suppression_requirement, 2000, 0.65).
measurement(m12, patriarchal_gender_norms, resistance, 2000, 0.50).

measurement(m13, patriarchal_gender_norms, extractiveness, 2100, 0.78).
measurement(m14, patriarchal_gender_norms, suppression_requirement, 2100, 0.70).
measurement(m15, patriarchal_gender_norms, resistance, 2100, 0.60).

% Gerty's Lameness: Stable, inherent impossibility (Signature: Natural Law)
measurement(m16, gerty_lameness, extractiveness, 2000, 0.02).
measurement(m17, gerty_lameness, suppression_requirement, 2000, 0.01).
measurement(m18, gerty_lameness, resistance, 2000, 0.01).

measurement(m19, gerty_lameness, extractiveness, 2100, 0.02).
measurement(m20, gerty_lameness, suppression_requirement, 2100, 0.01).
measurement(m21, gerty_lameness, resistance, 2100, 0.01).

% Section 5: Viable Alternatives (Signature Detection)
% Gerty's fantasy had viable alternatives (Signature: Coordination/Constructed)
intent_viable_alternative(nausicaa_evening, gerty_fantasy_scaffold, 'Realistic socio-economic identity and medical acceptance').
intent_alternative_rejected(nausicaa_evening, gerty_fantasy_scaffold, 'Realistic socio-economic identity and medical acceptance').

% Gender Norms: Alternatives exist but are suppressed
intent_viable_alternative(nausicaa_evening, patriarchal_gender_norms, 'Feminine autonomy outside the marriage market').
intent_alternative_rejected(nausicaa_evening, patriarchal_gender_norms, 'Feminine autonomy outside the marriage market').

% Gerty's Lameness: No alternatives (Signature: Natural Law)
% (Section purposefully empty for lameness)

% Section 6: Dependencies
affects_constraint(patriarchal_gender_norms, gerty_fantasy_scaffold).
affects_constraint(novelette_consumer_culture, gerty_fantasy_scaffold).
affects_constraint(gerty_lameness, gerty_fantasy_scaffold). % Drives the need for the fantasy support

% Section 7: Intent Evidence
% Consumer culture and Novelettes benefit from Gerty's aspirational capture
intent_beneficiary_class(nausicaa_evening, publishing_advertising_industry).
intent_power_change(nausicaa_evening, publishing_advertising_industry, 0.45).

% Patriarchal order benefits from the maintenance of domestic ideals
intent_beneficiary_class(nausicaa_evening, traditional_marriage_structure).
intent_power_change(nausicaa_evening, traditional_marriage_structure, 0.60).

% Section 8: Recommendations
recommendation(rec1, 'Sever the dependency on novelette_consumer_culture to prevent scaffold calcification into a Noose').
affects_constraint(rec1, gerty_fantasy_scaffold).

recommendation(rec2, 'Structural deconstruction of false mountains (gender norms) to reduce extractiveness from the individual').
affects_constraint(rec2, patriarchal_gender_norms).

% Section 9: Omega Variables
omega_variable(om1, conceptual, 'Exact degree of Bloom self-awareness regarding his own constructed constraints vs Gerty fantasy').
