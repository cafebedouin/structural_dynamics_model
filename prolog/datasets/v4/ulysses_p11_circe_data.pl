% Section 1: Entities & Intervals
entity(leopold_bloom, individual).
entity(stephen_dedalus, individual).
entity(bella_cohen_regime, structural).
entity(nighttown_moral_phantasmagoria, structural).
entity(masonic_ritual_scaffold, scaffold).
entity(imperial_military_authority, organizational).
entity(stephens_guilt_complex, individual).

interval(circe_nighttown_interval, 0, 60). % Representing approximately 12:00 AM to 1:00 AM

% Section 2: Events
event(e1, bloom_arrival_nighttown, 0, [location(mabbot_street), state(panting)]).
event(e2, hallucinatory_ascension, 15, [role(lord_mayor), location(bloomusalem)]).
event(e3, gender_transformation, 30, [actor(bello), target(ruby_cohen)]).
event(e4, chandelier_destruction, 45, [actor(stephen), motivation(non_serviam)]).
event(e5, physical_assault, 55, [actor(private_carr), target(stephen)]).
event(e6, rudy_apparition, 60, [type(hallucination), observer(bloom)]).

% Section 3: Constraint Claims & Metrics (Current State at T=60)
% Nighttown claims to be a free "Carnival" (Scaffold), but functions as a Noose.
constraint_claim(nighttown_moral_phantasmagoria, noose).
constraint_metric(nighttown_moral_phantasmagoria, extractiveness, 0.95).
constraint_metric(nighttown_moral_phantasmagoria, suppression_requirement, 0.92).
constraint_metric(nighttown_moral_phantasmagoria, resistance, 0.88).
constraint_metric(nighttown_moral_phantasmagoria, snap_back_potential, 0.98).

% The Masonic/Secret codes act as a load-bearing Scaffold for Bloom's survival.
constraint_claim(masonic_ritual_scaffold, scaffold).
constraint_metric(masonic_ritual_scaffold, extractiveness, 0.10).
constraint_metric(masonic_ritual_scaffold, suppression_requirement, 0.05).
constraint_metric(masonic_ritual_scaffold, resistance, 0.05).

% Stephen's guilt (The Mother) claims to be a Mountain (Divine Law).
constraint_claim(stephens_guilt_complex, mountain).
constraint_metric(stephens_guilt_complex, extractiveness, 0.85).
constraint_metric(stephens_guilt_complex, suppression_requirement, 0.90).
constraint_metric(stephens_guilt_complex, resistance, 0.92).

% Section 4: Temporal Measurements (Evolution of the Nighttown "Capture")
% Bella/Bello Regime: Transformation from transactional Rope to sadistic Noose.
measurement(m1, bella_cohen_regime, extractiveness, 0, 0.25).
measurement(m2, bella_cohen_regime, suppression_requirement, 0, 0.20).
measurement(m3, bella_cohen_regime, resistance, 0, 0.15).

measurement(m4, bella_cohen_regime, extractiveness, 30, 0.85).
measurement(m5, bella_cohen_regime, suppression_requirement, 30, 0.90).
measurement(m6, bella_cohen_regime, resistance, 30, 0.80).

measurement(m7, bella_cohen_regime, extractiveness, 60, 0.70). % Slight drop after Bloom regains control.
measurement(m8, bella_cohen_regime, suppression_requirement, 60, 0.60).
measurement(m9, bella_cohen_regime, resistance, 60, 0.50).

% Imperial Authority: High suppression throughout (Signature: Constructed Constraint).
measurement(m10, imperial_military_authority, extractiveness, 0, 0.80).
measurement(m11, imperial_military_authority, suppression_requirement, 0, 0.85).
measurement(m12, imperial_military_authority, resistance, 0, 0.75).

measurement(m13, imperial_military_authority, extractiveness, 60, 0.95).
measurement(m14, imperial_military_authority, suppression_requirement, 60, 0.95).
measurement(m15, imperial_military_authority, resistance, 60, 0.90).

% Section 5: Viable Alternatives (Signature Detection)
% Masonic Ritual: Alternatives existed (Standard social interaction).
intent_viable_alternative(circe_nighttown_interval, masonic_ritual_scaffold, 'Overt legal identification').
intent_alternative_rejected(circe_nighttown_interval, masonic_ritual_scaffold, 'Overt legal identification').

% Guilt Complex: Claims no alternatives (Impossibility/Divine Law).
% System will detect False Mountain due to high extractiveness.

% Section 6: Dependencies
affects_constraint(nighttown_moral_phantasmagoria, leopold_bloom).
affects_constraint(stephens_guilt_complex, stephen_dedalus).
affects_constraint(masonic_ritual_scaffold, nighttown_moral_phantasmagoria). % Use code to bypass nightmare logic.

% Section 7: Intent Evidence
% Nighttown owners/Bawds benefit from the economic and psychic extraction.
intent_beneficiary_class(circe_nighttown_interval, nighttown_vices_industry).
intent_power_change(circe_nighttown_interval, nighttown_vices_industry, 0.75).

% Imperial authority extracts obedience through violence (Private Carr).
intent_beneficiary_class(circe_nighttown_interval, british_imperial_administration).
intent_power_change(circe_nighttown_interval, british_imperial_administration, 0.85).

% Section 8: Recommendations
recommendation(rec1, 'Sever the hallucinatory capture of Nighttown via reality-grounded coordination').
affects_constraint(rec1, nighttown_moral_phantasmagoria).

recommendation(rec2, 'Utilize the Masonic/Secret scaffold to navigate constructed legal/military nooses').
affects_constraint(rec2, masonic_ritual_scaffold).

% Section 9: Omega Variables
omega_variable(om1, conceptual, 'Exact boundary between internal psychic projection and external social coercion in Nighttown').
