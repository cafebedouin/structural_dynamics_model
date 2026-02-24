% Section 1: Entities & Intervals
entity(viceregal_cavalcade, organizational).
entity(jesuit_orthodoxy, organizational).
entity(economic_scarcity_dublin, structural).
entity(death_entropy, individual).
entity(dilly_french_primer, scaffold).
entity(social_etiquette_maginni, class).

interval(wandering_rocks, 1455, 1600). % Time in 24h format as integers

% Section 2: Events
event(e1, viceregal_progression, 1545, [actor(earl_of_dudley), location(dublin_quays)]).
event(e2, alms_refusal, 1500, [actor(father_conmee), target(one_legged_sailor)]).
event(e3, scaffold_acquisition, 1540, [actor(dilly_dedalus), item(french_primer)]).

% Section 3: Constraint Claims & Metrics (Current State at T=1600)
% The British Imperial presence claims to be an immovable Mountain
constraint_claim(viceregal_cavalcade, mountain).
constraint_metric(viceregal_cavalcade, extractiveness, 0.82).
constraint_metric(viceregal_cavalcade, suppression_requirement, 0.85).
constraint_metric(viceregal_cavalcade, resistance, 0.55).

% The Church structure acts as a load-bearing Rope/Tangled Rope
constraint_claim(jesuit_orthodoxy, rope).
constraint_metric(jesuit_orthodoxy, extractiveness, 0.45).
constraint_metric(jesuit_orthodoxy, suppression_requirement, 0.40).
constraint_metric(jesuit_orthodoxy, resistance, 0.30).

% Death is a True Mountain (Natural Law)
constraint_claim(death_entropy, mountain).
constraint_metric(death_entropy, extractiveness, 0.02).
constraint_metric(death_entropy, suppression_requirement, 0.01).
constraint_metric(death_entropy, resistance, 0.01).

% Section 4: Temporal Measurements (Evolution over the Episode)
% Viceregal Cavalcade: High extractiveness/suppression throughout
measurement(m1, viceregal_cavalcade, extractiveness, 1500, 0.80).
measurement(m2, viceregal_cavalcade, suppression_requirement, 1500, 0.80).
measurement(m3, viceregal_cavalcade, resistance, 1500, 0.50).

measurement(m4, viceregal_cavalcade, extractiveness, 1600, 0.82).
measurement(m5, viceregal_cavalcade, suppression_requirement, 1600, 0.85).
measurement(m6, viceregal_cavalcade, resistance, 1600, 0.55).

% Economic Scarcity: Calcifying into a Snare for the Dedalus family
measurement(m7, economic_scarcity_dublin, extractiveness, 1500, 0.65).
measurement(m8, economic_scarcity_dublin, suppression_requirement, 1500, 0.60).
measurement(m9, economic_scarcity_dublin, resistance, 1500, 0.40).

measurement(m10, economic_scarcity_dublin, extractiveness, 1600, 0.88).
measurement(m11, economic_scarcity_dublin, suppression_requirement, 1600, 0.85).
measurement(m12, economic_scarcity_dublin, resistance, 1600, 0.70).

% Death Entropy: Perfectly stable (Signature: Natural Law)
measurement(m13, death_entropy, extractiveness, 1500, 0.02).
measurement(m14, death_entropy, suppression_requirement, 1500, 0.01).
measurement(m15, death_entropy, resistance, 1500, 0.01).

measurement(m16, death_entropy, extractiveness, 1600, 0.02).
measurement(m17, death_entropy, suppression_requirement, 1600, 0.01).
measurement(m18, death_entropy, resistance, 1600, 0.01).

% Section 5: Viable Alternatives (For Signature Detection)
% Social Etiquette/Maginni is a Coordination Scaffold (Alternatives existed)
intent_viable_alternative(wandering_rocks, social_etiquette_maginni, 'Informal/Proletarian social navigation').
intent_alternative_rejected(wandering_rocks, social_etiquette_maginni, 'Informal/Proletarian social navigation').

% Viceregal Cavalcade is a Constructed Constraint (Alternatives exist but are suppressed)
intent_viable_alternative(wandering_rocks, viceregal_cavalcade, 'Irish Home Rule/Republic').
intent_alternative_rejected(wandering_rocks, viceregal_cavalcade, 'Irish Home Rule/Republic').

% Section 6: Dependencies
affects_constraint(viceregal_cavalcade, economic_scarcity_dublin).
affects_constraint(economic_scarcity_dublin, jesuit_orthodoxy).
affects_constraint(dilly_french_primer, economic_scarcity_dublin). % Scaffold for transition

% Section 7: Intent Evidence
% Imperial power benefits the British Crown/Administrative class
intent_beneficiary_class(wandering_rocks, british_crown).
intent_power_change(wandering_rocks, british_crown, 0.80).

% Economic scarcity extracts from the Proletarian/Dedalus class
intent_beneficiary_class(wandering_rocks, landlord_class).
intent_power_change(wandering_rocks, landlord_class, 0.40).

% Section 8: Recommendations
recommendation(rec1, 'Deployment of educational scaffolds to mitigate economic snare calcification').
affects_constraint(rec1, dilly_french_primer).

recommendation(rec2, 'Structural severing of the viceregal snare via non-obsequious resistance').
affects_constraint(rec2, viceregal_cavalcade).

% Section 9: Omega Variables
omega_variable(om1, empirical, 'Exact degree of Father Conmee intentionality in sustaining poverty vs individual charity').
