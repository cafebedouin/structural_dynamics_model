% Section 1: Entities & Intervals
entity(leopold_bloom, individual).
entity(stephen_dedalus, individual).
entity(marion_bloom, individual).
entity(celestial_mechanics, structural).
entity(dublin_water_utility, structural).
entity(domestic_connubiality, structural).
entity(catechetical_prose, scaffold).
entity(equanimity_logic, scaffold).
entity(milly_bloom, individual).

% The "Ithaca" interval covering the return to Eccles St and Bloom's sleep.
interval(ithaca_interval, 0200, 0330).

% Section 2: Events
event(e1, railing_stratagem, 0205, [action(climbing_over_railings), catalyst(missing_key)]).
event(e2, water_boiling, 0215, [action(ebullition), thermal_units(72)]).
event(e3, cocoa_collation, 0230, [substance(epps_soluble_cocoa), recipient(stephen)]).
event(e4, heaventree_observation, 0245, [focus(astronomy), perspective(scientific_artistic)]).
event(e5, valedictory_departure, 0300, [action(stephen_exodus), sound(st_george_bells)]).
event(e6, bed_entry, 0315, [action(encountering_imprint), state(abnegation)]).

% Section 3: Constraint Claims & Metrics
% Celestial Mechanics claim to be a Mountain (Natural Law)
constraint_claim(celestial_mechanics, mountain).
constraint_metric(celestial_mechanics, extractiveness, 0.01).
constraint_metric(celestial_mechanics, suppression_requirement, 0.01).
constraint_metric(celestial_mechanics, resistance, 0.01).

% Catechetical Prose acts as a narrative Scaffold (replaces standard prose)
constraint_claim(catechetical_prose, scaffold).
constraint_metric(catechetical_prose, extractiveness, 0.15).
constraint_metric(catechetical_prose, suppression_requirement, 0.10).
constraint_metric(catechetical_prose, resistance, 0.05).

% Domestic Connubiality (Adultery) functions as a Snare in Bloom's perception
% Bloom claims it is a "Tangled Rope" (maintained by equanimity)
constraint_claim(domestic_connubiality, tangled_rope).
constraint_metric(domestic_connubiality, extractiveness, 0.88).
constraint_metric(domestic_connubiality, suppression_requirement, 0.75).
constraint_metric(domestic_connubiality, resistance, 0.90).
constraint_metric(domestic_connubiality, snap_back_potential, 0.95).

% Section 4: Temporal Measurements
% Dublin Water Utility: Transition from reservoir to domestic tap (Constructed/Coordination)
measurement(m1, dublin_water_utility, extractiveness, 0210, 0.20).
measurement(m2, dublin_water_utility, suppression_requirement, 0210, 0.15).
measurement(m3, dublin_water_utility, resistance, 0210, 0.10).

measurement(m4, dublin_water_utility, extractiveness, 0215, 0.35). % Scarcity (summer drouth)
measurement(m5, dublin_water_utility, suppression_requirement, 0215, 0.60). % Prohibitions enforced
measurement(m6, dublin_water_utility, resistance, 0215, 0.40).

% Equanimity Logic: Bloom's mental scaffold to manage adultery capture
measurement(m7, equanimity_logic, extractiveness, 0310, 0.10).
measurement(m8, equanimity_logic, suppression_requirement, 0310, 0.05).
measurement(m9, equanimity_logic, resistance, 0310, 0.05).

measurement(m10, equanimity_logic, extractiveness, 0320, 0.45). % Stress of the "imprint"
measurement(m11, equanimity_logic, suppression_requirement, 0320, 0.30).
measurement(m12, equanimity_logic, resistance, 0320, 0.15).

% Section 5: Viable Alternatives (Signature Detection)
% Catechetical Prose had alternatives (Historical/Narrative choice)
intent_viable_alternative(ithaca_interval, catechetical_prose, 'Standard Objective Narrator').
intent_alternative_rejected(ithaca_interval, catechetical_prose, 'Standard Objective Narrator').

% Equanimity Logic: Alternative was violent retribution or divorce
intent_viable_alternative(ithaca_interval, equanimity_logic, 'Assassination/Retribution').
intent_alternative_rejected(ithaca_interval, equanimity_logic, 'Assassination/Retribution').
intent_viable_alternative(ithaca_interval, equanimity_logic, 'Divorce/Legal Suit').
intent_alternative_rejected(ithaca_interval, equanimity_logic, 'Divorce/Legal Suit').

% Section 6: Dependencies
affects_constraint(equanimity_logic, domestic_connubiality). % Prevents systemic collapse
affects_constraint(celestial_mechanics, catechetical_prose). % Determines the pace/order
affects_constraint(dublin_water_utility, leopold_bloom). % Determines hydration/shaving potential

% Section 7: Intent Evidence
% The Waterworks committee benefits from usage prohibition
intent_beneficiary_class(ithaca_interval, municipal_administration).
intent_power_change(ithaca_interval, municipal_administration, 0.45).

% Bloom benefits from Equanimity by avoiding the "cataclysmic annihilation"
intent_beneficiary_class(ithaca_interval, bloom_psychic_stability).
intent_power_change(ithaca_interval, bloom_psychic_stability, 0.80).

% Section 8: Recommendations
recommendation(rec1, 'Accept the Mountain of celestial mechanics as the terminal boundary of individual experience').
affects_constraint(rec1, celestial_mechanics).

recommendation(rec2, 'Maintain the Equanimity Scaffold to mitigate the extractiveness of the domestic snare').
affects_constraint(rec2, equanimity_logic).

% Section 9: Omega Variables
omega_variable(om1, conceptual, 'Identity of M’Intosh (Underspecified)').
omega_variable(om2, empirical, 'Exact coordinate of the departer vs departer-remainer').
