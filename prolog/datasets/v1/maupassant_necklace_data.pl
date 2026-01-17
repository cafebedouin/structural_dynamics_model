% =============================================================================
% v3.1 Structural Analysis Engine Data Pack: THE DIAMOND NECKLACE
% Domain: Late 19th-Century Parisian Class Hierarchy and Usury Debt Cycle
% Interval: 10-Year Debt Amortization (T0 = The Ball/Loss, Tn = Final Payment)
% =============================================================================

% --- 1. Entities & Intervals ---
entity(mathilde_loisel, individual).
entity(m_loisel, individual).
entity(jeanne_forestier, individual).
entity(ministry_of_instruction, organizational).
entity(usurer_consortium, organizational).
entity(petty_bourgeoisie_clerks, class).
entity(high_bourgeoisie, class).
entity(parisian_class_hierarchy, structural).

interval(debt_cycle_1880, 0, 10).

% --- 2. Events ---
event(ev01_the_invitation, social_trigger, 0, [host(ministry_of_instruction), target(mathilde_loisel)]).
event(ev02_the_loss, critical_failure, 1, [object(the_necklace), location(rue_des_martyrs)]).
event(ev03_the_substitution, concealment_action, 2, [cost(36000), source(loisel_inheritance)]).
event(ev04_the_usury_trap, systemic_capture, 2, [action(borrowing), interest(compound_high)]).
event(ev05_the_ten_year_grind, labor_extraction, 5, [role(domestic_labor), state(poverty)]).
event(ev06_the_revelation, ironical_resolution, 10, [truth_value(500), paid_value(36000)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The physical, biological toll of manual labor and aging in the garret.
constraint_claim(subsistence_labor_grind, mountain).
constraint_metric(subsistence_labor_grind, accessibility_collapse, 0.85).

% Noose: The social shame and code of honor that prevented Mathilde from admitting the loss.
constraint_claim(social_shame_concealment, noose).
constraint_metric(social_shame_concealment, stakes_inflation, 0.95).

% Zombie: The 'Diamond' Necklace. A dead object of 500-franc value acting as a 36,000-franc master.
constraint_claim(the_false_luxury_totem, zombie).
constraint_metric(the_false_luxury_totem, suppression, 0.90).

% Rope: The usury notes and legal obligations to the race of lenders.
constraint_claim(compound_interest_obligation, rope).
constraint_metric(compound_interest_obligation, suppression, 0.80).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Abolish usurious compound interest for small-salary earners.').
recommendation(rec02, 'Mandatory disclosure of \'paste\' vs \'precious\' status in borrowed luxury rentals.').

affects_constraint(rec01, compound_interest_obligation).
affects_constraint(rec02, social_shame_concealment).

veto_actor(usurer_consortium).
veto_actor(high_bourgeoisie).

veto_exposed(usurer_consortium, rec01).
veto_exposed(high_bourgeoisie, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R at T0 and T10) ---

% Time T0 (Initial Social Pressure / Post-Loss)
measurement(m01, mathilde_loisel, accessibility_collapse(individual), 0, 0.30).
measurement(m02, mathilde_loisel, stakes_inflation(individual), 0, 0.85).
measurement(m03, mathilde_loisel, suppression(individual), 0, 0.40).
measurement(m04, mathilde_loisel, resistance(individual), 0, 0.15).

measurement(m05, usurer_consortium, accessibility_collapse(organizational), 0, 0.05).
measurement(m06, usurer_consortium, stakes_inflation(organizational), 0, 0.10).
measurement(m07, usurer_consortium, suppression(organizational), 0, 0.00).
measurement(m08, usurer_consortium, resistance(organizational), 0, 0.00).

measurement(m09, petty_bourgeoisie_clerks, accessibility_collapse(class), 0, 0.50).
measurement(m10, petty_bourgeoisie_clerks, stakes_inflation(class), 0, 0.60).
measurement(m11, petty_bourgeoisie_clerks, suppression(class), 0, 0.70).
measurement(m12, petty_bourgeoisie_clerks, resistance(class), 0, 0.20).

measurement(m13, parisian_class_hierarchy, accessibility_collapse(structural), 0, 0.10).
measurement(m14, parisian_class_hierarchy, stakes_inflation(structural), 0, 0.20).
measurement(m15, parisian_class_hierarchy, suppression(structural), 0, 0.00).
measurement(m16, parisian_class_hierarchy, resistance(structural), 0, 0.00).

% Time T10 (End of the Debt Cycle / Pre-Revelation)
measurement(m17, mathilde_loisel, accessibility_collapse(individual), 10, 0.95).
measurement(m18, mathilde_loisel, stakes_inflation(individual), 10, 1.00).
measurement(m19, mathilde_loisel, suppression(individual), 10, 0.98).
measurement(m20, mathilde_loisel, resistance(individual), 10, 0.02).

measurement(m21, usurer_consortium, accessibility_collapse(organizational), 10, 0.00).
measurement(m22, usurer_consortium, stakes_inflation(organizational), 10, 0.40).
measurement(m23, usurer_consortium, suppression(organizational), 10, 0.00).
measurement(m24, usurer_consortium, resistance(organizational), 10, 0.00).

measurement(m25, petty_bourgeoisie_clerks, accessibility_collapse(class), 10, 0.80).
measurement(m26, petty_bourgeoisie_clerks, stakes_inflation(class), 10, 0.90).
measurement(m27, petty_bourgeoisie_clerks, suppression(class), 10, 0.85).
measurement(m28, petty_bourgeoisie_clerks, resistance(class), 10, 0.05).

measurement(m29, parisian_class_hierarchy, accessibility_collapse(structural), 10, 0.15).
measurement(m30, parisian_class_hierarchy, stakes_inflation(structural), 10, 0.35).
measurement(m31, parisian_class_hierarchy, suppression(structural), 10, 0.00).
measurement(m32, parisian_class_hierarchy, resistance(structural), 10, 0.00).

% --- 6. Intent Evidence Layer ---
% Viable Alternative: Communicating the loss to Mme. Forestier (High Success Probability, High Social Cost).
intent_viable_alternative(debt_cycle_1880, parisian_class_hierarchy, 'Radical_Transparency_Honesty').
intent_alternative_rejected(debt_cycle_1880, parisian_class_hierarchy, 'Radical_Transparency_Honesty').

% Main Beneficiary: Usurers/Lenders who extracted 10 years of human capital from a lie.
intent_beneficiary_class(debt_cycle_1880, high_bourgeoisie).
intent_power_change(debt_cycle_1880, high_bourgeoisie, 0.45).

% Loser Class: The Clerk class (petty bourgeoisie) who sacrificed inherited and potential capital.
intent_loser_class(debt_cycle_1880, petty_bourgeoisie_clerks).
intent_power_change(debt_cycle_1880, petty_bourgeoisie_clerks, -0.90).

% Structural Logic: Beneficiary class (Bourgeoisie/Lenders) faces zero suppression/resistance.
intent_suppression_level(debt_cycle_1880, high_bourgeoisie, structural, 0.0).
intent_resistance_level(debt_cycle_1880, high_bourgeoisie, structural, 0.0).

% Normative Strength: The power of the 'Social Appearance' norm.
intent_norm_strength(debt_cycle_1880, 0, 0.95).
intent_norm_strength(debt_cycle_1880, 10, 0.40). % Eroded by exhaustion, but paid.
