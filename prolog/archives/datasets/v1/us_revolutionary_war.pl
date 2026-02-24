% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: U.S. Revolutionary War
% Domain: Geopolitical Conflict - American War of Independence (1765-1783)
% ==========================================================

% --- 1. Entities & Intervals ---
entity(george_washington, individual).
entity(continental_congress, organizational).
entity(patriot_merchant_class, class).
entity(loyalist_class, class).
entity(british_imperial_structure, structural).
entity(mercantile_system, structural).

interval(us_revolution_cycle, 1765, 1783).

% --- 2. Events ---
event(ev01_stamp_act_crisis, theoretical_initiation, 1765, [actor(british_parliament), reaction(sons_of_liberty), logic(no_taxation_without_rep)]).
event(ev02_coercive_acts_imposition, kinetic_initiation, 1774, [actor(british_crown), target(massachusetts), effect(port_closure)]).
event(ev03_declaration_of_independence, systemic_overhaul, 1776, [actor(continental_congress), property(sovereignty_claim)]).
event(ev04_treaty_of_paris, final_verification, 1783, [result(independence), target(british_imperial_structure)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: Atlantic Logistical Distance. The absolute physical limit of troop and supply transit across 3000 miles of ocean.
constraint_claim(atlantic_logistical_limit, mountain).
constraint_metric(atlantic_logistical_limit, accessibility_collapse, 0.98).

% Snare: The Coercive Acts (Intolerable Acts). A narrowing set of legal and economic restrictions intended to crush colonial agency.
constraint_claim(coercive_acts_noose, snare).
constraint_metric(coercive_acts_noose, stakes_inflation, 0.95).

% Piton: Virtual Representation. The dead concept that colonists were represented in Parliament 'in spirit,' used to maintain regulatory control.
constraint_claim(virtual_representation_myth, piton).
constraint_metric(virtual_representation_myth, suppression, 0.85).

% Rope: Proclamation of 1763. Ties colonial expansion directly to the Appalachian boundary, restricting geographical agency.
constraint_claim(proclamation_line_binding, rope).
constraint_metric(proclamation_line_binding, suppression, 0.75).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt Imperial Federalism with direct colonial seats in Parliament to bypass representation conflicts.').
recommendation(rec02, 'Implement localized manufacturing autonomy to reduce mercantile system dependency.').

affects_constraint(rec01, virtual_representation_myth).
affects_constraint(rec02, mercantile_system).

veto_actor(british_imperial_structure).
veto_actor(george_washington).

veto_exposed(british_imperial_structure, rec01).
veto_exposed(british_imperial_structure, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (1765: Pre-Revolutionary Stability / Imperial Dominance)
measurement(m01, george_washington, accessibility_collapse(individual), 1765, 0.15).
measurement(m02, george_washington, stakes_inflation(individual), 1765, 0.20).
measurement(m03, george_washington, suppression(individual), 1765, 0.10).
measurement(m04, george_washington, resistance(individual), 1765, 0.05).

measurement(m05, continental_congress, accessibility_collapse(organizational), 1765, 0.80). % Not yet formed/functioning
measurement(m06, continental_congress, stakes_inflation(organizational), 1765, 0.10).
measurement(m07, continental_congress, suppression(organizational), 1765, 0.90).
measurement(m08, continental_congress, resistance(organizational), 1765, 0.00).

measurement(m09, patriot_merchant_class, accessibility_collapse(class), 1765, 0.10).
measurement(m10, patriot_merchant_class, stakes_inflation(class), 1765, 0.30).
measurement(m11, patriot_merchant_class, suppression(class), 1765, 0.20).
measurement(m12, patriot_merchant_class, resistance(class), 1765, 0.15).

measurement(m13, british_imperial_structure, accessibility_collapse(structural), 1765, 0.00).
measurement(m14, british_imperial_structure, stakes_inflation(structural), 1765, 0.05).
measurement(m15, british_imperial_structure, suppression(structural), 1765, 0.00). % Beneficiary Logic
measurement(m16, british_imperial_structure, resistance(structural), 1765, 0.00). % Beneficiary Logic

% Time Tn (1783: Post-Revolutionary Independence)
measurement(m17, george_washington, accessibility_collapse(individual), 1783, 0.05).
measurement(m18, george_washington, stakes_inflation(individual), 1783, 1.00). % Ultimate life/death stakes achieved
measurement(m19, george_washington, suppression(individual), 1783, 0.05).
measurement(m20, george_washington, resistance(individual), 1783, 0.98).

measurement(m21, continental_congress, accessibility_collapse(organizational), 1783, 0.10).
measurement(m22, continental_congress, stakes_inflation(organizational), 1783, 0.95).
measurement(m23, continental_congress, suppression(organizational), 1783, 0.05).
measurement(m24, continental_congress, resistance(organizational), 1783, 0.98).

measurement(m25, patriot_merchant_class, accessibility_collapse(class), 1783, 0.05).
measurement(m26, patriot_merchant_class, stakes_inflation(class), 1783, 0.90).
measurement(m27, patriot_merchant_class, suppression(class), 1783, 0.00). % Emergent Beneficiary
measurement(m28, patriot_merchant_class, resistance(class), 1783, 0.10).

measurement(m29, british_imperial_structure, accessibility_collapse(structural), 1783, 0.95). % Structural expulsion from domain
measurement(m30, british_imperial_structure, stakes_inflation(structural), 1783, 0.80).
measurement(m31, british_imperial_structure, suppression(structural), 1783, 1.00).
measurement(m32, british_imperial_structure, resistance(structural), 1783, 0.05).

% --- 6. Intent Evidence ---
intent_viable_alternative(us_revolution_cycle, mercantile_system, 'Imperial_Federalism_Direct_Seats').
intent_alternative_rejected(us_revolution_cycle, mercantile_system, 'Imperial_Federalism_Direct_Seats').

intent_beneficiary_class(us_revolution_cycle, patriot_merchant_class).
intent_power_change(us_revolution_cycle, patriot_merchant_class, 0.95). % Capture of domestic market/political power

intent_loser_class(us_revolution_cycle, british_imperial_structure).
intent_power_change(us_revolution_cycle, british_imperial_structure, -0.90). % Total loss of North American colonies

intent_suppression_level(us_revolution_cycle, patriot_merchant_class, structural, 0.0).
intent_resistance_level(us_revolution_cycle, patriot_merchant_class, structural, 0.0).

intent_norm_strength(us_revolution_cycle, 1765, 0.90). % Absolute monarchy/Imperial law norm
intent_norm_strength(us_revolution_cycle, 1783, 0.98). % Republicanism as the new standard
