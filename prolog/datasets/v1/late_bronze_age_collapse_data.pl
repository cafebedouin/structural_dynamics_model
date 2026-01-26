% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Late Bronze Age Collapse
% Domain: Macro-History - Systemic Collapse of Mediterranean Civilizations
% ==========================================================

% --- 1. Entities & Intervals ---
entity(ramesses_iii, individual).
entity(mycenaean_palace_administration, organizational).
entity(warrior_chariot_elite, class).
entity(displaced_peasantry, class).
entity(palatial_command_economy, structural).
entity(international_trade_network, structural).

interval(lba_collapse_cycle, 0, 100). % T0: 1200 BCE (International Peak); Tn: 1150 BCE (Systemic Failure)

% --- 2. Events ---
event(ev01_hittite_famine, resource_collapse, 10, [location(anatolia), type(drought), severity(extreme)]).
event(ev02_pylos_destruction, kinetic_initiation, 45, [target(mycenaean_state), actor(unknown_invaders), result(palace_burning)]).
event(ev03_battle_of_the_delta, defensive_success, 85, [actor(ramesses_iii), target(sea_peoples), result(egyptian_survival)]).
event(ev04_iron_transition, technology_shift, 98, [subject(metallurgy), effect(bronze_obsolescence)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: Tin Trade Geography. The physical impossibility of manufacturing bronze without long-distance trade routes to Afghanistan/Cornwall.
constraint_claim(tin_trade_dependency, mountain).
constraint_metric(tin_trade_dependency, accessibility_collapse, 0.98).

% Snare: Interconnected Fragility. A 'multiplier effect' where the failure of one palace (e.g., Ugarit) triggers bankruptcy in others, narrowing survival paths.
constraint_claim(systemic_interdependency, snare).
constraint_metric(systemic_interdependency, stakes_inflation, 0.95).

% Piton: Palace Redistribution Norm. The dead belief that the King can provide food/security, persisting in administrative tablets even as the stores are empty.
constraint_claim(palatial_redistribution_myth, piton).
constraint_metric(palatial_redistribution_myth, suppression, 0.85).

% Rope: Chariot Infrastructure. Ties military capability to expensive, high-maintenance hardware that requires a stable state to function.
constraint_claim(chariot_logistics_binding, rope).
constraint_metric(chariot_logistics_binding, suppression, 0.75).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt decentralized Iron metallurgy to bypass the collapse of the Tin trade route.').
recommendation(rec02, 'Implement autonomous village-level subsistence to reduce dependency on failing palace stores.').

affects_constraint(rec01, tin_trade_dependency).
affects_constraint(rec02, palatial_command_economy).

veto_actor(warrior_chariot_elite).
veto_actor(mycenaean_palace_administration).

veto_exposed(warrior_chariot_elite, rec01). % Iron empowers the infantry and breaks the chariot monopoly.
veto_exposed(mycenaean_palace_administration, rec02). % Decentralized food control eliminates the palace's leverage.

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (1200 BCE: High Interconnectivity)
measurement(m01, ramesses_iii, accessibility_collapse(individual), 0, 0.20).
measurement(m02, ramesses_iii, stakes_inflation(individual), 0, 0.40).
measurement(m03, ramesses_iii, suppression(individual), 0, 0.10).
measurement(m04, ramesses_iii, resistance(individual), 0, 0.05).

measurement(m05, mycenaean_palace_administration, accessibility_collapse(organizational), 0, 0.15).
measurement(m06, mycenaean_palace_administration, stakes_inflation(organizational), 0, 0.30).
measurement(m07, mycenaean_palace_administration, suppression(organizational), 0, 0.05).
measurement(m08, mycenaean_palace_administration, resistance(organizational), 0, 0.02).

measurement(m09, warrior_chariot_elite, accessibility_collapse(class), 0, 0.10).
measurement(m10, warrior_chariot_elite, stakes_inflation(class), 0, 0.25).
measurement(m11, warrior_chariot_elite, suppression(class), 0, 0.00). % Beneficiary
measurement(m12, warrior_chariot_elite, resistance(class), 0, 0.00). % Beneficiary

measurement(m13, palatial_command_economy, accessibility_collapse(structural), 0, 0.05).
measurement(m14, palatial_command_economy, stakes_inflation(structural), 0, 0.15).
measurement(m15, palatial_command_economy, suppression(structural), 0, 0.00).
measurement(m16, palatial_command_economy, resistance(structural), 0, 0.00).

% Time Tn (1150 BCE: Systemic Collapse)
measurement(m17, ramesses_iii, accessibility_collapse(individual), 100, 0.85). % Options locked to mere defense
measurement(m18, ramesses_iii, stakes_inflation(individual), 100, 1.00). % Dynasty vs extinction
measurement(m19, ramesses_iii, suppression(individual), 100, 0.90). % Crushed by geopolitical tides
measurement(m20, ramesses_iii, resistance(individual), 100, 0.95). % Active defiance of the Sea Peoples

measurement(m21, mycenaean_palace_administration, accessibility_collapse(organizational), 100, 1.00). % Non-existence
measurement(m22, mycenaean_palace_administration, stakes_inflation(organizational), 100, 1.00).
measurement(m23, mycenaean_palace_administration, suppression(organizational), 100, 1.00).
measurement(m24, mycenaean_palace_administration, resistance(organizational), 100, 0.00).

measurement(m25, warrior_chariot_elite, accessibility_collapse(class), 100, 0.95).
measurement(m26, warrior_chariot_elite, stakes_inflation(class), 100, 1.00).
measurement(m27, warrior_chariot_elite, suppression(class), 100, 0.90).
measurement(m28, warrior_chariot_elite, resistance(class), 100, 0.10).

measurement(m29, palatial_command_economy, accessibility_collapse(structural), 100, 1.00). % Structural total failure
measurement(m30, palatial_command_economy, stakes_inflation(structural), 100, 0.60).
measurement(m31, palatial_command_economy, suppression(structural), 100, 0.00).
measurement(m32, palatial_command_economy, resistance(structural), 100, 0.00).

% --- 6. Intent Evidence ---
intent_viable_alternative(lba_collapse_cycle, palatial_command_economy, 'Decentralized_Communal_Subsistence').
intent_alternative_rejected(lba_collapse_cycle, palatial_command_economy, 'Decentralized_Communal_Subsistence').

intent_beneficiary_class(lba_collapse_cycle, displaced_peasantry). % Survival of the group through collapse of the tax structure
intent_power_change(lba_collapse_cycle, displaced_peasantry, 0.30). % Gain in local autonomy, despite danger

intent_loser_class(lba_collapse_cycle, warrior_chariot_elite).
intent_power_change(lba_collapse_cycle, warrior_chariot_elite, -0.98). % Total liquidation of the class status

intent_suppression_level(lba_collapse_cycle, displaced_peasantry, structural, 0.0).
intent_resistance_level(lba_collapse_cycle, displaced_peasantry, structural, 0.0).

intent_norm_strength(lba_collapse_cycle, 0, 0.95). % Absolute Palatial control
intent_norm_strength(lba_collapse_cycle, 100, 0.10). % Dark Age fragmentation
