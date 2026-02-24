% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Roman Empire Decline
% Domain: Macro-History - The Decline and Fall of the Western Roman Empire
% ==========================================================

% --- 1. Entities & Intervals ---
entity(emperor_romulus, individual).
entity(odoacer, individual).
entity(roman_legions, organizational).
entity(senatorial_elite, class).
entity(barbarian_foederati, class).
entity(western_imperial_system, structural).

interval(roman_decline_and_fall, 284, 476). % From Diocletian's Tetrarchy to the deposition of Romulus

% --- 2. Events ---
event(ev01_tetrarchy_split, structural_change, 284, [actor(diocletian), result(administrative_division)]).
event(ev02_sack_of_rome, kinetic_initiation, 410, [actor(alaric), target(rome), property(symbolic_collapse)]).
event(ev03_battle_of_frigidus, institutional_drain, 394, [effect(liquidation_of_western_field_army)]).
event(ev04_deposition_of_romulus, final_verification, 476, [actor(odoacer), result(sovereignty_extraction)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: Frontier Logistical Overstretch. The physical impossibility of defending a 10,000-mile perimeter with a shrinking tax base.
constraint_claim(frontier_overstretch, mountain).
constraint_metric(frontier_overstretch, accessibility_collapse, 0.98).

% Snare: The Foederati Dependency. Rome recruits barbarians to fight barbarians; each cycle narrows the state's autonomous military options.
constraint_claim(barbarian_military_dependency, snare).
constraint_metric(barbarian_military_dependency, stakes_inflation, 0.95).

% Piton: Eternal Rome Ideology. The dead concept that the Mediterranean remains a Roman Lake, persisting in protocol while the borders vanish.
constraint_claim(eternal_rome_myth, piton).
constraint_metric(eternal_rome_myth, suppression, 0.85).

% Rope: The Colonate System. Rigidly ties the peasantry to the soil to ensure a fixed tax revenue, stripping economic agency.
constraint_claim(colonate_land_binding, rope).
constraint_metric(colonate_land_binding, suppression, 0.75).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Abolish the foederati system and return to a professional citizen-soldier levy to regain military agency.').
recommendation(rec02, 'Withdraw to a defensible core (The Italian Peninsula) to bypass the mountain of logistical overstretch.').

affects_constraint(rec01, barbarian_military_dependency).
affects_constraint(rec02, frontier_overstretch).

veto_actor(senatorial_elite).
veto_actor(western_imperial_system).

veto_exposed(senatorial_elite, rec01). % Elite refused to serve or pay for citizen-levies.
veto_exposed(western_imperial_system, rec02). % The structural ideology of "Eternal Rome" made withdrawal unthinkable.

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (284: High Imperial Reorganization)
measurement(m01, emperor_romulus, accessibility_collapse(individual), 284, 0.20).
measurement(m02, emperor_romulus, stakes_inflation(individual), 284, 0.40).
measurement(m03, emperor_romulus, suppression(individual), 284, 0.15).
measurement(m04, emperor_romulus, resistance(individual), 284, 0.10).

measurement(m05, roman_legions, accessibility_collapse(organizational), 284, 0.10).
measurement(m06, roman_legions, stakes_inflation(organizational), 284, 0.25).
measurement(m07, roman_legions, suppression(organizational), 284, 0.05).
measurement(m08, roman_legions, resistance(organizational), 284, 0.05).

measurement(m09, senatorial_elite, accessibility_collapse(class), 284, 0.05).
measurement(m10, senatorial_elite, stakes_inflation(class), 284, 0.20).
measurement(m11, senatorial_elite, suppression(class), 284, 0.10).
measurement(m12, senatorial_elite, resistance(class), 284, 0.95).

measurement(m13, western_imperial_system, accessibility_collapse(structural), 284, 0.05).
measurement(m14, western_imperial_system, stakes_inflation(structural), 284, 0.10).
measurement(m15, western_imperial_system, suppression(structural), 284, 0.00). % Beneficiary Logic
measurement(m16, western_imperial_system, resistance(structural), 284, 0.00). % Beneficiary Logic

% Time Tn (476: Terminal Sovereignty Extraction)
measurement(m17, emperor_romulus, accessibility_collapse(individual), 476, 1.00). % No remaining moves.
measurement(m18, emperor_romulus, stakes_inflation(individual), 476, 1.00). % Absolute survival stake.
measurement(m19, emperor_romulus, suppression(individual), 476, 0.98). % Folded by the structure.
measurement(m20, emperor_romulus, resistance(individual), 476, 0.02). % Submission to odoacer.

measurement(m21, roman_legions, accessibility_collapse(organizational), 476, 0.95). % Functionally non-existent.
measurement(m22, roman_legions, stakes_inflation(organizational), 476, 0.95).
measurement(m23, roman_legions, suppression(organizational), 476, 0.98).
measurement(m24, roman_legions, resistance(organizational), 476, 0.05).

measurement(m25, senatorial_elite, accessibility_collapse(class), 476, 0.85).
measurement(m26, senatorial_elite, stakes_inflation(class), 476, 0.95).
measurement(m27, senatorial_elite, suppression(class), 476, 0.90).
measurement(m28, senatorial_elite, resistance(class), 476, 0.20).

measurement(m29, western_imperial_system, accessibility_collapse(structural), 476, 1.00).
measurement(m30, western_imperial_system, stakes_inflation(structural), 476, 0.50).
measurement(m31, western_imperial_system, suppression(structural), 476, 0.00). % Beneficiary Rule
measurement(m32, western_imperial_system, resistance(structural), 476, 0.00). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(roman_decline_and_fall, western_imperial_system, 'Regional_Gallic_Autonomy_Consolidation').
intent_alternative_rejected(roman_decline_and_fall, western_imperial_system, 'Regional_Gallic_Autonomy_Consolidation').

intent_beneficiary_class(roman_decline_and_fall, barbarian_foederati).
intent_power_change(roman_decline_and_fall, barbarian_foederati, 0.90). % Structural gain of land and sovereign title.

intent_loser_class(roman_decline_and_fall, senatorial_elite).
intent_power_change(roman_decline_and_fall, senatorial_elite, -0.75). % Loss of imperial protection and vast Mediterranean estates.

intent_suppression_level(roman_decline_and_fall, barbarian_foederati, structural, 0.0).
intent_resistance_level(roman_decline_and_fall, barbarian_foederati, structural, 0.0).

intent_norm_strength(roman_decline_and_fall, 284, 0.90). % Roman law as total dogma.
intent_norm_strength(roman_decline_and_fall, 476, 0.15). % Imperial norm shattered; local feudalism emerging.
