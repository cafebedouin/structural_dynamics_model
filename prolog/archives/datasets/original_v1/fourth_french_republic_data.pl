% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Collapse of the French Fourth Republic
% Domain: Political History - The May 1958 Crisis and the Rise of the Fifth Republic
% ==========================================================

% --- 1. Entities & Intervals ---
entity(charles_de_gaulle, individual).
entity(pierre_pflimlin, individual).
entity(french_army_algiers, organizational).
entity(national_assembly, organizational).
entity(parliamentary_class, class).
entity(military_officer_corps, class).
entity(fourth_republic_constitution, structural).
entity(executive_gaullist_framework, structural).

interval(may_1958_crisis, 0, 100). % From the May 13th Algiers coup (T0) to the investiture of De Gaulle (Tn)

% --- 2. Events ---
event(ev01_algiers_uprising, military_revolt, 0, [actor(french_army_algiers), goal(algerie_francaise)]).
event(ev02_de_gaulle_statement, theoretical_initiation, 15, [actor(charles_de_gaulle), text("ready_to_assume_powers")]).
event(ev03_operation_resurrection, physical_threat, 75, [subject(corsica_seizure), threat(paratrooper_drop_on_paris)]).
event(ev04_investiture_vote, systemic_overhaul, 100, [actor(national_assembly), result(fifth_republic_transition)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: Decolonization Pressure. The objective geopolitical and demographic reality of the Algerian War.
constraint_claim(decolonization_imperative, mountain).
constraint_metric(decolonization_imperative, accessibility_collapse, 0.95).

% Snare: Ministerial Instability. The 'System of Parties' that forced government collapses every few months, tightening the paralysis.
constraint_claim(parliamentary_immobilism, snare).
constraint_metric(parliamentary_immobilism, stakes_inflation, 0.92).

% Piton: 1946 Sovereignty Ideal. The dead belief that the Assembly could rule without a strong executive, surviving only as a legal shell.
constraint_claim(legislative_supremacy_myth, piton).
constraint_metric(legislative_supremacy_myth, suppression, 0.85).

% Rope: The 1946 Constitution. Ties the Prime Minister's hands, preventing effective military oversight.
constraint_claim(fourth_republic_legal_tether, rope).
constraint_metric(fourth_republic_legal_tether, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Abolish the investiture process to allow direct presidential appointment of cabinets.').
recommendation(rec02, 'Authorize emergency decree powers to the executive to bypass Assembly deadlock in wartime.').

affects_constraint(rec01, parliamentary_immobilism).
affects_constraint(rec02, legislative_supremacy_myth).

veto_actor(national_assembly).
veto_actor(parliamentary_class).

veto_exposed(national_assembly, rec01).
veto_exposed(parliamentary_class, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial Coup / Institutional Deadlock)
measurement(m01, charles_de_gaulle, accessibility_collapse(individual), 0, 0.80).
measurement(m02, charles_de_gaulle, stakes_inflation(individual), 0, 0.50).
measurement(m03, charles_de_gaulle, suppression(individual), 0, 0.40).
measurement(m04, charles_de_gaulle, resistance(individual), 0, 0.10).

measurement(m05, national_assembly, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, national_assembly, stakes_inflation(organizational), 0, 0.30).
measurement(m07, national_assembly, suppression(organizational), 0, 0.05).
measurement(m08, national_assembly, resistance(organizational), 0, 0.05).

measurement(m09, military_officer_corps, accessibility_collapse(class), 0, 0.20).
measurement(m10, military_officer_corps, stakes_inflation(class), 0, 0.85).
measurement(m11, military_officer_corps, suppression(class), 0, 0.10).
measurement(m12, military_officer_corps, resistance(class), 0, 0.90).

measurement(m13, executive_gaullist_framework, accessibility_collapse(structural), 0, 0.00).
measurement(m14, executive_gaullist_framework, stakes_inflation(structural), 0, 0.10).
measurement(m15, executive_gaullist_framework, suppression(structural), 0, 0.00). % Beneficiary Rule
measurement(m16, executive_gaullist_framework, resistance(structural), 0, 0.00). % Beneficiary Rule

% Time Tn (Investiture / Gaullist Dominance)
measurement(m17, charles_de_gaulle, accessibility_collapse(individual), 100, 0.00).
measurement(m18, charles_de_gaulle, stakes_inflation(individual), 100, 0.95).
measurement(m19, charles_de_gaulle, suppression(individual), 100, 0.00).
measurement(m20, charles_de_gaulle, resistance(individual), 100, 1.00).

measurement(m21, national_assembly, accessibility_collapse(organizational), 100, 0.90). % Compliance or dissolution
measurement(m22, national_assembly, stakes_inflation(organizational), 100, 0.95).
measurement(m23, national_assembly, suppression(organizational), 100, 0.85).
measurement(m24, national_assembly, resistance(organizational), 100, 0.10).

measurement(m25, military_officer_corps, accessibility_collapse(class), 100, 0.10).
measurement(m26, military_officer_corps, stakes_inflation(class), 100, 0.70).
measurement(m27, military_officer_corps, suppression(class), 100, 0.20).
measurement(m28, military_officer_corps, resistance(class), 100, 0.95).

measurement(m29, executive_gaullist_framework, accessibility_collapse(structural), 100, 0.00).
measurement(m30, executive_gaullist_framework, stakes_inflation(structural), 100, 0.50).
measurement(m31, executive_gaullist_framework, suppression(structural), 100, 0.00). % Beneficiary Rule
measurement(m32, executive_gaullist_framework, resistance(structural), 100, 0.00). % Beneficiary Rule

% --- 6. Intent Evidence ---
intent_viable_alternative(may_1958_crisis, fourth_republic_constitution, 'Liberal_Parliamentary_Consolidation').
intent_alternative_rejected(may_1958_crisis, fourth_republic_constitution, 'Liberal_Parliamentary_Consolidation').

intent_beneficiary_class(may_1958_crisis, military_officer_corps).
intent_power_change(may_1958_crisis, military_officer_corps, 0.45). % Tactical win, though later betrayed by de Gaulle

intent_loser_class(may_1958_crisis, parliamentary_class).
intent_power_change(may_1958_crisis, parliamentary_class, -0.90). % Permanent loss of supremacy to the Presidency

intent_suppression_level(may_1958_crisis, executive_gaullist_framework, structural, 0.0).
intent_resistance_level(may_1958_crisis, executive_gaullist_framework, structural, 0.0).

intent_norm_strength(may_1958_crisis, 0, 0.45). % Parliamentary supremacy is failing
intent_norm_strength(may_1958_crisis, 100, 0.98). % Presidential dominance becomes the new standard
