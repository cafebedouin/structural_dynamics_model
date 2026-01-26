% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Russia Consolidation
% Domain: Political Science - Consolidation of Power in Russia (1999-2008)
% ==========================================================

% --- 1. Entities & Intervals ---
entity(vladimir_putin, individual).
entity(mikhail_khodorkovsky, individual).
entity(federal_security_service, organizational).
entity(yukos_oil, organizational).
entity(oligarch_class, class).
entity(siloviki_class, class).
entity(russian_state_structure, structural).

interval(consolidation_period, 1999, 2008).

% --- 2. Events ---
event(ev01_yeltsin_resignation, political_ascent, 1999, [actor(vladimir_putin), condition(succession_guarantee)]).
event(ev02_media_most_seizure, systemic_overhaul, 2000, [target(media_most), result(information_centralization)]).
event(ev03_arrest_of_khodorkovsky, legal_coercion, 2003, [victim(mikhail_khodorkovsky), result(oligarch_submission)]).
event(ev04_abolition_of_elections, structural_change, 2004, [action(abolition_of_direct_gubernatorial_elections), result(vertical_of_power)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: Global Hydrocarbon Market Dependency.
constraint_claim(hydrocarbon_export_dependency, mountain).
constraint_metric(hydrocarbon_export_dependency, accessibility_collapse, 0.95).

% Snare: The 'Vertical of Power.' Regional autonomy is stripped as centralization tightens.
constraint_claim(vertical_of_power_centralization, snare).
constraint_metric(vertical_of_power_centralization, stakes_inflation, 0.90).

% Piton: 1990s Democratic Liberalism. The procedural shell remains (Duma, elections) but power is extracted.
constraint_claim(procedural_democratic_liberalism, piton).
constraint_metric(procedural_democratic_liberalism, suppression, 0.85).

% Rope: The 1993 Constitution. Ties executive action to a specific legal tether used for 'Dictatorship of Law.'
constraint_claim(nineteen_ninety_three_constitution, rope).
constraint_metric(ninety_ninety_three_constitution, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Restore direct gubernatorial elections to re-establish regional checks on executive power.').
recommendation(rec02, 'Decentralize energy sector ownership to prevent the use of state-owned enterprises as coercive tools.').

affects_constraint(rec01, vertical_of_power_centralization).
affects_constraint(rec02, hydrocarbon_export_dependency).

veto_actor(vladimir_putin).
veto_actor(federal_security_service).

veto_exposed(vladimir_putin, rec01).
veto_exposed(vladimir_putin, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (1999: Fragmented State / Oligarch Primacy)
measurement(m01, vladimir_putin, accessibility_collapse(individual), 1999, 0.80).
measurement(m02, vladimir_putin, stakes_inflation(individual), 1999, 0.90).
measurement(m03, vladimir_putin, suppression(individual), 1999, 0.60).
measurement(m04, vladimir_putin, resistance(individual), 1999, 0.10).

measurement(m05, federal_security_service, accessibility_collapse(organizational), 1999, 0.40).
measurement(m06, federal_security_service, stakes_inflation(organizational), 1999, 0.30).
measurement(m07, federal_security_service, suppression(organizational), 1999, 0.50).
measurement(m08, federal_security_service, resistance(organizational), 1999, 0.20).

measurement(m09, oligarch_class, accessibility_collapse(class), 1999, 0.10).
measurement(m10, oligarch_class, stakes_inflation(class), 1999, 0.20).
measurement(m11, oligarch_class, suppression(class), 1999, 0.05).
measurement(m12, oligarch_class, resistance(class), 1999, 0.00).

measurement(m13, russian_state_structure, accessibility_collapse(structural), 1999, 0.10).
measurement(m14, russian_state_structure, stakes_inflation(structural), 1999, 0.15).
measurement(m15, russian_state_structure, suppression(structural), 1999, 0.00).
measurement(m16, russian_state_structure, resistance(structural), 1999, 0.00).

% Time Tn (2008: Established Vertical / Peak Consolidation)
measurement(m17, vladimir_putin, accessibility_collapse(individual), 2008, 0.00).
measurement(m18, vladimir_putin, stakes_inflation(individual), 2008, 1.00).
measurement(m19, vladimir_putin, suppression(individual), 2008, 0.00).
measurement(m20, vladimir_putin, resistance(individual), 2008, 0.95).

measurement(m21, federal_security_service, accessibility_collapse(organizational), 2008, 0.05).
measurement(m22, federal_security_service, stakes_inflation(organizational), 2008, 0.95).
measurement(m23, federal_security_service, suppression(organizational), 2008, 0.00).
measurement(m24, federal_security_service, resistance(organizational), 2008, 0.98).

measurement(m25, oligarch_class, accessibility_collapse(class), 2008, 0.95).
measurement(m26, oligarch_class, stakes_inflation(class), 2008, 0.98).
measurement(m27, oligarch_class, suppression(class), 2008, 0.90).
measurement(m28, oligarch_class, resistance(class), 2008, 0.05).

measurement(m29, russian_state_structure, accessibility_collapse(structural), 2008, 0.00).
measurement(m30, russian_state_structure, stakes_inflation(structural), 2008, 0.50).
measurement(m31, russian_state_structure, suppression(structural), 2008, 0.00).
measurement(m32, russian_state_structure, resistance(structural), 2008, 0.00).

% --- 6. Intent Evidence ---
intent_viable_alternative(consolidation_period, russian_state_structure, 'Decentralized_Federal_Parliamentarianism').
intent_alternative_rejected(consolidation_period, russian_state_structure, 'Decentralized_Federal_Parliamentarianism').

intent_beneficiary_class(consolidation_period, siloviki_class).
intent_power_change(consolidation_period, siloviki_class, 0.85).

intent_loser_class(consolidation_period, oligarch_class).
intent_power_change(consolidation_period, oligarch_class, -0.90).

intent_suppression_level(consolidation_period, siloviki_class, structural, 0.0).
intent_resistance_level(consolidation_period, siloviki_class, structural, 0.0).

intent_norm_strength(consolidation_period, 1999, 0.40).
intent_norm_strength(consolidation_period, 2008, 0.98).
