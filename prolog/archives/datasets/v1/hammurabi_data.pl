% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Code of Hammurabi
% Domain: Ancient Statutory Law - The Legal Codification of Babylon
% ==========================================================

% --- 1. Entities & Intervals ---
entity(hammurabi, individual).
entity(shamas_sun_god, individual).
entity(commoner_subject, individual).
entity(city_courts, organizational).
entity(temple_administration, organizational).
entity(debtor_class, class).
entity(tributary_class, class).
entity(babylonian_state, structural).
entity(lex_talionis_system, structural).

interval(hammurabi_codification, 0, 100). % From pre-code decentralized justice (T0) to centralized legal order (Tn)

% --- 2. Events ---
event(ev01_divine_reception, legal_initiation, 5, [actor(hammurabi), source(shamas_sun_god), concept(divine_justice)]).
event(ev02_stele_promulgation, systemic_overhaul, 35, [location(sippara), method(diorite_inscription), reach(imperial)]).
event(ev03_judicial_standardization, parameter_setting, 70, [target(city_courts), logic(sealed_sentence)]).
event(ev04_imperial_stability, state_stabilization, 95, [subject(mesopotamia), result(fixed_price_and_wage)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: Lex Talionis (Retaliation). The biological/physical reality of 'eye for eye' and 'limb for limb' as irreversible physical damage.
constraint_claim(lex_talionis_retaliation, mountain).
constraint_metric(lex_talionis_retaliation, accessibility_collapse, 0.95).

% Snare: Debt Bondage. The trap where an unpaid debt forces the assignment of wife or child to work off the debt, tightening over three years.
constraint_claim(debt_bondage_trap, snare).
constraint_metric(debt_bondage_trap, stakes_inflation, 0.90).

% Piton: Divine Monarchy. The dead idea that the king is a direct conduit for the Sun God's judgment, persisting as a living regulatory force.
constraint_claim(divine_command_legitimacy, piton).
constraint_metric(divine_command_legitimacy, suppression, 0.85).

% Rope: Fixed Wages and Hires. Ties all economic activity to rigid statutory rates (e.g., 6 gur of corn for a boatman).
constraint_claim(statutory_wage_binding, rope).
constraint_metric(statutory_wage_binding, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Replace corporal retaliation with financial restitution for all social estates.').
recommendation(rec02, 'Abolish the assignment of family members for debt service to maintain household sovereignty.').

affects_constraint(rec01, lex_talionis_retaliation).
affects_constraint(rec02, debt_bondage_trap).

veto_actor(hammurabi).
veto_actor(babylonian_state).

veto_exposed(hammurabi, rec01).
veto_exposed(babylonian_state, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Pre-Codification / Clan-Based Justice)
measurement(m01, commoner_subject, accessibility_collapse(individual), 0, 0.20).
measurement(m02, commoner_subject, stakes_inflation(individual), 0, 0.50).
measurement(m03, commoner_subject, suppression(individual), 0, 0.15).
measurement(m04, commoner_subject, resistance(individual), 0, 0.80).

measurement(m05, city_courts, accessibility_collapse(organizational), 0, 0.30).
measurement(m06, city_courts, stakes_inflation(organizational), 0, 0.40).
measurement(m07, city_courts, suppression(organizational), 0, 0.10).
measurement(m08, city_courts, resistance(organizational), 0, 0.25).

measurement(m09, tributary_class, accessibility_collapse(class), 0, 0.25).
measurement(m10, tributary_class, stakes_inflation(class), 0, 0.45).
measurement(m11, tributary_class, suppression(class), 0, 0.20).
measurement(m12, tributary_class, resistance(class), 0, 0.70).

measurement(m13, babylonian_state, accessibility_collapse(structural), 0, 0.05).
measurement(m14, babylonian_state, stakes_inflation(structural), 0, 0.15).
measurement(m15, babylonian_state, suppression(structural), 0, 0.00). % Beneficiary logic
measurement(m16, babylonian_state, resistance(structural), 0, 0.00). % Beneficiary logic

% Time Tn (Post-Codification / Centralized Legal Totalitarianism)
measurement(m17, commoner_subject, accessibility_collapse(individual), 100, 0.85). % Options locked by written code
measurement(m18, commoner_subject, stakes_inflation(individual), 100, 0.98). % Capital punishment for theft/perjury
measurement(m19, commoner_subject, suppression(individual), 100, 0.90). % Agency replaced by statutory rule
measurement(m20, commoner_subject, resistance(individual), 100, 0.05). % Submission to imperial decree

measurement(m21, city_courts, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, city_courts, stakes_inflation(organizational), 100, 0.95).
measurement(m23, city_courts, suppression(organizational), 100, 0.10).
measurement(m24, city_courts, resistance(organizational), 100, 0.98).

measurement(m25, tributary_class, accessibility_collapse(class), 100, 0.10).
measurement(m26, tributary_class, stakes_inflation(class), 100, 0.95).
measurement(m27, tributary_class, suppression(class), 100, 0.40).
measurement(m28, tributary_class, resistance(class), 100, 0.60).

measurement(m29, babylonian_state, accessibility_collapse(structural), 100, 0.00).
measurement(m30, babylonian_state, stakes_inflation(structural), 100, 0.50).
measurement(m31, babylonian_state, suppression(structural), 100, 0.00). % Beneficiary logic
measurement(m32, babylonian_state, resistance(structural), 100, 0.00). % Beneficiary logic

% --- 6. Intent Evidence ---
intent_viable_alternative(hammurabi_codification, babylonian_state, 'Decentralized_Kin_Based_Mediation').
intent_alternative_rejected(hammurabi_codification, babylonian_state, 'Decentralized_Kin_Based_Mediation').

intent_beneficiary_class(hammurabi_codification, babylonian_state).
intent_power_change(hammurabi_codification, babylonian_state, 0.90). % Consolidation of monopoly on violence

intent_loser_class(hammurabi_codification, debtor_class). % Total loss of family autonomy to creditors
intent_power_change(hammurabi_codification, debtor_class, -0.75).

intent_suppression_level(hammurabi_codification, babylonian_state, structural, 0.0).
intent_resistance_level(hammurabi_codification, babylonian_state, structural, 0.0).

intent_norm_strength(hammurabi_codification, 0, 0.35). % Laws vary by locality/clan
intent_norm_strength(hammurabi_codification, 100, 1.00). % Absolute standard for the entire empire
