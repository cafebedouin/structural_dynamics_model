% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: The Gift of the Magi
% ==========================================================

% --- 1. Entities & Intervals ---
entity(della, individual).
entity(jim, individual).
entity(mme_sofronie, individual).
entity(the_young_household, organizational).
entity(working_poor, class).
entity(capitalist_social_norms, structural).

interval(christmas_sacrifice_interval, 0, 100).

% --- 2. Events ---
event(ev01_counting_money, resource_assessment, 5, [actor(della), cash(1.87), state(despair)]).
event(ev02_hair_sale, asset_liquidation, 30, [actor(della), target(mme_sofronie), price(20.00), asset(hair)]).
event(ev03_watch_sale, asset_liquidation, 40, [actor(jim), price(unknown), asset(gold_watch)]).
event(ev04_fob_purchase, commodity_acquisition, 50, [actor(della), item(platinum_fob), cost(21.00)]).
event(ev05_combs_acquisition, commodity_acquisition, 60, [actor(jim), item(tortoise_shell_combs)]).
event(ev06_the_exchange, convergence_of_irony, 90, [actor(della), actor(jim), result(mutual_utility_collapse)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute physical limit of a $20/week income in an $8/week flat.
constraint_claim(subsistence_limit, mountain).
constraint_metric(subsistence_limit, accessibility_collapse, 0.85).

% Noose: The social mandate of Christmas gift-giving that forces asset liquidation.
constraint_claim(christmas_gift_mandate, noose).
constraint_metric(christmas_gift_mandate, stakes_inflation, 0.95).

% Zombie: The 'Dillingham' name/status—a relic of a $30/week past that suppresses current reality.
constraint_claim(dillingham_pretense, zombie).
constraint_metric(dillingham_pretense, suppression, 0.50).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Establish a moratorium on seasonal gift-giving for households below the mendicancy threshold.').
recommendation(rec02, 'Abolish commercial hair-goods markets that leverage survival-based liquidation.').

affects_constraint(rec01, christmas_gift_mandate).
affects_constraint(rec02, subsistence_limit).

veto_actor(mme_sofronie).
veto_actor(capitalist_social_norms).

veto_exposed(mme_sofronie, rec02).
veto_exposed(capitalist_social_norms, rec01).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial State: Morning of Christmas Eve)
measurement(m01, della, accessibility_collapse(individual), 0, 0.20).
measurement(m02, della, stakes_inflation(individual), 0, 0.70).
measurement(m03, della, suppression(individual), 0, 0.30).
measurement(m04, della, resistance(individual), 0, 0.10).

measurement(m05, the_young_household, accessibility_collapse(organizational), 0, 0.40).
measurement(m06, the_young_household, stakes_inflation(organizational), 0, 0.50).
measurement(m07, the_young_household, suppression(organizational), 0, 0.40).
measurement(m08, the_young_household, resistance(organizational), 0, 0.05).

measurement(m09, working_poor, accessibility_collapse(class), 0, 0.60).
measurement(m10, working_poor, stakes_inflation(class), 0, 0.80).
measurement(m11, working_poor, suppression(class), 0, 0.70).
measurement(m12, working_poor, resistance(class), 0, 0.20).

measurement(m13, capitalist_social_norms, accessibility_collapse(structural), 0, 0.00).
measurement(m14, capitalist_social_norms, stakes_inflation(structural), 0, 0.10).
measurement(m15, capitalist_social_norms, suppression(structural), 0, 0.00). % Beneficiary
measurement(m16, capitalist_social_norms, resistance(structural), 0, 0.00). % Beneficiary

% Time Tn (Final State: Post-Gift Exchange)
measurement(m17, della, accessibility_collapse(individual), 100, 0.90). % Utility of hair/combs = 0
measurement(m18, della, stakes_inflation(individual), 100, 1.00). % Total asset depletion
measurement(m19, della, suppression(individual), 100, 0.95). % Conformed to sacrifice norm
measurement(m20, della, resistance(individual), 100, 0.00).

measurement(m21, the_young_household, accessibility_collapse(organizational), 100, 0.95).
measurement(m22, the_young_household, stakes_inflation(organizational), 100, 0.90).
measurement(m23, the_young_household, suppression(organizational), 100, 0.85).
measurement(m24, the_young_household, resistance(organizational), 100, 0.00).

measurement(m25, working_poor, accessibility_collapse(class), 100, 0.70).
measurement(m26, working_poor, stakes_inflation(class), 100, 0.90).
measurement(m27, working_poor, suppression(class), 100, 0.80).
measurement(m28, working_poor, resistance(class), 100, 0.15).

measurement(m29, capitalist_social_norms, accessibility_collapse(structural), 100, 0.05).
measurement(m30, capitalist_social_norms, stakes_inflation(structural), 100, 0.30).
measurement(m31, capitalist_social_norms, suppression(structural), 100, 0.00). % Beneficiary
measurement(m32, capitalist_social_norms, resistance(structural), 100, 0.00). % Beneficiary

% --- 6. Intent Evidence ---
intent_viable_alternative(christmas_sacrifice_interval, capitalist_social_norms, 'Mutual_Agreement_to_Forego_Gifts').
intent_alternative_rejected(christmas_sacrifice_interval, capitalist_social_norms, 'Mutual_Agreement_to_Forego_Gifts').

intent_beneficiary_class(christmas_sacrifice_interval, capitalist_social_norms).
intent_power_change(christmas_sacrifice_interval, capitalist_social_norms, 0.25). % Norm reinforcement via destruction of household wealth

intent_loser_class(christmas_sacrifice_interval, working_poor).
intent_power_change(christmas_sacrifice_interval, working_poor, -0.75). % 100% loss of heirloom and biological assets

intent_suppression_level(christmas_sacrifice_interval, capitalist_social_norms, structural, 0.0).
intent_resistance_level(christmas_sacrifice_interval, capitalist_social_norms, structural, 0.0).

intent_norm_strength(christmas_sacrifice_interval, 0, 0.80). % Strong expectation
intent_norm_strength(christmas_sacrifice_interval, 100, 1.00). % Totalized through tragic irony
