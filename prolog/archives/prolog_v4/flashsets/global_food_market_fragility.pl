% ============================================================================
% CONSTRAINT STORY: global_food_market_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_food_market_fragility, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: global_food_market_fragility
 *   human_readable: Global Food Market Fragility to Correlated Crop Failures
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The global food market is a complex system for distributing staple crops.
 *   Its fragility arises from its vulnerability to correlated crop failures,
 *   often driven by climate change or geopolitical instability. This
 *   fragility creates winners and losers, with low-income populations and
 *   food-importing nations bearing the brunt of the impact, while
 *   food-trading corporations and grain-exporting nations can benefit.
 *
 * KEY AGENTS:
 *   - Low-Income Populations: Primary target (powerless/trapped) - suffer from price spikes and food shortages.
 *   - Food-Importing Nations: Secondary target (moderate/constrained) - constrained by the market's volatility.
 *   - Grain-Exporting Nations: Primary beneficiary (institutional/arbitrage) - benefit from increased demand and prices.
 *   - Food Trading Corporations: Secondary beneficiary (powerful/arbitrage) - profit from market volatility and price differences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_food_market_fragility, 0.6).
domain_priors:suppression_score(global_food_market_fragility, 0.7).
domain_priors:theater_ratio(global_food_market_fragility, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_food_market_fragility, extractiveness, 0.6).
narrative_ontology:constraint_metric(global_food_market_fragility, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(global_food_market_fragility, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_food_market_fragility, tangled_rope).
narrative_ontology:human_readable(global_food_market_fragility, "Global Food Market Fragility to Correlated Crop Failures").
narrative_ontology:topic_domain(global_food_market_fragility, "economic/geopolitical").

domain_priors:requires_active_enforcement(global_food_market_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_food_market_fragility, food_trading_corporations).
narrative_ontology:constraint_beneficiary(global_food_market_fragility, grain_exporting_nations).
narrative_ontology:constraint_victim(global_food_market_fragility, food_importing_nations).
narrative_ontology:constraint_victim(global_food_market_fragility, low_income_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low-income populations in food-importing nations are trapped by the global food market. They have no alternatives when prices rise due to crop failures. This is a Snare as they bear the brunt of the fragility with no exit.
constraint_indexing:constraint_classification(global_food_market_fragility, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Food-importing nations are constrained. They must secure food for their populations but have limited leverage in the global market. They can diversify sources to some extent, but correlated failures limit this. The outcome is a Tangled Rope; they experience extraction, but maintain constrained coordination.
constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Grain-exporting nations benefit from higher prices and increased demand during times of global food market fragility. They can increase production to capitalize on the market. This is a Rope; they experience coordination and benefits. High production may not be sustainable and relies on continued subsidies.
constraint_indexing:constraint_classification(global_food_market_fragility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Food trading corporations are positioned to profit from volatility in the market by exploiting price differences and managing risk through sophisticated financial instruments. They also benefit from the continued enforcement of existing trade rules and infrastructure. Pure coordination function for those best positioned to arbitrage.
constraint_indexing:constraint_classification(global_food_market_fragility, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees the system as a tangled rope: it serves a function in distributing food globally, but also extracts value and exacerbates inequalities during crises, and requires active enforcement to maintain.
constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_food_market_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_food_market_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_food_market_fragility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_food_market_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(global_food_market_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.6) due to the significant transfer of wealth from food-importing to food-exporting nations during crises. Suppression is also high (0.7) because of the lack of alternatives for food-importing nations and low-income populations. Theater ratio is low (0.3) because there is limited performative activity associated with managing this extraction. The enforcement occurs through global trade regulations and existing contracts.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is significant. Low-income populations see the system as a snare, as they bear the brunt of the negative impacts. Grain-exporting nations and food trading corporations see it as a rope, benefiting from increased demand and arbitrage opportunities. Food-importing nations see it as a tangled rope, as they are both constrained and benefit from the system's basic function of food distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural position of each agent. Food-importing nations and low-income populations are victims, so their d values are high, leading to higher chi. Grain-exporting nations and food trading corporations are beneficiaries, so their d values are low, leading to lower chi. An analytical perspective sees the combined impacts with a value of moderate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_model_accuracy,
    'How accurately can climate models predict correlated crop failures across different regions?',
    'Improved climate modeling, historical data analysis, and machine learning to identify patterns and predict risks.',
    'If inaccurate, the fragility of the system will remain hidden, leading to unexpected crises. If accurate, proactive measures can be taken to mitigate risks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_model_accuracy, empirical, 'Accuracy of climate models in predicting correlated crop failures.').

omega_variable(
    reserve_capacity_sufficiency,
    'Is the current global reserve capacity (grain stockpiles, storage facilities) sufficient to buffer against large-scale crop failures?',
    'Analysis of current reserve levels, historical data on crop failures, and simulations to determine optimal reserve levels.',
    'If insufficient, the system will be highly vulnerable to price spikes and food shortages. If sufficient, the system will be more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_capacity_sufficiency, empirical, 'Sufficiency of global reserve capacity to handle crop failures.').

omega_variable(
    trade_policy_coordination,
    'To what extent can international trade policies be coordinated to ensure equitable access to food during crises?',
    'Negotiations and agreements among nations, improved monitoring of trade flows, and mechanisms for rapid response to food shortages.',
    'If poorly coordinated, protectionist measures and export restrictions can exacerbate crises. If well-coordinated, food can be distributed more equitably.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trade_policy_coordination, preference, 'Coordination of trade policies to ensure food access during crises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_food_market_fragility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glob_tr_t0, global_food_market_fragility, theater_ratio, 0, 0.1).
narrative_ontology:measurement(glob_tr_t10, global_food_market_fragility, theater_ratio, 10, 0.2).
narrative_ontology:measurement(glob_tr_t20, global_food_market_fragility, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(glob_be_t0, global_food_market_fragility, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(glob_be_t10, global_food_market_fragility, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(glob_be_t20, global_food_market_fragility, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(global_food_market_fragility, climate_change_impacts).
narrative_ontology:affects_constraint(global_food_market_fragility, global_trade_networks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
