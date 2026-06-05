% ============================================================================
% CONSTRAINT STORY: spv_variations_us_cold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spv_variations_us_cold, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: spv_variations_us_cold
 *   human_readable: Stratospheric Polar Vortex Variations (US Cold Outbreaks)
 *   domain: atmospheric_science
 *
 * SUMMARY:
 *   This constraint describes the atmospheric dynamics of the Stratospheric
 *   Polar Vortex (SPV) that lead to extreme cold-air outbreaks (CAOs) in the
 *   continental US. Variations in the SPV can disrupt normal weather
 *   patterns, causing significant impacts on various sectors, including
 *   energy, agriculture, and public health. The constraint represents a
 *   complex interplay between natural variability and human vulnerability.
 *
 * KEY AGENTS:
 *   - Low-Income Households: Primary victims (powerless/trapped) — disproportionately affected by energy costs and lack resources.
 *   - Agricultural Sector: Secondary victims (moderate/constrained) — faces crop damage and economic losses.
 *   - Weather Forecasting Services: Primary beneficiaries (institutional/arbitrage) — profit from predicting these events.
 *   - Energy Trading Firms: Secondary beneficiaries (institutional/arbitrage) — profit from volatility in energy markets.
 *   - Climate Scientists: Analytical observers (analytical/analytical) — research the long-term effects and predictability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spv_variations_us_cold, 0.5).
domain_priors:suppression_score(spv_variations_us_cold, 0.4).
domain_priors:theater_ratio(spv_variations_us_cold, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spv_variations_us_cold, extractiveness, 0.5).
narrative_ontology:constraint_metric(spv_variations_us_cold, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(spv_variations_us_cold, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spv_variations_us_cold, tangled_rope).
narrative_ontology:human_readable(spv_variations_us_cold, "Stratospheric Polar Vortex Variations (US Cold Outbreaks)").
narrative_ontology:topic_domain(spv_variations_us_cold, "atmospheric_science").

domain_priors:requires_active_enforcement(spv_variations_us_cold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spv_variations_us_cold, weather_forecasting_services).
narrative_ontology:constraint_beneficiary(spv_variations_us_cold, energy_trading_firms).
narrative_ontology:constraint_victim(spv_variations_us_cold, low_income_households).
narrative_ontology:constraint_victim(spv_variations_us_cold, agricultural_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of low-income households trapped in regions affected by cold outbreaks. They face high energy costs and lack the resources to mitigate the impact.
constraint_indexing:constraint_classification(spv_variations_us_cold, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% Perspective of the agricultural sector constrained by the effects of cold outbreaks, leading to crop damage and economic losses. They benefit from long-term weather forecasts, but short-term events can be devastating. The sector is partially able to adapt but still bears significant risk.
constraint_indexing:constraint_classification(spv_variations_us_cold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of weather forecasting services that benefit from understanding and predicting these events, gaining value from information dissemination. They can arbitrage their models and services to profit.
constraint_indexing:constraint_classification(spv_variations_us_cold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of energy trading firms which benefit from the volatility created by the cold outbreaks, allowing them to profit from fluctuations in energy demand and prices. They can arbitrage their positions to capitalize on expected weather patterns.
constraint_indexing:constraint_classification(spv_variations_us_cold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical perspective of climate scientists trying to understand the long-term effects and predictability of SPV variations. They benefit from increased research funding but are also constrained by the complexity of the system.
constraint_indexing:constraint_classification(spv_variations_us_cold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spv_variations_us_cold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spv_variations_us_cold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spv_variations_us_cold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spv_variations_us_cold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(spv_variations_us_cold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness of 0.50 reflects the overall economic and social costs associated with the cold outbreaks. Suppression of 0.40 represents the limited ability to fully mitigate the impacts of these events due to incomplete predictability and lack of adaptive capacity. The theater ratio of 0.20 indicates relatively little performative activity beyond necessary forecasting and emergency response.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises because different actors experience the effects of SPV variations in drastically different ways. Low-income households are trapped and bear the brunt of the economic hardship. Energy companies and weather services benefit from the arbitrage opportunities presented by the events and have the resources to adapt. Climate scientists benefit from increased research interest.
 *
 * DIRECTIONALITY LOGIC:
 *   Weather forecasting services and energy trading firms benefit through arbitrage. Farmers and the agricultural sector are constrained, and low-income households are trapped, bearing the greatest burden. Climate scientists benefit by increased research funding but are constrained by the complexity of the atmospheric system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predictability_threshold,
    'What is the limit of predictability for SPV variations and their impact on US cold outbreaks?',
    'Improved climate models and data assimilation techniques.',
    'Determines the effectiveness of long-term forecasts and the ability to mitigate impacts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictability_threshold, empirical, 'Predictability limit of SPV variations').

omega_variable(
    attribution_accuracy,
    'How accurately can specific cold outbreaks be attributed to SPV variations versus other climate drivers?',
    'Advanced statistical analysis and climate model simulations.',
    'Influences policy decisions regarding climate change mitigation and adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_accuracy, conceptual, 'Attribution of cold outbreaks to SPV variations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spv_variations_us_cold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spv__tr_t0, spv_variations_us_cold, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spv__tr_t5, spv_variations_us_cold, theater_ratio, 5, 0.2).
narrative_ontology:measurement(spv__tr_t10, spv_variations_us_cold, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(spv__be_t0, spv_variations_us_cold, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spv__be_t5, spv_variations_us_cold, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(spv__be_t10, spv_variations_us_cold, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spv_variations_us_cold, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
