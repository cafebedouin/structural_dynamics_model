% ============================================================================
% CONSTRAINT STORY: china_africa_zero_tariff_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_africa_zero_tariff_2026, []).

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
 *   constraint_id: china_africa_zero_tariff_2026
 *   human_readable: China-Africa Zero-Tariff Trade Framework
 *   domain: economic
 *
 * SUMMARY:
 *   China's zero-tariff trade framework with Africa aims to increase trade
 *   and economic cooperation. This framework eliminates tariffs on imports
 *   from a significant number of African countries, potentially benefiting
 *   Chinese exporters and African consumers through access to cheaper goods.
 *   However, it also poses challenges for African manufacturers who may
 *   struggle to compete with low-priced Chinese products. The long-term
 *   impacts are uncertain, with debates focusing on whether it will foster
 *   sustainable development or increase economic dependence.
 *
 * KEY AGENTS:
 *   - chinese_exporters: Primary beneficiaries (institutional/arbitrage) - Gain increased market access.
 *   - african_consumers: Secondary beneficiaries (moderate/mobile) - Access to cheaper goods.
 *   - african_manufacturers: Primary victims (powerless/trapped) - Face increased competition.
 *   - least_competitive_african_nations: Secondary victims (moderate/constrained) - Risk of economic dependence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_africa_zero_tariff_2026, 0.55).
domain_priors:suppression_score(china_africa_zero_tariff_2026, 0.3).
domain_priors:theater_ratio(china_africa_zero_tariff_2026, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(china_africa_zero_tariff_2026, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_africa_zero_tariff_2026, tangled_rope).
narrative_ontology:human_readable(china_africa_zero_tariff_2026, "China-Africa Zero-Tariff Trade Framework").
narrative_ontology:topic_domain(china_africa_zero_tariff_2026, "economic").

domain_priors:requires_active_enforcement(china_africa_zero_tariff_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, chinese_exporters).
narrative_ontology:constraint_beneficiary(china_africa_zero_tariff_2026, african_consumers).
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, african_manufacturers).
narrative_ontology:constraint_victim(china_africa_zero_tariff_2026, least_competitive_african_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of African manufacturers unable to compete with Chinese imports, facing potential business closures and job losses. They are trapped as they lack the capital or technology to compete effectively.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of African nations with nascent manufacturing sectors. They benefit from increased trade and access to cheaper Chinese goods for their consumers, but their manufacturers face intense competition. Their exit options are constrained because of existing trade agreements and reliance on Chinese investment.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective of Chinese exporters who gain increased market access in Africa without tariffs, leading to increased sales and profits. They can arbitrage different markets, making them a primary beneficiary.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of African consumers who benefit from cheaper Chinese goods. They are mobile in the sense that they can choose between different products.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Perspective of an analytical observer assessing the long-term impacts of the framework on both China and Africa, considering both the benefits of increased trade and the potential drawbacks of economic dependence and industrial hollowing.
constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_africa_zero_tariff_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_africa_zero_tariff_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_africa_zero_tariff_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_africa_zero_tariff_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The framework allows China to extract economic benefits through increased exports and market share. Suppression (0.30): Moderate. While there is no direct coercion, African manufacturers face suppression due to their relative lack of competitiveness. Theater ratio (0.20): Low. The agreement has real economic effects beyond symbolic gestures, though there is an element of projecting China's image as a benevolent partner.
 *
 * PERSPECTIVAL GAP:
 *   The agreement is viewed differently depending on the stakeholder. Chinese exporters see it as an opportunity (Rope). African manufacturers may see it as a threat (Snare). African nations with some manufacturing capacity might see it as a mix of opportunities and threats (Tangled Rope). An analytical observer would aim to understand the net effect, considering both benefits and drawbacks (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The framework primarily benefits Chinese exporters (low d) and extracts from less competitive African manufacturers (high d). African nations with emerging industries face a mixed scenario (moderate d), as they benefit from cheaper imports for their consumers but struggle with increasing competition for their local industries. African consumers benefit through lower cost imports.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_impact_african_industrialization,
    'What will be the long-term impact on African industrialization?',
    'Economic modeling and historical analysis of similar trade agreements.',
    'Determine whether the framework promotes sustainable development or reinforces economic dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_african_industrialization, empirical, 'The long-term impact of zero-tariff framework on African industrial development.').

omega_variable(
    terms_of_trade_evolution,
    'How will the terms of trade evolve between China and Africa?',
    'Tracking export and import prices, sectorial growth, and trade balances.',
    'Reveals the distribution of economic benefits and potential imbalances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terms_of_trade_evolution, empirical, 'Evolution of trade terms over time under the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_africa_zero_tariff_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_africa_zero_tariff_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(chin_tr_t5, china_africa_zero_tariff_2026, theater_ratio, 5, 0.2).
narrative_ontology:measurement(chin_tr_t10, china_africa_zero_tariff_2026, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_africa_zero_tariff_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(chin_be_t5, china_africa_zero_tariff_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(chin_be_t10, china_africa_zero_tariff_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_africa_zero_tariff_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
