% ============================================================================
% CONSTRAINT STORY: india_semi_mission
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_semi_mission, []).

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
 *   constraint_id: india_semi_mission
 *   human_readable: India Semiconductor Mission 2.0
 *   domain: economic
 *
 * SUMMARY:
 *   The India Semiconductor Mission 2.0 aims to boost the domestic chip
 *   industry through financial incentives, infrastructure development, and
 *   skill training. It represents an attempt to create a national champion in
 *   a strategically important sector. However, it also entails significant
 *   costs to taxpayers and carries the risk of creating inefficiencies or
 *   distorting the market.
 *
 * KEY AGENTS:
 *   - Domestic Chip Manufacturers: Primary beneficiaries (institutional/arbitrage)
 *   - Indian Taxpayers: Primary victims (powerless/trapped)
 *   - Importing Sectors: Secondary victims (moderate/constrained)
 *   - Skilled Labor Force: Mobile workers (moderate/mobile)
 *   - Equipment Suppliers: Beneficiaries (institutional/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_semi_mission, 0.55).
domain_priors:suppression_score(india_semi_mission, 0.4).
domain_priors:theater_ratio(india_semi_mission, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_semi_mission, extractiveness, 0.55).
narrative_ontology:constraint_metric(india_semi_mission, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(india_semi_mission, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_semi_mission, tangled_rope).
narrative_ontology:human_readable(india_semi_mission, "India Semiconductor Mission 2.0").
narrative_ontology:topic_domain(india_semi_mission, "economic").

domain_priors:requires_active_enforcement(india_semi_mission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_semi_mission, domestic_chip_manufacturers).
narrative_ontology:constraint_beneficiary(india_semi_mission, equipment_suppliers).
narrative_ontology:constraint_beneficiary(india_semi_mission, skilled_labor_force).
narrative_ontology:constraint_victim(india_semi_mission, indian_taxpayers).
narrative_ontology:constraint_victim(india_semi_mission, importing_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Indian Taxpayers are trapped and bear the cost of subsidies and potential inefficiencies.
constraint_indexing:constraint_classification(india_semi_mission, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Importing sectors are constrained by potential increases in domestic chip prices but benefit from greater supply chain security and potential economic growth.
constraint_indexing:constraint_classification(india_semi_mission, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Domestic Chip Manufacturers benefit from financial incentives and infrastructure development and have arbitrage options to adjust production and investment.
constraint_indexing:constraint_classification(india_semi_mission, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% The skilled labor force benefits from increased job opportunities but has a sunset clause because they may migrate elsewhere.
constraint_indexing:constraint_classification(india_semi_mission, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the India Semiconductor Mission 2.0 as a tangled rope, involving both coordination and extraction at a global scale.
constraint_indexing:constraint_classification(india_semi_mission, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_semi_mission_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_semi_mission, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_semi_mission, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_semi_mission, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(india_semi_mission_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-High. The mission extracts resources from taxpayers to subsidize the domestic chip industry. The extraction is asymmetric, as the benefits are concentrated among a few firms. Suppression (0.40): Moderate. The program may suppress competition from foreign chip manufacturers and distort market signals. However, it also aims to address market failures such as underinvestment in R&D and infrastructure. Theater ratio (0.30): Low. The mission is primarily focused on achieving concrete outcomes such as increased domestic chip production and exports, rather than on symbolic gestures.
 *
 * PERSPECTIVAL GAP:
 *   The Indian Taxpayers perspective is a Snare because they have little power to influence the program or exit it. The Domestic Chip Manufacturers perspective is a Rope because they benefit from the incentives and see it as a coordination mechanism. Importing sectors see a tangled rope because while local supply chains would benefit, the costs of imports would rise.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the relationship to the mission. Domestic Chip Manufacturers benefit from subsidies and have arbitrage options. Indian Taxpayers bear the costs and have no exit. Importing Sectors bear costs in the form of increased costs but also benefit from more local supply.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_competitiveness,
    'Will the domestic chip industry become genuinely competitive on the global market, or will it remain dependent on government subsidies?',
    'Longitudinal analysis of export volumes, market share, and profitability of domestic chip manufacturers.',
    'If genuinely competitive, the program will be a successful example of industrial policy. If subsidy-dependent, it will represent a long-term drain on resources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_competitiveness, empirical, 'The long-term competitiveness of the domestic chip industry.').

omega_variable(
    supply_chain_security,
    'How much does domestic chip production improve supply chain security and reduce reliance on foreign suppliers?',
    'Analysis of import dependence before and after the program, as well as assessments of geopolitical risk factors.',
    'If significantly improved, the program will contribute to national security. If minimally improved, the economic costs may outweigh the benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_security, empirical, 'The degree to which domestic chip production improves supply chain security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_semi_mission, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, india_semi_mission, theater_ratio, 0, 0.1).
narrative_ontology:measurement(indi_tr_t5, india_semi_mission, theater_ratio, 5, 0.2).
narrative_ontology:measurement(indi_tr_t10, india_semi_mission, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, india_semi_mission, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(indi_be_t5, india_semi_mission, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(indi_be_t10, india_semi_mission, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_semi_mission, resource_allocation).
narrative_ontology:affects_constraint(india_semi_mission, global_chip_supply).
narrative_ontology:affects_constraint(india_semi_mission, indian_economic_growth).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
