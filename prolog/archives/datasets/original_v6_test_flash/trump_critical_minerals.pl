% ============================================================================
% CONSTRAINT STORY: trump_critical_minerals
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_critical_minerals, []).

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
 *   constraint_id: trump_critical_minerals
 *   human_readable: Trump Critical Minerals Stockpile Project
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Trump administration initiated a project to stockpile critical
 *   minerals, aiming to reduce reliance on foreign suppliers, particularly
 *   China. This project aimed to enhance national security and support
 *   domestic mining industries, but it also raised concerns about economic
 *   protectionism and trade relations.
 *
 * KEY AGENTS:
 *   - Domestic Mining Companies: Beneficiaries of increased demand and government contracts.
 *   - Defense Industry: Beneficiaries of a secure supply of critical minerals.
 *   - Taxpayers: Victims who bear the cost of the stockpile through increased taxes.
 *   - International Trade Partners: Victims who face constrained access to the US market.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_critical_minerals, 0.55).
domain_priors:suppression_score(trump_critical_minerals, 0.45).
domain_priors:theater_ratio(trump_critical_minerals, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_critical_minerals, extractiveness, 0.55).
narrative_ontology:constraint_metric(trump_critical_minerals, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(trump_critical_minerals, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_critical_minerals, tangled_rope).
narrative_ontology:human_readable(trump_critical_minerals, "Trump Critical Minerals Stockpile Project").
narrative_ontology:topic_domain(trump_critical_minerals, "economic/political").

domain_priors:requires_active_enforcement(trump_critical_minerals).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_critical_minerals, domestic_mining_companies).
narrative_ontology:constraint_beneficiary(trump_critical_minerals, defense_industry).
narrative_ontology:constraint_victim(trump_critical_minerals, taxpayers).
narrative_ontology:constraint_victim(trump_critical_minerals, international_trade_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Taxpayers bear the cost of the stockpile through increased taxes or reallocation of resources, with little direct benefit and no exit option.
constraint_indexing:constraint_classification(trump_critical_minerals, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% International trade partners face constrained access to the US market for critical minerals, which can disrupt trade relationships and economic stability.
constraint_indexing:constraint_classification(trump_critical_minerals, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Domestic mining companies benefit from increased demand and government contracts, leading to higher profits and market share. They can arbitrage government support.
constraint_indexing:constraint_classification(trump_critical_minerals, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The defense industry benefits from a secure supply of critical minerals, reducing reliance on foreign sources. They can arbitrage this security into reliable production.
constraint_indexing:constraint_classification(trump_critical_minerals, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The stockpile project presents a complex mix of strategic security, economic protectionism, and potential market distortion.
constraint_indexing:constraint_classification(trump_critical_minerals, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_critical_minerals_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_critical_minerals, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_critical_minerals, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_critical_minerals, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trump_critical_minerals_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness value (0.55) reflects the cost to taxpayers and potential disruption to international trade. The suppression value (0.45) accounts for the limited exit options for affected parties. The theater ratio (0.30) is relatively low, indicating that the project has a genuine strategic purpose.
 *
 * PERSPECTIVAL GAP:
 *   The stockpile project is viewed differently by various actors. Domestic industries see it as a positive step towards economic security, while taxpayers and international partners may perceive it as an unfair burden or trade barrier. The analytical observer recognizes the complex trade-offs involved.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic is based on the structural relationship between the agents and the constraint. Beneficiaries, such as domestic mining companies, experience low directionality, while victims, such as taxpayers and international trade partners, experience high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification is appropriate because the project combines elements of coordination (securing mineral supply) and extraction (cost to taxpayers and trade partners). It prevents mislabeling as pure extraction by acknowledging the strategic value of the stockpile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_stockpile,
    'Will the stockpile effectively mitigate supply chain vulnerabilities and reduce reliance on foreign suppliers?',
    'Analysis of stockpile composition, storage capacity, and potential release mechanisms during supply disruptions.',
    'If effective, the project will enhance national security and economic resilience. If ineffective, it will waste resources and create artificial market distortions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_stockpile, empirical, 'The effectiveness of the stockpile in mitigating supply chain vulnerabilities.').

omega_variable(
    economic_impact_on_trade_partners,
    'What is the magnitude of the economic impact on international trade partners, particularly those with established supply relationships?',
    'Economic modeling of trade flows, analysis of tariff policies, and evaluation of alternative sourcing strategies.',
    'If the impact is significant, it could lead to trade disputes and retaliatory measures. If minimal, the project may be viewed as a reasonable measure to enhance national security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_on_trade_partners, empirical, 'The economic impact on international trade partners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_critical_minerals, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trum_tr_t0, trump_critical_minerals, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trum_tr_t2, trump_critical_minerals, theater_ratio, 2, 0.25).
narrative_ontology:measurement(trum_tr_t4, trump_critical_minerals, theater_ratio, 4, 0.3).

% Extraction over time
narrative_ontology:measurement(trum_be_t0, trump_critical_minerals, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(trum_be_t2, trump_critical_minerals, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(trum_be_t4, trump_critical_minerals, base_extractiveness, 4, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_critical_minerals, resource_allocation).
narrative_ontology:affects_constraint(trump_critical_minerals, us_china_trade_relations).
narrative_ontology:affects_constraint(trump_critical_minerals, global_mineral_supply_chains).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
