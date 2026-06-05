% ============================================================================
% CONSTRAINT STORY: trump_indian_tariffs_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_indian_tariffs_2026, []).

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
 *   constraint_id: trump_indian_tariffs_2026
 *   human_readable: Trump's Tariff Reduction on India (2026)
 *   domain: economic
 *
 * SUMMARY:
 *   In a hypothetical 2026, a Trump administration reduces tariffs on Indian
 *   goods to 18% after the Modi government agrees to reduce oil purchases
 *   from Russia. This agreement is a complex geopolitical and economic
 *   maneuver with varied impacts on different stakeholders. It represents a
 *   mix of coordination between the US and India, and extraction from Russia
 *   and potentially US domestic industries.
 *
 * KEY AGENTS:
 *   - Indian Exporters: Primary beneficiary (institutional/arbitrage) - Gain better access to the US market.
 *   - US Consumers: Beneficiary (moderate/mobile) - Access cheaper goods.
 *   - Russian Oil Producers: Primary victim (powerless/trapped) - Face pressure due to reduced oil purchases from India.
 *   - US Domestic Industries: Victim (powerless/trapped) - Face increased competition.
 *   - US Government: Negotiator (institutional/constrained) - Balances geopolitical and economic interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_indian_tariffs_2026, 0.55).
domain_priors:suppression_score(trump_indian_tariffs_2026, 0.45).
domain_priors:theater_ratio(trump_indian_tariffs_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_indian_tariffs_2026, tangled_rope).
narrative_ontology:human_readable(trump_indian_tariffs_2026, "Trump's Tariff Reduction on India (2026)").
narrative_ontology:topic_domain(trump_indian_tariffs_2026, "economic").

domain_priors:requires_active_enforcement(trump_indian_tariffs_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, indian_exporters).
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, us_consumers).
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, russian_oil_producers).
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, us_domestic_industries_competing_with_india).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of US domestic industries competing with Indian imports. They are trapped as they cannot easily shift production or compete with potentially lower-priced Indian goods due to reduced tariffs. Immediate impact on their market share.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of Indian exporters who benefit from lower tariffs, gaining better access to the US market. They can arbitrage this situation by increasing exports to the US, leading to higher profits and market share.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical observer sees the tangled rope nature of the agreement. It involves both coordination (trade relations, market access) and extraction (pressure on Russia, impact on US domestic industries). Over the long term, there are complex geopolitical and economic effects.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% US Consumers benefit from lower prices due to the reduced tariffs, and they are mobile since they can easily choose to buy cheaper goods. The mobility helps them easily exit the agreement if needed.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% Perspective of Russian oil producers. They face pressure as India reduces its oil purchases from them, affecting their revenues and market share. They are trapped in a situation with limited alternative buyers in the short term.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_indian_tariffs_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_indian_tariffs_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trump_indian_tariffs_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Moderate level reflecting the extraction of value from Russian oil producers and US industries but some benefit to US consumers and Indian exporters. Suppression: 0.45 - Medium level reflecting the restriction on Russia oil purchase. Theater_ratio: 0.30 - The agreement involves some degree of performative aspects related to geopolitical posturing but also substantive economic and strategic shifts.
 *
 * PERSPECTIVAL GAP:
 *   The agreement is seen as a Snare by US domestic industries and Russian oil producers who bear significant costs. Indian exporters and US consumers view it as a Rope, benefiting from increased trade and lower prices. The Analytical Observer sees the agreement as a Tangled Rope, acknowledging the mix of coordination and extraction inherent in the deal.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are Indian exporters and US consumers, who experience positive economic effects from the trade agreement. The victims are Russian oil producers, who face reduced demand, and US domestic industries, who face increased competition. These roles influence the classification from each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is best classified as a Tangled Rope because it involves both coordination and extraction. While some stakeholders benefit (Indian exporters, US consumers), others face negative consequences (Russian oil producers, US domestic industries). It is not a pure rope because there is a clear extractive aspect. It is not a pure snare because there are coordinating elements as well. The classification reflects the mixed nature of the agreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_stability,
    'To what extent does this trade agreement impact geopolitical stability in the region?',
    'Analysis of diplomatic relations, military activity, and international trade patterns.',
    'Agreement could either foster cooperation or exacerbate existing tensions, affecting long-term economic stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_stability, empirical, 'Impact on geopolitical stability.').

omega_variable(
    us_industry_competitiveness,
    'How significantly does the tariff reduction affect the competitiveness of US domestic industries?',
    'Econometric modeling, industry surveys, and market share analysis.',
    'Could lead to job losses, reduced investment, and a decline in overall US manufacturing output.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_industry_competitiveness, empirical, 'Impact on US Industry Competitiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_indian_tariffs_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trum_tr_t0, trump_indian_tariffs_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trum_tr_t3, trump_indian_tariffs_2026, theater_ratio, 3, 0.25).
narrative_ontology:measurement(trum_tr_t6, trump_indian_tariffs_2026, theater_ratio, 6, 0.3).

% Extraction over time
narrative_ontology:measurement(trum_be_t0, trump_indian_tariffs_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(trum_be_t3, trump_indian_tariffs_2026, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(trum_be_t6, trump_indian_tariffs_2026, base_extractiveness, 6, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_indian_tariffs_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
