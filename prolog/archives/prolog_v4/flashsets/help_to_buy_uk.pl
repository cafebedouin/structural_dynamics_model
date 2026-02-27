% ============================================================================
% CONSTRAINT STORY: help_to_buy_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_help_to_buy_uk, []).

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
 *   constraint_id: help_to_buy_uk
 *   human_readable: UK 'Help to Buy' Equity Loan Scheme
 *   domain: economic
 *
 * SUMMARY:
 *   The UK 'Help to Buy' Equity Loan Scheme aimed to assist first-time buyers
 *   by providing an equity loan from the government. However, the scheme also
 *   benefited property developers by stimulating demand and increasing
 *   property prices. This created a complex dynamic with both coordination
 *   and extraction elements. The scheme ran from 2013 to 2023.
 *
 * KEY AGENTS:
 *   - Property Developers: Primary beneficiary (institutional/arbitrage) - benefit from increased sales and higher prices.
 *   - Low-Income Borrowers: Primary victim (powerless/trapped) - face rising debt and potential long-term affordability issues.
 *   - Future Taxpayers: Secondary victim (moderate/constrained) - bear the risk of government losses if property values decline.
 *   - High Income First Time Buyers: Secondary beneficiary (institutional/arbitrage) - can afford higher value properties with this scheme.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(help_to_buy_uk, 0.55).
domain_priors:suppression_score(help_to_buy_uk, 0.45).
domain_priors:theater_ratio(help_to_buy_uk, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(help_to_buy_uk, extractiveness, 0.55).
narrative_ontology:constraint_metric(help_to_buy_uk, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(help_to_buy_uk, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(help_to_buy_uk, tangled_rope).
narrative_ontology:human_readable(help_to_buy_uk, "UK 'Help to Buy' Equity Loan Scheme").
narrative_ontology:topic_domain(help_to_buy_uk, "economic").

domain_priors:requires_active_enforcement(help_to_buy_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(help_to_buy_uk, property_developers).
narrative_ontology:constraint_beneficiary(help_to_buy_uk, high_income_first_time_buyers).
narrative_ontology:constraint_victim(help_to_buy_uk, low_income_borrowers).
narrative_ontology:constraint_victim(help_to_buy_uk, future_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low-income borrowers are trapped by the scheme due to rising property values and interest accrual, increasing their debt burden over time.
constraint_indexing:constraint_classification(help_to_buy_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Property developers benefit from increased demand and sales, facilitated by the scheme.
constraint_indexing:constraint_classification(help_to_buy_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The scheme acts as a tangled rope, providing coordination for first-time buyers but also extracting value through increased property prices and future taxpayer burden.
constraint_indexing:constraint_classification(help_to_buy_uk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% High income first time buyers are able to leverage the scheme for more desirable properties or lower initial payments, essentially arbitrage.
constraint_indexing:constraint_classification(help_to_buy_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(help_to_buy_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(help_to_buy_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(help_to_buy_uk, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(help_to_buy_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(help_to_buy_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The scheme extracts value from future taxpayers through potential losses on equity loans and from low-income borrowers facing affordability challenges. Suppression (0.45): Moderate. The scheme suppresses alternative housing options and potentially traps borrowers into long-term debt. Theater ratio (0.30): Low. While there is some political theater surrounding the scheme's 'success', its primary function is to stimulate the housing market.
 *
 * PERSPECTIVAL GAP:
 *   Low-income borrowers experience the scheme as a snare due to the risk of rising debt and limited exit options. Property developers see it as a rope, facilitating increased sales and profits. Analytical observers recognize the tangled rope nature of the scheme, balancing coordination benefits with extraction risks. The high income first time buyers perspective sees it as a rope because they are able to arbitrage the scheme.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the agent's structural position. Property developers and high income first time buyers, as beneficiaries with arbitrage options, have low 'd' values and experience the scheme as a rope. Low-income borrowers, as victims with limited exit options, have high 'd' values and experience the scheme as a snare. Future taxpayers bear the risk of potential losses and have a moderate 'd' value.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'Help to Buy' scheme resolves the mandatrophy by highlighting the different perspectives on the same structural phenomenon. While it aims to coordinate first-time buyers with the housing market, it also extracts value through increased prices and taxpayer burden. The analytical observer identifies the tangled rope, balancing the coordination and extraction aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_affordability,
    'What is the long-term affordability of the scheme for low-income borrowers, considering rising property values and interest accrual?',
    'Longitudinal study tracking the financial stability of scheme participants over 10-15 years.',
    'If unaffordable: Scheme is a snare for low-income borrowers. If affordable: Scheme is a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_affordability, empirical, 'Long-term affordability for low-income borrowers.').

omega_variable(
    impact_on_property_prices,
    'To what extent did the scheme contribute to overall property price inflation, benefiting developers but harming affordability for non-participants?',
    'Econometric analysis comparing property price trends in areas with high vs. low scheme participation.',
    'If significant inflation: Scheme is a tangled rope. If minimal inflation: Scheme is closer to a pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_property_prices, empirical, 'Impact on overall property prices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(help_to_buy_uk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(help_tr_t0, help_to_buy_uk, theater_ratio, 0, 0.15).
narrative_ontology:measurement(help_tr_t5, help_to_buy_uk, theater_ratio, 5, 0.25).
narrative_ontology:measurement(help_tr_t10, help_to_buy_uk, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(help_be_t0, help_to_buy_uk, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(help_be_t5, help_to_buy_uk, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(help_be_t10, help_to_buy_uk, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(help_to_buy_uk, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
