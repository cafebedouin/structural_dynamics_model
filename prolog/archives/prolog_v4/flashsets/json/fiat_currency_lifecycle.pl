% ============================================================================
% CONSTRAINT STORY: fiat_currency_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiat_currency_lifecycle, []).

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
 *   constraint_id: fiat_currency_lifecycle
 *   human_readable: The Lifecycle of a Fiat Currency
 *   domain: economic/political
 *
 * SUMMARY:
 *   This constraint story models the typical lifecycle of an unbacked fiat
 *   currency, from its initial adoption and use to its eventual decline and
 *   potential replacement. Early adopters and the government benefit, while
 *   later adopters and those on fixed incomes bear the costs of inflation.
 *
 * KEY AGENTS:
 *   - Initial Government Issuers: Primary beneficiary (institutional/arbitrage) - Benefits from seigniorage and monetary policy flexibility.
 *   - Early Adopters: Secondary beneficiary (moderate/mobile) - Benefit from increased economic activity and asset appreciation.
 *   - Late Adopters: Primary target (moderate/constrained) - Bear the costs of inflation and devaluation.
 *   - Fixed Income Earners: Primary target (powerless/trapped) - Trapped by contracts denominated in the depreciating currency.
 *   - Holders of Competing Currencies: Secondary target (moderate/mobile) - Suffer losses due to the fiat currency's dominance.
 *   - Central Bank (Late Stage): Institutional actor (institutional/constrained) - May be constrained to maintain the currency's facade.
 *   - Analytical Observer: Neutral observer (analytical/analytical) - Sees the full lifecycle, including both the benefits and the extractive phases.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiat_currency_lifecycle, 0.6).
domain_priors:suppression_score(fiat_currency_lifecycle, 0.5).
domain_priors:theater_ratio(fiat_currency_lifecycle, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiat_currency_lifecycle, extractiveness, 0.6).
narrative_ontology:constraint_metric(fiat_currency_lifecycle, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(fiat_currency_lifecycle, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiat_currency_lifecycle, tangled_rope).
narrative_ontology:human_readable(fiat_currency_lifecycle, "The Lifecycle of a Fiat Currency").
narrative_ontology:topic_domain(fiat_currency_lifecycle, "economic/political").

domain_priors:requires_active_enforcement(fiat_currency_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, initial_government_issuers).
narrative_ontology:constraint_beneficiary(fiat_currency_lifecycle, early_adopters).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, late_adopters).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, fixed_income_earners).
narrative_ontology:constraint_victim(fiat_currency_lifecycle, holders_of_competing_currencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIXED INCOME EARNERS (SNARE) - Trapped by contracts denominated in the depreciating currency, unable to exit without significant loss.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LATE ADOPTERS (TANGLED ROPE) - Constrained by the established network effect but benefit from transactional convenience.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INITIAL GOVERNMENT ISSUERS (ROPE) - Benefits from seigniorage and monetary policy flexibility. See it as coordination.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL BANK (PITON) - At later stages, may be constrained to maintain the currency's facade even when it's functionally failing.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - Sees the full lifecycle, including both the benefits and the extractive phases.
constraint_indexing:constraint_classification(fiat_currency_lifecycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiat_currency_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fiat_currency_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiat_currency_lifecycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fiat_currency_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fiat_currency_lifecycle, TR),
    TR >= 0.70.

:- end_tests(fiat_currency_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Inflation and devaluation transfer wealth from savers and fixed-income earners to the government and early adopters. Suppression (0.50): Moderate. Government regulations and network effects create barriers to adopting alternative currencies. Theater ratio (0.75): High. While there may be public statements supporting the currency, the fundamental economic realities drive its lifecycle. The theater increases over time as the central bank attempts to maintain confidence in a failing currency.
 *
 * PERSPECTIVAL GAP:
 *   The government and early adopters view the currency as a beneficial tool for economic management, while late adopters and those on fixed incomes experience it as a form of extraction. The analytical observer sees the complete cycle and its distributional effects.
 *
 * DIRECTIONALITY LOGIC:
 *   The initial government issuers benefit from the seigniorage (profit made from issuing currency) and the increased monetary policy flexibility a fiat currency offers. Therefore they see the currency's lifecycle as a rope. Fixed income earners are harmed by the inflation the currency generates, and are often trapped by contractual obligations, so they see a snare. Late adopters see a tangled rope as there are benefits and harms.
 *
 * MANDATROPHY ANALYSIS:
 *   The lifecycle of a fiat currency could be mislabeled as a simple snare if only considering those harmed by inflation. However, the initial benefits to the issuing government and early adopters demonstrate that it is initially a tangled rope, offering benefits and extraction, that can degrade over time to a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_confidence,
    'At what level of devaluation does confidence in the currency collapse, triggering hyperinflation or a currency crisis?',
    'Empirical study of past currency crises; econometric modeling of confidence indicators.',
    'Determines the extractiveness experienced by different groups and the classification from their perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_confidence, empirical, 'Threshold for currency confidence collapse.').

omega_variable(
    alternative_currency_accessibility,
    'How accessible are alternative currencies (stablecoins, foreign currencies, barter systems) to different segments of the population?',
    'Surveys of currency usage; analysis of regulatory barriers to alternative currencies.',
    'Determines the exit options available to different groups, influencing their classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_currency_accessibility, empirical, 'Accessibility of alternative currencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiat_currency_lifecycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiat_tr_t0, fiat_currency_lifecycle, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fiat_tr_t5, fiat_currency_lifecycle, theater_ratio, 5, 0.5).
narrative_ontology:measurement(fiat_tr_t10, fiat_currency_lifecycle, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(fiat_be_t0, fiat_currency_lifecycle, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(fiat_be_t5, fiat_currency_lifecycle, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(fiat_be_t10, fiat_currency_lifecycle, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiat_currency_lifecycle, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
