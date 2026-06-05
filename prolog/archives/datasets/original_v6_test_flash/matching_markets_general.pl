% ============================================================================
% CONSTRAINT STORY: matching_markets_general
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_matching_markets_general, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: matching_markets_general
 *   human_readable: Matching Markets (Non-Commodity Exchange)
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Matching markets are non-commodity exchange systems where price is not
 *   the primary market-clearing mechanism. They are often used for matching
 *   individuals to schools, hospitals to doctors, or organ donors to
 *   recipients. These markets involve 'thick' preferences, where the choice
 *   is mutual and the preferences of all parties must be considered.
 *
 * KEY AGENTS:
 *   - market_participants: Beneficiaries (moderate/mobile)
 *   - market_designer: Beneficiary (institutional/analytical)
 *   - analytical_observer: (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(matching_markets_general, 0.35).
domain_priors:suppression_score(matching_markets_general, 0.2).
domain_priors:theater_ratio(matching_markets_general, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(matching_markets_general, extractiveness, 0.35).
narrative_ontology:constraint_metric(matching_markets_general, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(matching_markets_general, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(matching_markets_general, rope).
narrative_ontology:human_readable(matching_markets_general, "Matching Markets (Non-Commodity Exchange)").
narrative_ontology:topic_domain(matching_markets_general, "economic/social/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(matching_markets_general, market_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Participants benefit from the coordination provided by the matching market, increasing the probability of finding a suitable match.
constraint_indexing:constraint_classification(matching_markets_general, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Designers benefit from the successful deployment of the matching market, gaining reputation and influence in the field.
constraint_indexing:constraint_classification(matching_markets_general, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% The analytical observer sees the matching market as a successful coordination mechanism.
constraint_indexing:constraint_classification(matching_markets_general, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(matching_markets_general_tests).
:- end_tests(matching_markets_general_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the market is designed to benefit all participants by increasing the likelihood of a successful match. Suppression is moderate because participants may have limited options outside the matching market.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives view the matching market as a coordination mechanism, with slight differences in the benefits perceived by individual participants and the market designer.
 *
 * DIRECTIONALITY LOGIC:
 *   All actors see the constraint as a rope since all benefit from coordination.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(matching_markets_general, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(matching_markets_general, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
