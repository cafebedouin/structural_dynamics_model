% ============================================================================
% CONSTRAINT STORY: open_culture_newsletter
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_culture_newsletter, []).

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
 *   constraint_id: open_culture_newsletter
 *   human_readable: The 'Free Newsletter for Email' Exchange
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint models the common online value exchange where a user
 *   provides their email address to a platform (Open Culture) in return for
 *   access to a curated newsletter. Users gain access to valuable content but
 *   sacrifice attention and potentially privacy. Platforms gain user data for
 *   marketing and engagement but are responsible for creating and delivering
 *   content.
 *
 * KEY AGENTS:
 *   - Open Culture Platform: Primary beneficiary (institutional/arbitrage) - Gains access to user data.
 *   - User Attention: Primary victim (powerless/trapped) - Subject to increased email volume and marketing messages.
 *   - User Privacy: Secondary victim (powerless/trapped) - Email may be shared and used for tracking.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_culture_newsletter, 0.45).
domain_priors:suppression_score(open_culture_newsletter, 0.3).
domain_priors:theater_ratio(open_culture_newsletter, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_culture_newsletter, extractiveness, 0.45).
narrative_ontology:constraint_metric(open_culture_newsletter, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(open_culture_newsletter, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_culture_newsletter, tangled_rope).
narrative_ontology:human_readable(open_culture_newsletter, "The 'Free Newsletter for Email' Exchange").
narrative_ontology:topic_domain(open_culture_newsletter, "technological/economic").

domain_priors:requires_active_enforcement(open_culture_newsletter).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_culture_newsletter, open_culture_platform).
narrative_ontology:constraint_victim(open_culture_newsletter, user_attention).
narrative_ontology:constraint_victim(open_culture_newsletter, user_privacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Average User (Snare) - Feels trapped due to the overwhelming number of newsletters and marketing emails they receive, leading to diminished attention and privacy. Exiting one newsletter often leads to being targeted by others.
constraint_indexing:constraint_classification(open_culture_newsletter, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: The Open Culture Platform (Rope) - Benefits from access to users' email addresses for marketing and engagement. They can leverage this information to build a community and improve their offerings.
constraint_indexing:constraint_classification(open_culture_newsletter, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: The Analytical Observer (Tangled Rope) - Recognizes the exchange as a mixed bag. Users gain access to valuable content, but their attention and privacy are extracted to some extent. The platform gains access to user data, but must also invest in providing a worthwhile newsletter.
constraint_indexing:constraint_classification(open_culture_newsletter, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_culture_newsletter_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_culture_newsletter, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_culture_newsletter, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(open_culture_newsletter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate extraction as user provides email data. Suppression (0.30): Moderate, as user is able to unsubscribe, however, this does little to alleviate email-related marketing from similar outlets. Theater ratio (0.20): Low, as the newsletter provides genuine educational value, not a high percentage of performance to actual function.
 *
 * PERSPECTIVAL GAP:
 *   The average user perceives a snare because their attention and privacy are extracted in exchange for something they may not value as much as initially thought. The platform perceives a rope because the email addresses they collect are instrumental in building relationships with subscribers. Analytical observer sees tangled rope because both a user and the platform extract from the other, while providing a legitimate function.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform benefits from user data (low d). User's attention and privacy are targeted (high d). Analytical observer sees mixed effects (medium d).
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic instance of the kind of exchange which can be seen as an example of coordination but is vulnerable to exploitation. There exists the potential of seeing the exchange as an example of a mutually beneficial exchange, although the average user is subject to the extractive properties of it as well.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_valuation_accuracy,
    'How accurately do users assess the long-term cost to their attention and privacy when subscribing?',
    'Longitudinal studies tracking user engagement and privacy preferences.',
    'If valuation is accurate, the exchange approaches a rope. If inaccurate (users underestimate costs), it''s closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_valuation_accuracy, empirical, 'User valuation of their attention and privacy in exchange for newsletter content.').

omega_variable(
    platform_content_quality,
    'How valuable is the content provided by the platform?',
    'User satisfaction surveys and content engagement metrics.',
    'Higher quality content strengthens the coordination aspect, lower quality strengthens the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_content_quality, empirical, 'The quality and relevance of the content provided by the Open Culture newsletter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_culture_newsletter, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(open_tr_t0, open_culture_newsletter, theater_ratio, 0, 0.1).
narrative_ontology:measurement(open_tr_t5, open_culture_newsletter, theater_ratio, 5, 0.2).
narrative_ontology:measurement(open_tr_t10, open_culture_newsletter, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(open_be_t0, open_culture_newsletter, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(open_be_t5, open_culture_newsletter, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(open_be_t10, open_culture_newsletter, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_culture_newsletter, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
