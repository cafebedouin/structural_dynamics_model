% ============================================================================
% CONSTRAINT STORY: beehiiv_platform_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beehiiv_platform_model, []).

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
 *   constraint_id: beehiiv_platform_model
 *   human_readable: The Beehiiv Newsletter Platform Business Model
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Beehiiv is a newsletter platform that provides creators with tools for
 *   publishing, audience growth, and monetization. Its business model can be
 *   viewed differently based on the perspective of the user, ranging from a
 *   facilitating rope for Beehiiv itself to a potential snare for smaller
 *   creators.
 *
 * KEY AGENTS:
 *   - Beehiiv Platform: Primary beneficiary (institutional/arbitrage)
 *   - High Profile Newsletter Creators: Secondary beneficiary (moderate/mobile)
 *   - Smaller Newsletter Creators: Primary target (powerless/trapped)
 *   - Alternative Newsletter Platforms: Secondary target (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beehiiv_platform_model, 0.55).
domain_priors:suppression_score(beehiiv_platform_model, 0.4).
domain_priors:theater_ratio(beehiiv_platform_model, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beehiiv_platform_model, extractiveness, 0.55).
narrative_ontology:constraint_metric(beehiiv_platform_model, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(beehiiv_platform_model, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beehiiv_platform_model, tangled_rope).
narrative_ontology:human_readable(beehiiv_platform_model, "The Beehiiv Newsletter Platform Business Model").
narrative_ontology:topic_domain(beehiiv_platform_model, "technological/economic").

domain_priors:requires_active_enforcement(beehiiv_platform_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, beehiiv_platform).
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, high_profile_newsletter_creators).
narrative_ontology:constraint_victim(beehiiv_platform_model, smaller_newsletter_creators).
narrative_ontology:constraint_victim(beehiiv_platform_model, alternative_newsletter_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Beehiiv views its platform model as a rope, facilitating newsletter creation and distribution. They benefit directly from the platform's usage and have arbitrage options through adjusting pricing or features.
constraint_indexing:constraint_classification(beehiiv_platform_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Smaller newsletter creators may perceive the platform model as a snare. They are often trapped within the platform due to audience lock-in and the cost of switching, facing extraction through platform fees and limited control.
constraint_indexing:constraint_classification(beehiiv_platform_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% High-profile creators view the platform as a tangled rope. While benefiting from audience growth and monetization features, they also face some extraction through platform fees and potential content restrictions, but they have mobile exit options due to their brand strength.
constraint_indexing:constraint_classification(beehiiv_platform_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% An analytical observer sees the Beehiiv platform model as a tangled rope. It provides coordination benefits for creators and readers but also extracts value and suppresses alternatives, leading to a mixed classification.
constraint_indexing:constraint_classification(beehiiv_platform_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beehiiv_platform_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beehiiv_platform_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beehiiv_platform_model, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(beehiiv_platform_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beehiiv_platform_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Beehiiv extracts value through fees and potential restrictions, affecting creators. Suppression (0.40): Moderate. Limited platform choice and audience lock-in create suppression. Theater ratio (0.20): Low. The platform primarily functions as intended without significant performative elements.
 *
 * PERSPECTIVAL GAP:
 *   Beehiiv sees a coordinating rope, while smaller creators may experience a snare due to dependence. High-profile creators experience a mix (tangled rope), benefiting from the platform's features but also subject to its control. The analytical observer sees a mixed model with both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the degree to which agents benefit or are extracted from the platform. Beehiiv benefits directly, while smaller creators are extracted from. High profile creators have a mixed directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    platform_lockin_degree,
    'To what extent does audience lock-in create a barrier for creators to switch platforms?',
    'Analyze creator churn rates and the correlation with audience size and engagement.',
    'High lock-in strengthens the snare classification; low lock-in shifts smaller creators towards a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_lockin_degree, empirical, 'The degree of platform lock-in affecting creator mobility.').

omega_variable(
    alternative_platform_viability,
    'How viable are alternative newsletter platforms in terms of features, pricing, and audience reach?',
    'Compare Beehiiv with competitors across key metrics and analyze creator adoption rates.',
    'Increased viability weakens Beehiiv''s extractive power; reduced viability reinforces the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'The viability and competitiveness of alternative platforms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beehiiv_platform_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beeh_tr_t0, beehiiv_platform_model, theater_ratio, 0, 0.1).
narrative_ontology:measurement(beeh_tr_t5, beehiiv_platform_model, theater_ratio, 5, 0.15).
narrative_ontology:measurement(beeh_tr_t10, beehiiv_platform_model, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(beeh_be_t0, beehiiv_platform_model, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(beeh_be_t5, beehiiv_platform_model, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(beeh_be_t10, beehiiv_platform_model, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beehiiv_platform_model, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
