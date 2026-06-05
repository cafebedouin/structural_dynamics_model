% ============================================================================
% CONSTRAINT STORY: platform_app_store_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_app_store_duopoly, []).

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
 *   constraint_id: platform_app_store_duopoly
 *   human_readable: Platform Mandate for Proprietary App Stores and In-App Payments
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint models the mobile operating system duopoly (Apple's iOS
 *   and Google's Android) that requires app developers to use proprietary app
 *   stores for distribution and in-app payment systems, which charge
 *   commissions of 15-30%. This mandate creates a complex relationship
 *   between the platform owners, app developers, and consumers, with elements
 *   of both coordination and extraction.
 *
 * KEY AGENTS:
 *   - Apple and Google: Primary beneficiaries (institutional/arbitrage) — derive revenue from commissions and control over the app ecosystem.
 *   - App Developers: Primary victims (powerless/trapped) — face high commissions and restrictions on distribution.
 *   - Consumers: Secondary victims (moderate/constrained) – may face higher prices due to commissions and have limited choices for app distribution.
 *   - Large App Development Companies: Powerful actors (powerful/mobile) – can negotiate deals or pursue alternative distribution strategies
 *   - Analytical Observer: Sees mixed coordination and extraction (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_app_store_duopoly, 0.6).
domain_priors:suppression_score(platform_app_store_duopoly, 0.7).
domain_priors:theater_ratio(platform_app_store_duopoly, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_app_store_duopoly, extractiveness, 0.6).
narrative_ontology:constraint_metric(platform_app_store_duopoly, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(platform_app_store_duopoly, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_app_store_duopoly, tangled_rope).
narrative_ontology:human_readable(platform_app_store_duopoly, "Platform Mandate for Proprietary App Stores and In-App Payments").
narrative_ontology:topic_domain(platform_app_store_duopoly, "technological").

domain_priors:requires_active_enforcement(platform_app_store_duopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, apple).
narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, google).
narrative_ontology:constraint_victim(platform_app_store_duopoly, app_developers).
narrative_ontology:constraint_victim(platform_app_store_duopoly, consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual app developers are largely trapped within the duopoly, facing significant barriers to reaching users outside of the official app stores. This lack of viable alternatives makes the constraint a snare from their perspective. They are effectively powerless in the face of platform policies.
constraint_indexing:constraint_classification(platform_app_store_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Medium-sized app development companies have some leverage due to their existing user base and brand recognition, but are still significantly constrained by the duopoly's rules. They benefit from access to a large user base but suffer from the extraction of in-app purchase commissions and restrictions on alternative payment methods. They have some options for exiting the ecosystem (e.g., web apps), but these are often less desirable than native apps.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Apple and Google benefit from the control over their app stores and in-app payment systems. This control provides them with significant revenue streams, data collection opportunities, and the ability to curate the user experience. From their perspective, the mandate functions as a rope, coordinating a standardized and secure platform ecosystem. They can arbitrage between competing interests.
constraint_indexing:constraint_classification(platform_app_store_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Large app development companies like Spotify and Epic have enough power to challenge the duopoly's mandates. They can negotiate with the platform owners, develop alternative distribution methods (e.g., direct downloads), or even launch legal challenges. While they benefit from access to the platforms' user base, they also experience the constraint as an extraction mechanism due to high commissions and restrictions on payment options. They are mobile in that they can pursue alternative strategies.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% An analytical observer recognizes the duopoly's platform mandate as a tangled rope. It has elements of coordination (creating a standardized ecosystem and ensuring security), but also significant extraction (in-app purchase commissions) and suppression of alternatives (restrictions on distribution and payment methods).
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_app_store_duopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_app_store_duopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_app_store_duopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_app_store_duopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(platform_app_store_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The 15-30% commission on in-app purchases represents a significant extraction of revenue from app developers. Suppression (0.70): High. The restrictions on alternative app distribution channels and payment methods limit developer autonomy and consumer choice. Theater ratio (0.30): Low. While there is some rhetoric about ensuring security and user experience, the primary motivation for the mandate is revenue generation.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between Apple/Google (who see the mandate as a legitimate form of coordination) and app developers (who experience it as an extraction mechanism). Large app development companies have more agency and see a mixed picture, while individual developers are largely trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Apple and Google benefit from the mandate, as it generates significant revenue and control over the app ecosystem. App developers, especially smaller ones, are the primary victims, as they face high commissions and restrictions. Larger app developers have more bargaining power and can negotiate better terms, while consumers are indirectly affected through higher prices and limited choices.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the platform mandate has elements of both coordination and extraction. The platform owners coordinate the app ecosystem and ensure a certain level of security and user experience. However, they also extract significant revenue from app developers through commissions and restrictions on alternative distribution and payment methods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competition_viability,
    'How viable are alternative app distribution channels (e.g., third-party app stores, web apps) as a means of breaking the duopoly''s control?',
    'Market share analysis of alternative app distribution channels; user surveys on the adoption of alternative apps; developer interviews on the costs and benefits of using alternative channels.',
    'If alternative channels become viable, the constraint could shift from a tangled rope to a rope, as developers gain more bargaining power. If alternative channels remain niche, the constraint will remain a tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competition_viability, empirical, 'Viability of alternative app distribution channels.').

omega_variable(
    regulation_impact,
    'How will government regulation impact the duopoly''s platform mandate?',
    'Analysis of ongoing antitrust lawsuits and regulatory investigations; assessment of the impact of new laws and regulations on app store policies; tracking changes in platform policies in response to regulatory pressure.',
    'If regulation leads to significant changes in platform policies (e.g., allowing alternative payment methods), the extraction from app developers could decrease. If regulation has little impact, the constraint will remain a tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_impact, conceptual, 'Impact of government regulation on platform mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_app_store_duopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_tr_t0, platform_app_store_duopoly, theater_ratio, 0, 0.2).
narrative_ontology:measurement(plat_tr_t5, platform_app_store_duopoly, theater_ratio, 5, 0.3).
narrative_ontology:measurement(plat_tr_t10, platform_app_store_duopoly, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(plat_be_t0, platform_app_store_duopoly, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(plat_be_t5, platform_app_store_duopoly, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(plat_be_t10, platform_app_store_duopoly, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_app_store_duopoly, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
