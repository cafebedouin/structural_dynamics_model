% ============================================================================
% CONSTRAINT STORY: fb_creator_monetization_indonesia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fb_creator_monetization_indonesia, []).

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
 *   constraint_id: fb_creator_monetization_indonesia
 *   human_readable: Facebook/Meta's 'Performance Bonus' Monetization Program for Indonesian Creators
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Meta's Performance Bonus program aims to incentivize content creation in
 *   Indonesia by offering monetization opportunities. However, this program
 *   creates a complex dynamic where creators are incentivized to maximize
 *   engagement, potentially at the expense of content quality and
 *   authenticity. Emerging creators often struggle to gain visibility, while
 *   Meta benefits from increased platform activity. The program exhibits
 *   characteristics of a tangled rope, balancing coordination and extraction.
 *
 * KEY AGENTS:
 *   - Meta Platforms: Primary beneficiary (institutional/arbitrage) - Gains increased content and engagement.
 *   - Successful Content Creators Indonesia: Secondary beneficiary (moderate/mobile) - Benefits from monetization but faces algorithmic constraints.
 *   - Emerging Content Creators Indonesia: Primary victim (powerless/trapped) - Struggles to gain visibility and monetization.
 *   - Facebook Users Indonesia: Secondary victim (powerful/constrained) - Exposed to potentially lower quality, engagement-optimized content.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fb_creator_monetization_indonesia, 0.55).
domain_priors:suppression_score(fb_creator_monetization_indonesia, 0.4).
domain_priors:theater_ratio(fb_creator_monetization_indonesia, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, extractiveness, 0.55).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fb_creator_monetization_indonesia, tangled_rope).
narrative_ontology:human_readable(fb_creator_monetization_indonesia, "Facebook/Meta's 'Performance Bonus' Monetization Program for Indonesian Creators").
narrative_ontology:topic_domain(fb_creator_monetization_indonesia, "technological/economic").

domain_priors:requires_active_enforcement(fb_creator_monetization_indonesia).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, meta_platforms).
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, successful_content_creators_indonesia).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, emerging_content_creators_indonesia).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, facebook_users_indonesia).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Emerging creators often find themselves trapped in the pursuit of monetization, heavily reliant on Facebook's algorithm and platform rules, with little recourse if their content is not promoted or if their monetization is revoked. They lack the resources and influence to effectively exit or negotiate better terms. Feels the full extractive pressure.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Meta benefits from increased content creation and user engagement on its platform, driving advertising revenue. It has significant arbitrage power due to its global reach and control over the platform's algorithm and monetization policies. Experiences the constraint as a positive coordination mechanism.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Successful creators benefit from monetization opportunities but are also subject to platform policies and algorithmic changes, creating a mix of coordination and extraction. They can potentially move to other platforms but risk losing their existing audience. Experiences both coordination and extraction.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Users are increasingly exposed to content primarily designed to maximize engagement for monetization purposes, which can degrade the quality and authenticity of content on the platform. User's choice on what to view is limited by the engagement-optimized algorithm, yet the program benefits users by providing more content.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The monetization program presents a complex interplay of coordination and extraction. While creators gain income and Facebook gains engagement, emerging creators and platform users bear costs through algorithmic manipulation and reduced content quality. This exhibits properties of a tangled rope.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fb_creator_monetization_indonesia_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fb_creator_monetization_indonesia, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fb_creator_monetization_indonesia, TR),
    TR >= 0.70.

:- end_tests(fb_creator_monetization_indonesia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The program extracts value from creators in the form of increased platform engagement and content creation, but also provides monetization opportunities. Suppression (0.40): Moderate. Facebook's algorithm controls content visibility, suppressing the reach of creators who don't adhere to its engagement-optimized rules. Theater ratio (0.30): Low. There's some performative aspect, but not high. Not a lot of visible theatrics.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different structural positions of each agent. Meta sees a positive coordination mechanism, increasing platform activity and revenue. Successful creators experience a mix of coordination and extraction, benefiting from monetization but subject to algorithmic constraints. Emerging creators feel trapped, struggling to gain visibility. Analytical Observer witnesses a mixed constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the relationship between the agents and the extraction flow. Meta benefits, receiving a low 'd' value. Emerging creators bear the costs, leading to a high 'd' value. The algorithmic nature of content promotion determines the distribution of benefit and pain.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing the dual nature of the program. It coordinates content creation but also extracts value through algorithmic control and potential degradation of content quality. This makes it distinct from pure extraction or pure coordination. Without the active participation of the Indonesian content creators there would be no Facebook content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_fairness,
    'To what extent is Facebook''s algorithm transparent and unbiased in promoting content for monetization?',
    'Independent audits of the algorithm''s performance across different content types and creator demographics; analysis of content reach and engagement patterns.',
    'If unfair: Extraction disproportionately affects certain creators. If fair: Program primarily serves as a coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_fairness, empirical, 'Fairness of Facebook''s algorithm in content promotion.').

omega_variable(
    content_authenticity,
    'Does the monetization incentive lead to a decline in content authenticity and quality?',
    'User surveys on perceived content quality and authenticity; analysis of content characteristics (e.g., sensationalism, clickbait) over time.',
    'If significant decline: Extraction harms the user experience. If minimal decline: Monetization primarily incentivizes creation of valuable content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_authenticity, empirical, 'Impact of monetization on content authenticity and quality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fb_creator_monetization_indonesia, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fb_c_tr_t0, fb_creator_monetization_indonesia, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fb_c_tr_t3, fb_creator_monetization_indonesia, theater_ratio, 3, 0.25).
narrative_ontology:measurement(fb_c_tr_t6, fb_creator_monetization_indonesia, theater_ratio, 6, 0.3).

% Extraction over time
narrative_ontology:measurement(fb_c_be_t0, fb_creator_monetization_indonesia, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fb_c_be_t3, fb_creator_monetization_indonesia, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(fb_c_be_t6, fb_creator_monetization_indonesia, base_extractiveness, 6, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fb_creator_monetization_indonesia, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
