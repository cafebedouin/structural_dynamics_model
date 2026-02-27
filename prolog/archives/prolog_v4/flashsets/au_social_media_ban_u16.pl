% ============================================================================
% CONSTRAINT STORY: au_social_media_ban_u16
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [PROPOSED]
% ============================================================================

:- module(constraint_au_social_media_ban_u16, []).

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
 *   constraint_id: au_social_media_ban_u16
 *   human_readable: Australian Under-16 Social Media Ban
 *   domain: social_technological
 *
 * SUMMARY:
 *   A proposed Australian federal law to ban social media access for children
 *   under the age of 16, enforced through mandatory age verification systems
 *   implemented by platforms. This measure has sparked debate regarding its
 *   potential benefits and drawbacks. Some argue it protects children from
 *   online harm, while others claim it infringes on their rights and limits
 *   their access to information and social interaction. The law's
 *   implementation would require significant changes to platform verification
 *   practices and raise questions about privacy and data security.
 *
 * KEY AGENTS:
 *   - Australian Children Under 16: Primary target (powerless/trapped) - Bears extraction through limited access to social media.
 *   - Parents of U16: Secondary target (moderate/constrained) - Impacted by the government's decision and may be forced to monitor children more closely, however, have agency through some monitoring apps.
 *   - Australian Federal Government: Primary beneficiary (institutional/arbitrage) - Benefits through increased control and perceived protection of children.
 *   - Social Media Platforms: Institutional Actor (institutional/arbitrage) - Bear the costs for implementation but can also benefit from the improved image and reduced liability.
 *   - Analytical Observer: (analytical/analytical) sees all sides of the situation and has no direct stake in its success or failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(au_social_media_ban_u16, 0.55).
domain_priors:suppression_score(au_social_media_ban_u16, 0.7).
domain_priors:theater_ratio(au_social_media_ban_u16, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(au_social_media_ban_u16, extractiveness, 0.55).
narrative_ontology:constraint_metric(au_social_media_ban_u16, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(au_social_media_ban_u16, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(au_social_media_ban_u16, tangled_rope).
narrative_ontology:human_readable(au_social_media_ban_u16, "Australian Under-16 Social Media Ban").
narrative_ontology:topic_domain(au_social_media_ban_u16, "social_technological").

domain_priors:requires_active_enforcement(au_social_media_ban_u16).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, australian_federal_government).
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, social_media_platforms).
narrative_ontology:constraint_victim(au_social_media_ban_u16, australian_children_u16).
narrative_ontology:constraint_victim(au_social_media_ban_u16, parents_of_u16).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of Australian children under 16 who are banned from social media. They are trapped as they have no real way to circumvent this constraint, especially given their age and limited access to resources. High extraction.
constraint_indexing:constraint_classification(au_social_media_ban_u16, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of parents who are both constrained and benefit from the law. They are constrained by the law, but may also benefit from the perceived safety of their children. Tangled Rope due to mixed constraint and coordination function. However, also a possible victim due to reduced autonomy in decisions about their children.
constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the Australian Federal Government. The government benefits through increased control over online content and potential positive public perception. The government sees this as coordination to protect children, with benefits outweighing costs. However, potential loss of support from affected demographics. This is a Rope from the government's perspective.
constraint_indexing:constraint_classification(au_social_media_ban_u16, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of social media platforms who are initially forced to invest in age verification but benefit from a clearer regulatory environment and reduced liability for harmful content affecting children. They can arbitrage the new regulations.
constraint_indexing:constraint_classification(au_social_media_ban_u16, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer views the ban as a Tangled Rope due to its mixed effects: it provides coordination by protecting children but also introduces asymmetric extraction and suppression of freedoms. Benefits some while harming others. Requires active enforcement.
constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(au_social_media_ban_u16_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(au_social_media_ban_u16, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(au_social_media_ban_u16, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(au_social_media_ban_u16, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(au_social_media_ban_u16_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The law extracts from children by limiting access to social media, a significant form of communication and social interaction. Suppression (0.70): High. The ban suppresses children's access to information and limits their online freedoms. Theater ratio (0.30): Low. The law has a genuine aim of protecting children, but there's also a theatrical element to appease concerns around internet safety and appear proactive.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing structural positions of the agents involved. Children see the ban as a pure extraction (Snare), as it limits their freedoms and access to information. Parents have a more mixed perspective (Tangled Rope) due to their dual role as decision-makers and protectors. The government views it as a form of coordination (Rope) to protect children from online harm. The analytical observer recognizes the trade-offs and classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the relationship each agent has to the constraint. Children are victims and experience a high directionality value (d=0.95) due to their limited exit options. The government benefits from increased control and is the primary beneficiary, thus experiencing a low directionality value (d=0.05). Parents have an intermediate position, with a directionality value reflecting their mixed experience of constraint and potential benefit (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as Tangled Rope to account for the mix of extraction and coordination it generates. Classifying it as pure extraction (Snare) would ignore the potential benefits of child protection. Classifying it as pure coordination (Rope) would disregard the limitations of freedom it creates. Tangled Rope captures this nuanced reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    age_verification_accuracy,
    'How accurate and easily circumvented will age verification systems be?',
    'Technical analysis of proposed verification methods and their vulnerability to circumvention.',
    'If verification is easily bypassed, the ban is ineffective and the negative impacts are amplified. If verification is highly accurate, the ban''s suppression effect is more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(age_verification_accuracy, empirical, 'Accuracy of age verification systems.').

omega_variable(
    long_term_impact_on_socialization,
    'What is the long-term impact of restricted social media access on children''s socialization and development?',
    'Longitudinal studies comparing the socialization of children with and without social media access.',
    'If negative impacts are significant, the ban is harmful. If positive or neutral, the ban is beneficial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_impact_on_socialization, empirical, 'Long term impact on socialization.').

omega_variable(
    alternative_platform_migration,
    'To what extent will children migrate to alternative, less regulated platforms?',
    'Tracking of user migration patterns following the implementation of the ban.',
    'If migration is significant, the ban is ineffective and may push children to less safe online environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_migration, empirical, 'Migration to alternative platforms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(au_social_media_ban_u16, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(au_s_tr_t0, au_social_media_ban_u16, theater_ratio, 0, 0.2).
narrative_ontology:measurement(au_s_tr_t5, au_social_media_ban_u16, theater_ratio, 5, 0.3).
narrative_ontology:measurement(au_s_tr_t10, au_social_media_ban_u16, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(au_s_be_t0, au_social_media_ban_u16, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(au_s_be_t5, au_social_media_ban_u16, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(au_s_be_t10, au_social_media_ban_u16, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(au_social_media_ban_u16, enforcement_mechanism).
narrative_ontology:affects_constraint(au_social_media_ban_u16, online_privacy_laws).
narrative_ontology:affects_constraint(au_social_media_ban_u16, internet_censorship).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
