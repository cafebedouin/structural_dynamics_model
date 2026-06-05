% ============================================================================
% CONSTRAINT STORY: sm_addictive_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sm_addictive_design, []).

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
 *   constraint_id: sm_addictive_design
 *   human_readable: Social Media Addictive Design Features
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint represents the set of design principles and algorithmic
 *   features common to modern social media platforms, which are optimized to
 *   maximize user engagement and time-on-platform. These features, such as
 *   infinite scrolling, personalized notifications, and variable rewards, can
 *   lead to addictive behaviors and negative impacts on users' mental health,
 *   productivity, and social relationships. While the platforms benefit from
 *   increased user engagement and advertising revenue, the individual users
 *   and society as a whole may bear the costs.
 *
 * KEY AGENTS:
 *   - Social Media Platforms: Primary beneficiary (institutional/arbitrage) - drive revenue from increased user engagement.
 *   - Advertisers: Secondary beneficiary (moderate/constrained) - benefit from targeted advertising opportunities.
 *   - Individual Users: Primary victim (powerless/trapped) - face addiction, mental health issues and productivity loss.
 *   - Society Wellbeing: Primary victim (powerless/trapped) - experience erosion of social discourse and productivity.
 *   - Regulatory Bodies: Secondary Agents (organized/constrained) - Aim to balance user protection with platform innovation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sm_addictive_design, 0.65).
domain_priors:suppression_score(sm_addictive_design, 0.7).
domain_priors:theater_ratio(sm_addictive_design, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sm_addictive_design, extractiveness, 0.65).
narrative_ontology:constraint_metric(sm_addictive_design, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sm_addictive_design, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sm_addictive_design, tangled_rope).
narrative_ontology:human_readable(sm_addictive_design, "Social Media Addictive Design Features").
narrative_ontology:topic_domain(sm_addictive_design, "technological").

domain_priors:requires_active_enforcement(sm_addictive_design).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sm_addictive_design, social_media_platforms).
narrative_ontology:constraint_beneficiary(sm_addictive_design, advertisers).
narrative_ontology:constraint_victim(sm_addictive_design, individual_users).
narrative_ontology:constraint_victim(sm_addictive_design, society_wellbeing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual user often feels trapped by the addictive nature of the platform, struggling to reduce their usage despite negative impacts on their well-being. High perceived extraction and limited exit options.
constraint_indexing:constraint_classification(sm_addictive_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Social media platforms benefit from increased user engagement, which drives advertising revenue. They coordinate user activity and attention, facilitating connections and content sharing, which is perceived as a beneficial service with easy exit options.
constraint_indexing:constraint_classification(sm_addictive_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Advertisers benefit from the platforms' ability to capture user attention, but are also constrained by the platforms' rules and the potential for negative backlash if ads are too intrusive or manipulative. They experience a mix of coordination and extraction.
constraint_indexing:constraint_classification(sm_addictive_design, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Regulatory bodies are constrained by the platforms' power and lobbying efforts, but also aim to protect users from harmful design features. They try to steer platform design while dealing with constrained resources and powers. They have some agency to improve the situation but aren't powerless.
constraint_indexing:constraint_classification(sm_addictive_design, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees the mixed benefits of social media alongside its extractive elements, noting the active enforcement mechanisms that uphold the system and the beneficiaries who profit from it. Long-term view reveals the unintended consequences of algorithms optimized for engagement.
constraint_indexing:constraint_classification(sm_addictive_design, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sm_addictive_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sm_addictive_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sm_addictive_design, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sm_addictive_design, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sm_addictive_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extraction value (0.65) indicates the platforms capture a significant portion of user attention and time. Suppression is also high (0.70) due to the reinforcing feedback loops and lack of alternatives once users are deeply engaged. The theater ratio is moderate (0.30), reflecting some genuine utility alongside manipulative aspects. The claimed type is Tangled Rope because the addictive features serve a coordination purpose (connecting users and content) but with extractive consequences.
 *
 * PERSPECTIVAL GAP:
 *   Individual users often experience social media as a Snare, feeling trapped and exploited by addictive features. Social media platforms perceive their services as ropes, providing useful services that connect people and enable information sharing. Advertisers see a tangled rope, benefiting from increased ad opportunities, while being dependent on the platform. The analytical observer sees the tension between the platform's beneficial coordination function and the extractive outcomes for individual users and society, indicating Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the beneficiaries and victims. Social media platforms benefit from addictive design, while individual users are the target of extraction. Advertisers benefit but are also reliant on the platforms' rules. Regulatory bodies attempt to act as an organized force that can escape this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impact_on_mental_health,
    'What is the true causal impact of addictive design features on users'' mental health and well-being, controlling for pre-existing conditions?',
    'Longitudinal studies with control groups, measuring mental health outcomes before and after exposure to specific design features.',
    'If the impact is severe, stronger regulation may be justified. If the impact is minor, the platforms may be able to self-regulate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_mental_health, empirical, 'Causal impact on mental health.').

omega_variable(
    self_regulation_efficacy,
    'Can social media platforms effectively self-regulate to mitigate the harmful effects of addictive design features?',
    'Track the implementation of voluntary changes by platforms and measure the resulting impact on user behavior and well-being.',
    'If self-regulation is effective, government intervention may be unnecessary. If self-regulation fails, stricter regulation will be required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_regulation_efficacy, empirical, 'Efficacy of self-regulation.').

omega_variable(
    defining_addiction,
    'What constitutes ''addictive design'' and what level of platform influence counts as coercive?',
    'Expert panels defining thresholds or case law and jurisprudence establishing platform responsibilities in this domain.',
    'The precise definition of ''addiction'' will determine which design features are considered harmful and subject to regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defining_addiction, conceptual, 'Definition of addictive design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sm_addictive_design, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sm_a_tr_t0, sm_addictive_design, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sm_a_tr_t5, sm_addictive_design, theater_ratio, 5, 0.2).
narrative_ontology:measurement(sm_a_tr_t10, sm_addictive_design, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(sm_a_be_t0, sm_addictive_design, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sm_a_be_t5, sm_addictive_design, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(sm_a_be_t10, sm_addictive_design, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sm_addictive_design, information_standard).
narrative_ontology:affects_constraint(sm_addictive_design, attention_economy).
narrative_ontology:affects_constraint(sm_addictive_design, filter_bubble).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
