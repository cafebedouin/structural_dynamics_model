% ============================================================================
% CONSTRAINT STORY: epistemic_overload_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_overload_collapse, []).

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
 *   constraint_id: epistemic_overload_collapse
 *   human_readable: The Signal-Drowning Vortex
 *   domain: cognitive/informational/technological
 *
 * SUMMARY:
 *   The Signal-Drowning Vortex describes the phenomenon where the sheer
 *   volume and velocity of information exceed an individual's capacity to
 *   process it, leading to cognitive overload and impaired decision-making.
 *   This is exacerbated by contradictory and often misleading information
 *   sources. The rise of social media and the attention economy have
 *   accelerated this process. Traditional media's role in filtering
 *   information has been undermined by alternative sources.
 *
 * KEY AGENTS:
 *   - Individual Sensemaking Capacity: Primary victim (powerless/trapped) - lacks the resources to filter or verify information effectively.
 *   - Information Aggregators: Beneficiary (moderate/constrained) - benefits from increased engagement, but constrained by the need to manage misinformation.
 *   - Epistemic Commons: Secondary victim (powerless/trapped) - the collective understanding of the world is degraded by the spread of misinformation.
 *   - Traditional Media Institutions: Degraded gatekeepers (institutional/constrained) - find their role diminished.
 *   - Attention Economy Platforms: Primary beneficiary (institutional/arbitrage) - benefits from increased engagement even with degraded content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_overload_collapse, 0.65).
domain_priors:suppression_score(epistemic_overload_collapse, 0.7).
domain_priors:theater_ratio(epistemic_overload_collapse, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_overload_collapse, extractiveness, 0.65).
narrative_ontology:constraint_metric(epistemic_overload_collapse, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(epistemic_overload_collapse, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_overload_collapse, snare).
narrative_ontology:human_readable(epistemic_overload_collapse, "The Signal-Drowning Vortex").
narrative_ontology:topic_domain(epistemic_overload_collapse, "cognitive/informational/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, information_aggregators).
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, attention_economy_platforms).
narrative_ontology:constraint_victim(epistemic_overload_collapse, individual_sensemaking_capacity).
narrative_ontology:constraint_victim(epistemic_overload_collapse, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual, bombarded with information, lacks the cognitive resources to effectively filter or verify claims. Trapped within the information flow, their sensemaking collapses. Maximum experienced extraction.
constraint_indexing:constraint_classification(epistemic_overload_collapse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% Platforms benefit from increased engagement driven by the constant influx of new information (Rope aspect). However, they are also constrained by the need to manage misinformation and maintain user trust. Experiences mixed coordination and extraction.
constraint_indexing:constraint_classification(epistemic_overload_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Organized groups attempting to provide structured information and verification mechanisms are constrained by the scale of the problem and the speed of information dissemination. They provide a coordination service, but also face extraction as their efforts are often undermined by bad actors.
constraint_indexing:constraint_classification(epistemic_overload_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Traditional media, once gatekeepers of information, find their role diminished in the face of decentralized online content. Their existing verification processes are often too slow to keep pace, and their brand reputation is eroded by the spread of misinformation. They still maintain a sense of authority, but their function is diminished.
constraint_indexing:constraint_classification(epistemic_overload_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% From a broad, long-term perspective, the information environment can be seen as a complex system with both beneficial and detrimental aspects. The observer can analyze the flows of information, identify vulnerabilities, and propose interventions, but is also part of the system they are observing.
constraint_indexing:constraint_classification(epistemic_overload_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_overload_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_overload_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_overload_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_overload_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_overload_collapse, TR),
    TR >= 0.70.

:- end_tests(epistemic_overload_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The information environment extracts cognitive resources from individuals. Suppression (0.70): High. Individuals lack the ability to effectively filter or verify information, resulting in them being suppressed by noise. Theater ratio (0.30): Moderate. There is some effort towards filtering, but the scale of the problem is vast.
 *
 * PERSPECTIVAL GAP:
 *   The individual experiences a snare, trapped by the overwhelming information. Information aggregators see a tangled rope as they benefit from the influx of information, but are constrained by the need to manage misinformation. Traditional media sees a piton as their role as gatekeepers is eroded.
 *
 * DIRECTIONALITY LOGIC:
 *   The individual is a trapped target, experiencing significant extraction. Information aggregators benefit from increased engagement, giving them lower directionality. The analytical observer can step back and assess the overall dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_limits,
    'What is the actual upper bound on individual cognitive capacity for processing complex information streams?',
    'Cognitive science research on attention span, working memory, and information processing speed; correlation between information load and decision-making quality.',
    'If capacity is lower than assumed: overload effects are more pervasive and damaging. If capacity is higher: resilience strategies can be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_limits, empirical, 'Understanding the limits of individual cognitive capacity.').

omega_variable(
    misinformation_detection_effectiveness,
    'How effective are current techniques for detecting and combating misinformation at scale?',
    'Analysis of fact-checking accuracy, the speed of misinformation spread vs. corrective information, and the impact of labeling and algorithmic interventions.',
    'If detection is highly effective: the severity of the overload effect is mitigated. If detection is weak: misinformation compounds the problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(misinformation_detection_effectiveness, empirical, 'Evaluating the effectiveness of misinformation detection techniques.').

omega_variable(
    platform_incentive_alignment,
    'Can platforms be effectively incentivized to prioritize information quality over engagement metrics?',
    'Policy experiments with algorithmic transparency requirements, liability rules, and alternative funding models for content moderation.',
    'If incentives can be aligned: platforms can play a positive role in mitigating overload. If not: their inherent business model exacerbates the problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_incentive_alignment, preference, 'Assessing the potential for aligning platform incentives with information quality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_overload_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t0, epistemic_overload_collapse, theater_ratio, 0, 0.1).
narrative_ontology:measurement(epis_tr_t5, epistemic_overload_collapse, theater_ratio, 5, 0.2).
narrative_ontology:measurement(epis_tr_t10, epistemic_overload_collapse, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t0, epistemic_overload_collapse, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(epis_be_t5, epistemic_overload_collapse, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(epis_be_t10, epistemic_overload_collapse, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_overload_collapse, information_standard).
narrative_ontology:affects_constraint(epistemic_overload_collapse, filter_bubble_dynamics).
narrative_ontology:affects_constraint(epistemic_overload_collapse, algorithmic_bias_amplification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
