% ============================================================================
% CONSTRAINT STORY: attention_market_cannibalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_market_cannibalization, []).

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
 *   constraint_id: attention_market_cannibalization
 *   human_readable: The Cognitive Exhaustion Loop
 *   domain: economic/psychological/technological
 *
 * SUMMARY:
 *   The cognitive exhaustion loop describes a scenario where digital
 *   platforms compete for user attention, leading to an over-extraction of
 *   cognitive resources. This competition can undermine individual well-being
 *   and social cohesion. Platforms optimize for 'engagement' metrics, often
 *   at the expense of user's ability to focus on essential life functions.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary target (powerless/trapped) - bear the cost of cognitive overload.
 *   - Digital Platforms: Primary beneficiary (institutional/arbitrage) - benefit from increased engagement and monetization.
 *   - Attention Brokers: Secondary beneficiary (powerful/mobile) - data analytics firms that aggregate user data.
 *   - Social Cohesion: Secondary target (powerless/trapped) - suffers from fractured attention and polarization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_market_cannibalization, 0.65).
domain_priors:suppression_score(attention_market_cannibalization, 0.7).
domain_priors:theater_ratio(attention_market_cannibalization, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_market_cannibalization, extractiveness, 0.65).
narrative_ontology:constraint_metric(attention_market_cannibalization, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(attention_market_cannibalization, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_market_cannibalization, tangled_rope).
narrative_ontology:human_readable(attention_market_cannibalization, "The Cognitive Exhaustion Loop").
narrative_ontology:topic_domain(attention_market_cannibalization, "economic/psychological/technological").

domain_priors:requires_active_enforcement(attention_market_cannibalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_market_cannibalization, digital_platforms).
narrative_ontology:constraint_beneficiary(attention_market_cannibalization, attention_brokers).
narrative_ontology:constraint_victim(attention_market_cannibalization, individual_users).
narrative_ontology:constraint_victim(attention_market_cannibalization, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual user, trapped in the ecosystem, experiences the constant demands for attention as a snare, extracting cognitive resources and reducing overall well-being. They lack the power to resist the constant onslaught of notifications and stimuli.
constraint_indexing:constraint_classification(attention_market_cannibalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Digital platforms, competing for market share, benefit from increased engagement, even if it leads to user exhaustion. They see the attention market as a rope, facilitating connection and information flow, optimizing for user retention and monetization, and can readily shift strategies to maintain their position.
constraint_indexing:constraint_classification(attention_market_cannibalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the attention economy appears as a tangled rope, offering benefits in information access and connection but also extracting cognitive resources and potentially undermining social cohesion through fragmentation and polarization. The analytical observer sees both the coordination and extraction functions.
constraint_indexing:constraint_classification(attention_market_cannibalization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Parents, worried about their children's well-being, see a tangled rope. They benefit from some of the platform's coordination functions in organizing family life, but are also victimized by platform strategies that compete for their child's attention, which they have only moderate power to curtail.
constraint_indexing:constraint_classification(attention_market_cannibalization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_market_cannibalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_market_cannibalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_market_cannibalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_market_cannibalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(attention_market_cannibalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Platforms extract a significant portion of user attention, impacting cognitive resources. Suppression (0.70): High. Users face significant barriers to escaping the loop due to persuasive design, network effects, and habit formation. Theater ratio (0.30): Low. Platforms primarily focus on functional engagement metrics. Active enforcement through algorithms and platform design.
 *
 * PERSPECTIVAL GAP:
 *   The exhausted user experiences a snare, while the platform views it as a rope. The analytical observer recognizes the mixed nature of the attention economy, exhibiting both coordination and extraction. The parent experiences the situation as a tangled rope because they benefit from coordination, yet witness their child be victimized.
 *
 * DIRECTIONALITY LOGIC:
 *   Digital platforms benefit from heightened user engagement even as individuals become trapped in attention loops that siphon more of their attention away from essential tasks. Directionality of the institutional actors shifts negative, directionality of the powerless is shifted positive.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Tangled Rope because the extraction function is coupled with an underlying coordination function -- platforms solve a problem for users and the extraction is a byproduct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_bandwidth_limit,
    'What is the true limit of human cognitive bandwidth, and how much of it can be sustainably allocated to digital engagement without negative consequences?',
    'Neuroscience research, longitudinal studies on digital usage and mental health, cognitive load analysis.',
    'Determines the severity of extraction. A lower limit implies a more severe snare, requiring stronger regulation and user empowerment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_bandwidth_limit, empirical, 'The limit of sustainable digital engagement.').

omega_variable(
    platform_responsibility_threshold,
    'At what point do digital platforms become ethically responsible for mitigating the negative consequences of attention extraction?',
    'Legal frameworks, industry self-regulation, public discourse and ethical guidelines.',
    'Determines the feasibility of coordination. A higher threshold implies less coordination and greater extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_responsibility_threshold, preference, 'The point of ethical responsibility for digital platforms.').

omega_variable(
    counter_narrative_strength,
    'How effective are counter-narratives and user-empowerment strategies in mitigating the effects of the attention economy?',
    'Evaluation of media literacy programs, analysis of user behavior in response to platform interventions, and analysis of alternative platform engagement strategies.',
    'Determines the potential for exit. Stronger counter-narratives empower users to escape the snare, shifting it towards a more balanced exchange.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_narrative_strength, empirical, 'The effectiveness of user empowerment strategies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_market_cannibalization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atte_tr_t0, attention_market_cannibalization, theater_ratio, 0, 0.1).
narrative_ontology:measurement(atte_tr_t5, attention_market_cannibalization, theater_ratio, 5, 0.2).
narrative_ontology:measurement(atte_tr_t10, attention_market_cannibalization, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(atte_be_t0, attention_market_cannibalization, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(atte_be_t5, attention_market_cannibalization, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(atte_be_t10, attention_market_cannibalization, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_market_cannibalization, information_standard).
narrative_ontology:affects_constraint(attention_market_cannibalization, filter_bubble_polarization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
