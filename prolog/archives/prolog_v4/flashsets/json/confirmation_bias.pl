% ============================================================================
% CONSTRAINT STORY: confirmation_bias
% ============================================================================
% Version: 1.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_confirmation_bias, []).

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
 *   constraint_id: confirmation_bias
 *   human_readable: Confirmation Bias (Socially Amplified)
 *   domain: social/cognitive/technological
 *
 * SUMMARY:
 *   Confirmation bias is the tendency to favor information confirming
 *   existing beliefs, amplified through social media algorithms that create
 *   echo chambers. This constraint extracts epistemic autonomy from
 *   individuals trapped in these chambers, while providing benefits to
 *   platforms and misinformation spreaders. The increase in base
 *   extractiveness reflects the growing sophistication of algorithmic
 *   manipulation. Fact-checking organizations, as legacy institutions, are
 *   increasingly theatrical as they struggle to keep up with the scale and
 *   speed of misinformation.
 *
 * KEY AGENTS:
 *   - Echo Chamber Participants: Primary victim (powerless/trapped) — epistemic autonomy extracted.
 *   - Critical Thinking Individuals: Secondary victim (moderate/constrained) — cognitive load and social pressure.
 *   - Platform Promoting Engagement: Primary beneficiary (institutional/arbitrage) — Engagement and data extraction optimized by biased content.
 *   - Misinformation Spreaders: Powerful agents (powerful/mobile) — spreading intentionally false information.
 *   - Fact-Checking Organizations: Inertial agent (institutional/constrained) — increasingly theatrical.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(confirmation_bias, 0.6).
domain_priors:suppression_score(confirmation_bias, 0.7).
domain_priors:theater_ratio(confirmation_bias, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(confirmation_bias, extractiveness, 0.6).
narrative_ontology:constraint_metric(confirmation_bias, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(confirmation_bias, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(confirmation_bias, tangled_rope).
narrative_ontology:human_readable(confirmation_bias, "Confirmation Bias (Socially Amplified)").
narrative_ontology:topic_domain(confirmation_bias, "social/cognitive/technological").

domain_priors:requires_active_enforcement(confirmation_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(confirmation_bias, echo_chamber_participants).
narrative_ontology:constraint_beneficiary(confirmation_bias, misinformation_spreaders).
narrative_ontology:constraint_victim(confirmation_bias, epistemic_commons).
narrative_ontology:constraint_victim(confirmation_bias, critical_thinking_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual trapped within an echo chamber, bombarded with reinforcing information and suppressed dissent. No viable exit option; high extraction of epistemic autonomy.
constraint_indexing:constraint_classification(confirmation_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Individual attempting to maintain critical thinking skills while exposed to biased information. Constrained by cognitive load and social pressure. Receives some benefits from informational diversity, but also bears costs of navigating misinformation.
constraint_indexing:constraint_classification(confirmation_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Social media platform algorithm designed to maximize user engagement through personalized content. Algorithm benefits from increased activity (arbitrage) and reinforces user biases, creating a coordination mechanism for content distribution within user-defined groups.
constraint_indexing:constraint_classification(confirmation_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Agents deliberately exploiting confirmation bias to spread misinformation for political or economic gain. Mobile across platforms and ideologies. Extraction through manipulation of public opinion, but some coordination among themselves.
constraint_indexing:constraint_classification(confirmation_bias, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Legacy institutions (news media, academic journals) attempting to combat misinformation through fact-checking. Constrained by limited resources and the speed of information dissemination. High theater, as fact-checks often reach a smaller audience than the original misinformation.
constraint_indexing:constraint_classification(confirmation_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical observer assessing the global impact of confirmation bias on epistemic reliability. Recognizes mixed coordination/extraction due to engagement optimization combined with epistemic degradation.
constraint_indexing:constraint_classification(confirmation_bias, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(confirmation_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(confirmation_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(confirmation_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(confirmation_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(confirmation_bias, TR),
    TR >= 0.70.

:- end_tests(confirmation_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.60 (High). Significant extraction of epistemic autonomy as individuals become trapped in echo chambers. Suppression: 0.70 (High). Algorithmic filtering and social pressure suppress dissenting opinions. Theater Ratio: 0.75 (High). Fact-checking organizations are increasingly theatrical, as they struggle to keep up with the scale and speed of misinformation.
 *
 * PERSPECTIVAL GAP:
 *   The trapped individual experiences a snare, the critical thinking individual experiences the tangled rope, and the platform experiences pure coordination (rope). Fact-checkers are pitons, and the analytical observer sees the tangled rope of a society with compromised sensemaking.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the beneficiary/victim status. Platform algorithms are beneficiaries, so directionality value approaches 0, hence Rope. Trapped users have directionality approaching 1, hence Snare. Analytical observer sees overall mix of exploitation and limited coordination (Tangled Rope).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_threshold,
    'What level of bias-reinforcement constitutes a critical threat to individual autonomy and social cohesion?',
    'Longitudinal studies of individuals exposed to varying levels of biased information; correlation with measures of political polarization and social trust.',
    'If threshold is low: aggressive interventions needed to limit biased content. If threshold is high: individual resilience may be sufficient to mitigate negative effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_threshold, empirical, 'Critical threshold of bias-reinforcement exposure').

omega_variable(
    intervention_effectiveness,
    'Which interventions (algorithmic adjustments, content moderation, media literacy campaigns) are most effective in combating the spread of misinformation and reducing the impact of confirmation bias?',
    'A/B testing of different interventions on social media platforms; randomized controlled trials measuring the impact of media literacy training on cognitive biases.',
    'Determines optimal strategies for mitigating negative consequences of confirmation bias and misinformation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_effectiveness, empirical, 'Efficacy of countermeasures against confirmation bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(confirmation_bias, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conf_tr_t0, confirmation_bias, theater_ratio, 0, 0.1).
narrative_ontology:measurement(conf_tr_t5, confirmation_bias, theater_ratio, 5, 0.4).
narrative_ontology:measurement(conf_tr_t10, confirmation_bias, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(conf_be_t0, confirmation_bias, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(conf_be_t5, confirmation_bias, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(conf_be_t10, confirmation_bias, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(confirmation_bias, information_standard).
narrative_ontology:affects_constraint(confirmation_bias, filter_bubble).
narrative_ontology:affects_constraint(confirmation_bias, algorithmic_bias).

% DUAL FORMULATION NOTE:
% Confirmation bias is upstream of filter bubbles and algorithmic bias. Filter bubbles are an emergent property caused by confirmation bias.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
