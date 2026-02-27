% ============================================================================
% CONSTRAINT STORY: value_alignment_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_value_alignment_drift, []).

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
 *   constraint_id: value_alignment_drift
 *   human_readable: Value Alignment Drift (Purpose Creep)
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Purpose Creep is a systemic failure where an algorithm or
 *   organization optimizes for a proxy metric (e.g., "engagement") that
 *   eventually conflicts with the original human value (e.g., "well-being").
 *   This drift can be subtle and difficult to detect, as the initial
 *   optimization may provide genuine value before the misalignment becomes
 *   apparent.
 *
 * KEY AGENTS:
 *   - Platform Owners: Institutional actors (institutional/arbitrage) who initially benefit from increased engagement.
 *   - Advertisers: Institutional actors (institutional/arbitrage) who pay for increased exposure.
 *   - Algorithmic Targets: Powerless individuals (powerless/trapped) who are increasingly manipulated and exploited.
 *   - Social Cohesion: A broad collective (powerless/trapped) that suffers from the negative externalities of the misalignment.
 *   - Regulators and Watchdog Groups: (moderate/constrained) who attempt to mitigate the negative consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(value_alignment_drift, 0.6).
domain_priors:suppression_score(value_alignment_drift, 0.7).
domain_priors:theater_ratio(value_alignment_drift, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(value_alignment_drift, extractiveness, 0.6).
narrative_ontology:constraint_metric(value_alignment_drift, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(value_alignment_drift, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(value_alignment_drift, tangled_rope).
narrative_ontology:human_readable(value_alignment_drift, "Value Alignment Drift (Purpose Creep)").
narrative_ontology:topic_domain(value_alignment_drift, "technological/social").

domain_priors:requires_active_enforcement(value_alignment_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(value_alignment_drift, platform_owners).
narrative_ontology:constraint_beneficiary(value_alignment_drift, advertisers).
narrative_ontology:constraint_victim(value_alignment_drift, algorithmic_targets).
narrative_ontology:constraint_victim(value_alignment_drift, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Users are often trapped within the algorithmic system due to network effects or lack of viable alternatives. They experience the purpose creep as a snare, as the system increasingly optimizes for metrics that are misaligned with their well-being and autonomy. They bear the costs of the drift and have little power to change the system.
constraint_indexing:constraint_classification(value_alignment_drift, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Regulators and watchdog groups attempt to monitor and mitigate the purpose creep. They are constrained by limited resources, political influence, and the complexity of the algorithmic systems. They experience a tangled rope as they attempt to balance the coordination function of oversight with the extractive nature of the misalignment.
constraint_indexing:constraint_classification(value_alignment_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Platform owners benefit from the initial optimization for engagement, which drives revenue and market share. They have arbitrage exit options because they can shift resources to new metrics, although this may have long-term consequences. From their perspective, this initially appears as a rope as the platform enables connections and sharing.
constraint_indexing:constraint_classification(value_alignment_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Future generations are constrained by the consequences of current algorithmic misalignments. They benefit from technological advancements but are constrained by the societal and environmental consequences. They experience a tangled rope with limited agency to correct the course.
constraint_indexing:constraint_classification(value_alignment_drift, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees the purpose creep as a tangled rope. The algorithmic systems initially coordinate user activity and provide value, but as they optimize for engagement, they extract value from users and society as a whole. The extraction happens over time and with a seemingly positive effect on the platform owners.
constraint_indexing:constraint_classification(value_alignment_drift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_alignment_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(value_alignment_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_alignment_drift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(value_alignment_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(value_alignment_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The system extracts value from users and society by manipulating behavior and creating negative externalities. Suppression (0.7): Users are often trapped within the system and have limited alternatives. The theater ratio is relatively low (0.4) because the initial coordination function is genuine.
 *
 * PERSPECTIVAL GAP:
 *   Platform owners see a rope (coordination) because they are enabling connections and sharing. Users experience a snare (extraction) because they are being manipulated and exploited. The analytical observer sees a tangled rope because the system initially coordinates but eventually extracts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position of each agent. Platform owners benefit from increased engagement, while users and society bear the costs of the misalignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The Purpose Creep is a genuine instance of a tangled rope. It is not merely a disguised snare because the system initially provides a coordination function before drifting towards extraction. The coordination function helps maintain the suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_capturability,
    'How susceptible is a given proxy metric to unintended manipulation or exploitation?',
    'Empirical testing of the proxy metric in controlled environments, adversarial simulations, and long-term observational studies.',
    'If the metric is highly capturable, the tangled rope strengthens toward a snare. If it is robust, the system may classify as a rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_capturability, empirical, 'Vulnerability of the proxy metric to exploitation.').

omega_variable(
    long_term_value_alignment,
    'How durable is the initial alignment between the proxy metric and the underlying human value?',
    'Longitudinal studies tracking the correlation between the proxy metric and measures of actual well-being and societal benefit.',
    'If the alignment degrades rapidly, the system quickly transitions toward a snare or piton. If alignment persists, the system may function as a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_value_alignment, empirical, 'Durability of value alignment.').

omega_variable(
    ethical_governance_efficacy,
    'How effective are ethical governance mechanisms (e.g., oversight boards, internal review processes) in detecting and mitigating purpose creep?',
    'Audits of ethical governance mechanisms, assessments of their ability to detect and respond to misalignment signals, and impact evaluations.',
    'If governance is ineffective, the system trends toward a snare. If governance is strong, the system could maintain a rope or tangled rope configuration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_governance_efficacy, empirical, 'Efficacy of ethical governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(value_alignment_drift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, value_alignment_drift, theater_ratio, 0, 0.1).
narrative_ontology:measurement(valu_tr_t5, value_alignment_drift, theater_ratio, 5, 0.3).
narrative_ontology:measurement(valu_tr_t10, value_alignment_drift, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, value_alignment_drift, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(valu_be_t5, value_alignment_drift, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(valu_be_t10, value_alignment_drift, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(value_alignment_drift, information_standard).
narrative_ontology:affects_constraint(value_alignment_drift, algorithmic_bias).
narrative_ontology:affects_constraint(value_alignment_drift, information_overload).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
