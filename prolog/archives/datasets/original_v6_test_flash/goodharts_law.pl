% ============================================================================
% CONSTRAINT STORY: goodharts_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodharts_law, []).

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
 *   constraint_id: goodharts_law
 *   human_readable: Goodhart's Law
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Goodhart's Law describes the process where a metric, initially intended
 *   to measure progress toward a specific goal, becomes distorted and gamed
 *   as agents optimize directly for the metric itself rather than the
 *   underlying goal. This often leads to a decline in the actual desired
 *   outcome, as resources are diverted to artificially inflate the metric.
 *
 * KEY AGENTS:
 *   - System Goals: Abstract entity representing the original intention (powerless/trapped)
 *   - Metric Optimizers: Agents who actively game the metric (institutional/arbitrage)
 *   - Naive Actors: Agents who are not actively gaming the metric (moderate/constrained)
 *   - Analytical Observer: Observer who sees the overall pattern (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodharts_law, 0.55).
domain_priors:suppression_score(goodharts_law, 0.65).
domain_priors:theater_ratio(goodharts_law, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodharts_law, extractiveness, 0.55).
narrative_ontology:constraint_metric(goodharts_law, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(goodharts_law, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodharts_law, tangled_rope).
narrative_ontology:human_readable(goodharts_law, "Goodhart's Law").
narrative_ontology:topic_domain(goodharts_law, "economic/social/technological").

domain_priors:requires_active_enforcement(goodharts_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodharts_law, metric_optimizers).
narrative_ontology:constraint_victim(goodharts_law, system_goals).
narrative_ontology:constraint_victim(goodharts_law, naive_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The original system goals become trapped. They are powerless to prevent the metric substitution and suffer from the resulting distortions. No exit option.
constraint_indexing:constraint_classification(goodharts_law, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Naive actors who are not actively gaming the system are constrained. They experience extraction because their efforts are not rewarded in the same way as metric optimizers, yet they also benefit somewhat from the coordination that the initial, pre-Goodhart metric provided.
constraint_indexing:constraint_classification(goodharts_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The agents who are actively optimizing for the metric benefit in the short term. They experience this as a coordination mechanism, allowing them to achieve their goals more efficiently within the system. They can arbitrage the metric for personal gain.
constraint_indexing:constraint_classification(goodharts_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% From a distance, Goodhart's Law appears as a Piton, a degraded system. The initial intention and coordination function have been lost, and only the performative aspect of the metric remains.  The original goal is no longer pursued, but the metric is still tracked and reported as if it were.
constraint_indexing:constraint_classification(goodharts_law, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodharts_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodharts_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodharts_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodharts_law, TR),
    TR >= 0.70.

:- end_tests(goodharts_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55. The system is significantly distorted by the focus on the metric, extracting value from the original goals.  Suppression: 0.65. Alternatives to achieving the original goal are suppressed because they are not measured or rewarded. Theater Ratio: 0.80.  The performative aspect of the metric is high, as agents prioritize appearing to achieve the goal over actually achieving it.
 *
 * PERSPECTIVAL GAP:
 *   The metric optimizers see the system as a rope, because it enables them to achieve their narrow goals efficiently. Naive actors see it as a tangled rope, because they benefit somewhat from the initial intention, but are also extracted from by the metric distortion. The original system goals are trapped, and thus see the system as a snare. The analytical observer sees a piton, a system where only the appearance of progress remains, while the underlying intention has been lost.
 *
 * DIRECTIONALITY LOGIC:
 *   Metric optimizers benefit from the system as it is gamed. Naive actors and system goals suffer because their outcomes are not aligned with the metric. The analytical observer sees the overall pattern of decay.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_complexity,
    'How complex must a metric be to resist Goodharting?',
    'Empirical study of different metric designs and their resistance to gaming.',
    'If low complexity is sufficient, then simple metrics can be used safely. If high complexity is required, then the cost of measurement increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_complexity, empirical, 'Determines the level of effort required to create robust metrics.').

omega_variable(
    incentive_alignment,
    'To what extent can incentives be aligned with the intended outcome?',
    'Analysis of incentive structures and their impact on behavior.',
    'High alignment minimizes Goodhart''s Law. Low alignment exacerbates it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_alignment, conceptual, 'Determines the strength of Goodhart''s Law in a given system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodharts_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(good_tr_t0, goodharts_law, theater_ratio, 0, 0.2).
narrative_ontology:measurement(good_tr_t5, goodharts_law, theater_ratio, 5, 0.5).
narrative_ontology:measurement(good_tr_t10, goodharts_law, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(good_be_t0, goodharts_law, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(good_be_t5, goodharts_law, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(good_be_t10, goodharts_law, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodharts_law, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
