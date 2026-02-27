% ============================================================================
% CONSTRAINT STORY: martian_signal_latency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_martian_signal_latency, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: martian_signal_latency
 *   human_readable: Martian Signal Latency (One-Way Light Time)
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   Martian signal latency is the absolute delay in communication between
 *   Earth and Mars caused by the finite speed of light. This delay varies
 *   depending on the relative positions of the two planets, ranging from
 *   approximately 3 to 22 minutes for a one-way trip. It's a fundamental
 *   physical limitation that affects all aspects of communication, from
 *   controlling rovers to sending scientific data.
 *
 * KEY AGENTS:
 *   - Remote Operator: Control of rovers on Mars (powerless/trapped)
 *   - Space Agency: Planning Mars missions (institutional/analytical)
 *   - Analytical Observer: Understands the fundamental physics (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(martian_signal_latency, 0.01).
domain_priors:suppression_score(martian_signal_latency, 0.0).
domain_priors:theater_ratio(martian_signal_latency, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(martian_signal_latency, extractiveness, 0.01).
narrative_ontology:constraint_metric(martian_signal_latency, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(martian_signal_latency, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(martian_signal_latency, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(martian_signal_latency, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(martian_signal_latency, mountain).
narrative_ontology:human_readable(martian_signal_latency, "Martian Signal Latency (One-Way Light Time)").
narrative_ontology:topic_domain(martian_signal_latency, "technological/scientific").

domain_priors:emerges_naturally(martian_signal_latency).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a remote operator on Earth controlling a rover on Mars, the signal latency is a fixed constraint imposed by the speed of light and the distance between the planets. They are 'trapped' by this latency and cannot circumvent it in real-time. Any immediate action they take will experience a delay.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% From the perspective of a space agency planning long-term Mars missions, the signal latency is a fundamental constraint. They can develop strategies to mitigate its impact, such as autonomous systems or pre-programmed instructions, but they cannot eliminate it. They analyze this constraint as a fixed parameter in their mission design.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From an analytical observer's perspective, the signal latency due to the speed of light is an immutable physical constraint that governs interplanetary communication. It's a universal constant that cannot be bypassed or altered, regardless of technological advancements.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(martian_signal_latency_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(martian_signal_latency, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(martian_signal_latency, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(martian_signal_latency, ExtMetricName, E),
    domain_priors:suppression_score(martian_signal_latency, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(martian_signal_latency),
    narrative_ontology:constraint_metric(martian_signal_latency, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(martian_signal_latency, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(martian_signal_latency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.01): Extremely low, as the latency doesn't represent an active extraction but an unavoidable delay. Suppression (0.0): There is no suppression involved. The delay is a physical reality, not an imposed barrier. Theater ratio (0.0): There's no performative aspect to signal latency; it's a direct consequence of physics.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this as a mountain, though they experience it in different ways. The remote operator experiences it as a frustrating delay. The space agency has to design missions around it. The analytical observer understands it as a fundamental limit. However, the immutable nature of the constraint makes it invariant from any perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Since all perspectives classify this as a mountain, beneficiary and victim declarations are not applicable because the constraint emerges naturally. The directionality is inherently neutral; there is no extraction or active suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy because it is a true mountain. It cannot be misclassified as a snare or tangled rope because there is no active extraction or suppression; the latency is a consequence of the speed of light and distance, not an imposed barrier. Similarly, it cannot be a piton or scaffold because those involve intentional coordination schemes that have either degraded or are temporary, respectively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(martian_signal_latency, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
