% ============================================================================
% CONSTRAINT STORY: damped_harmonics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_damped_harmonics, []).

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
 *   constraint_id: damped_harmonics
 *   human_readable: Damped Harmonic Motion
 *   domain: technological
 *
 * SUMMARY:
 *   Damped harmonic motion describes an oscillatory system where the
 *   amplitude decreases over time due to dissipative forces (friction, air
 *   resistance, or internal viscosity). This behavior is a fundamental
 *   principle in physics and engineering, applicable to various systems
 *   ranging from mechanical oscillators to electrical circuits. The damping
 *   effect represents an inevitable decay in energy, posing a constraint on
 *   system performance and longevity.
 *
 * KEY AGENTS:
 *   - Inertial Mass: Target (powerless/trapped) - Undergoes inevitable energy dissipation.
 *   - Engineering Design: Beneficiary (institutional/analytical) - Must account for this effect when designing systems.
 *   - Analytical Observer: Observer (analytical/analytical) - Views this as a fundamental law of physics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(damped_harmonics, 0.15).
domain_priors:suppression_score(damped_harmonics, 0.02).
domain_priors:theater_ratio(damped_harmonics, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(damped_harmonics, extractiveness, 0.15).
narrative_ontology:constraint_metric(damped_harmonics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(damped_harmonics, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(damped_harmonics, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(damped_harmonics, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(damped_harmonics, mountain).
narrative_ontology:human_readable(damped_harmonics, "Damped Harmonic Motion").
narrative_ontology:topic_domain(damped_harmonics, "technological").

domain_priors:emerges_naturally(damped_harmonics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an inertial mass undergoing damped harmonic motion, this is simply an immutable consequence of physics.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% From the perspective of engineering design, damped harmonic motion represents an intrinsic physical constraint and an inevitable decay.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% An analytical observer sees damped harmonic motion as a fundamental principle governing the behavior of oscillatory systems, regardless of scale or application.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(damped_harmonics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(damped_harmonics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(damped_harmonics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(damped_harmonics, ExtMetricName, E),
    domain_priors:suppression_score(damped_harmonics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(damped_harmonics),
    narrative_ontology:constraint_metric(damped_harmonics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(damped_harmonics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(damped_harmonics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. Dissipation is inevitable, but it is generally low. Suppression (0.02): Very low, as there are limited options to alter the behavior. Theater Ratio (0.01): Virtually none, as the system inevitably loses energy to damping. All perspectives classify the phenomenon as a mountain.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap, as all agents experience damped harmonic motion as an inevitable consequence of physics. Whether one is the object being damped (powerless), designing around it (institutional) or studying it (analytical), the classification remains a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position — their power level, exit options, and relationship to the dissipation. Because the system is a natural law, there are no beneficiaries or victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(damped_harmonics, 0, 100).

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
