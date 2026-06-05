% ============================================================================
% CONSTRAINT STORY: logistic_map_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_logistic_map_dynamics, []).

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
 *   constraint_id: logistic_map_dynamics
 *   human_readable: The Logistic Map (Bifurcation and Chaos)
 *   domain: mathematical/biological
 *
 * SUMMARY:
 *   The Logistic Map (x_{n+1} = r * x_n * (1 - x_n)) is a simple polynomial
 *   mapping that demonstrates how complex, chaotic behavior can arise from
 *   simple non-linear dynamical equations. Its behavior is intrinsic to the
 *   mathematics and is seen as a fundamental property by any observer, hence
 *   classifying as a mountain.
 *
 * KEY AGENTS:
 *   - Mathematical Observer: Analytical agent, sees the mathematical necessity of the system.
 *   - Unaware Initial State: Experiences the deterministic evolution.
 *   - Population Biologist: Utilizes the map as a simplified model.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(logistic_map_dynamics, 0.15).
domain_priors:suppression_score(logistic_map_dynamics, 0.01).
domain_priors:theater_ratio(logistic_map_dynamics, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(logistic_map_dynamics, extractiveness, 0.15).
narrative_ontology:constraint_metric(logistic_map_dynamics, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(logistic_map_dynamics, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(logistic_map_dynamics, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(logistic_map_dynamics, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(logistic_map_dynamics, mountain).
narrative_ontology:human_readable(logistic_map_dynamics, "The Logistic Map (Bifurcation and Chaos)").
narrative_ontology:topic_domain(logistic_map_dynamics, "mathematical/biological").

domain_priors:emerges_naturally(logistic_map_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The analytical observer, understanding the mathematics, recognizes that the logistic map's behavior is mathematically determined and independent of any observer's perspective or intervention. It's a natural law of non-linear dynamics.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of an initial condition, unaware of the underlying equation, the system's future state is entirely determined, whether it leads to a stable fixed point, oscillation, or chaos. There is no escaping the dynamics dictated by the map.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% A population biologist using the logistic map as a model recognizes its inherent mathematical properties. While the model simplifies reality, the underlying dynamics it reveals are robust and emerge naturally from the equation itself, regardless of specific parameter choices.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(logistic_map_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(logistic_map_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(logistic_map_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(logistic_map_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(logistic_map_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(logistic_map_dynamics),
    narrative_ontology:constraint_metric(logistic_map_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(logistic_map_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(logistic_map_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness, suppression and theater ratio are very low because the Logistic Map represents an inherent mathematical relationship that exists regardless of human interference or observation. It's a fundamental concept, not a system that can be exploited or manipulated.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap as any observer, regardless of their power, time horizon or exit options, will perceive the same inherent mathematical properties of the Logistic Map. It is a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim is applicable. The logistic map represents a natural mathematical relationship, not a system where extraction or benefit is relevant.
 *
 * MANDATROPHY ANALYSIS:
 *   The logistic map has no significant potential for misclassification as other constraint types. It is a pure mathematical relationship, not a coordination mechanism, extraction system or temporary support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(logistic_map_dynamics, 0, 100).

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
