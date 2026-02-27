% ============================================================================
% CONSTRAINT STORY: strange_attractor_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strange_attractor_dynamics, []).

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
 *   constraint_id: strange_attractor_dynamics
 *   human_readable: Strange Attractor Dynamics (Sensitive Dependence)
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   This constraint models the dynamics *on* a strange attractor,
 *   specifically the sensitive dependence on initial conditions that
 *   characterizes chaos. Small changes in initial conditions lead to
 *   exponentially diverging trajectories within the attractor's bounded
 *   region. This is a fundamental property of the mathematical system and is
 *   therefore classified as a mountain from various perspectives.
 *
 * KEY AGENTS:
 *   - Individual Trajectory: Powerless/Trapped - Subject to sensitive dependence.
 *   - Analytical Observer: Analytical/Analytical - Recognizes sensitive dependence as a fundamental property.
 *   - Scientific Community: Institutional/Analytical - Models and understands sensitive dependence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strange_attractor_dynamics, 0.15).
domain_priors:suppression_score(strange_attractor_dynamics, 0.01).
domain_priors:theater_ratio(strange_attractor_dynamics, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strange_attractor_dynamics, extractiveness, 0.15).
narrative_ontology:constraint_metric(strange_attractor_dynamics, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(strange_attractor_dynamics, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(strange_attractor_dynamics, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(strange_attractor_dynamics, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strange_attractor_dynamics, mountain).
narrative_ontology:human_readable(strange_attractor_dynamics, "Strange Attractor Dynamics (Sensitive Dependence)").
narrative_ontology:topic_domain(strange_attractor_dynamics, "mathematical/physical").

domain_priors:emerges_naturally(strange_attractor_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% An individual trajectory within the attractor cannot escape sensitive dependence on initial conditions.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% From an analytical perspective, sensitive dependence is an inherent property of the dynamics.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The scientific community recognizes and models the inherent sensitive dependence.
constraint_indexing:constraint_classification(strange_attractor_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strange_attractor_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(strange_attractor_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strange_attractor_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(strange_attractor_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(strange_attractor_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(strange_attractor_dynamics),
    narrative_ontology:constraint_metric(strange_attractor_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(strange_attractor_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(strange_attractor_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low as the constraint represents an inherent property rather than a coercive force. Suppression is low because there are no alternatives to the sensitive dependence within the system. The theater ratio is zero as there is no performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap, as all agents recognize and experience sensitive dependence as a fundamental property of the system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not a major factor here as this is a natural law. Beneficiaries and victims aren't directly relevant.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is clearly a mountain. There is no possibility of mislabeling this as a coordination or extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strange_attractor_dynamics, 0, 100).

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
