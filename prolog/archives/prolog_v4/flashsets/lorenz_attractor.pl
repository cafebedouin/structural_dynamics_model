% ============================================================================
% CONSTRAINT STORY: lorenz_attractor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lorenz_attractor, []).

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
 *   constraint_id: lorenz_attractor
 *   human_readable: Sensitivity to Initial Conditions (Lorenz Attractor)
 *   domain: technological
 *
 * SUMMARY:
 *   The Lorenz Attractor exemplifies sensitivity to initial conditions in
 *   deterministic nonlinear systems. Even minute differences in the starting
 *   state can lead to drastically different long-term outcomes, making
 *   precise prediction fundamentally impossible beyond a certain time
 *   horizon. This inherent unpredictability acts as a constraint on
 *   technological systems relying on such dynamics.
 *
 * KEY AGENTS:
 *   - Powerless observer (powerless/trapped): Unable to alter the sensitivity to initial conditions.
 *   - Institutional analyst (institutional/analytical): Recognizes the inherent sensitivity as a fundamental property.
 *   - Analytical observer (analytical/analytical): Sees the sensitivity as a natural law of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lorenz_attractor, 0.15).
domain_priors:suppression_score(lorenz_attractor, 0.01).
domain_priors:theater_ratio(lorenz_attractor, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lorenz_attractor, extractiveness, 0.15).
narrative_ontology:constraint_metric(lorenz_attractor, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(lorenz_attractor, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lorenz_attractor, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(lorenz_attractor, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lorenz_attractor, mountain).
narrative_ontology:human_readable(lorenz_attractor, "Sensitivity to Initial Conditions (Lorenz Attractor)").
narrative_ontology:topic_domain(lorenz_attractor, "technological").

domain_priors:emerges_naturally(lorenz_attractor).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Even a powerless observer is trapped by the deterministic nature of the system, unable to alter the sensitivity to initial conditions.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% An institution analyzing the system recognizes the inherent sensitivity as a fundamental property.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From an analytical perspective, the sensitivity to initial conditions is a natural law of the system.
constraint_indexing:constraint_classification(lorenz_attractor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lorenz_attractor_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lorenz_attractor, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lorenz_attractor, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lorenz_attractor, ExtMetricName, E),
    domain_priors:suppression_score(lorenz_attractor, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lorenz_attractor),
    narrative_ontology:constraint_metric(lorenz_attractor, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lorenz_attractor, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lorenz_attractor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the sensitivity to initial conditions doesn't extract resources or opportunities in a direct way. Suppression is low because the system's behavior is governed by its inherent dynamics, not by external coercive forces. The theater ratio is low since the system behaves deterministically according to its equations.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as Mountain because sensitivity to initial conditions is a fundamental property of the Lorenz system, regardless of the observer's power, exit options, or scope.
 *
 * DIRECTIONALITY LOGIC:
 *   The inherent sensitivity affects all observers equally and is a property of the attractor. All relationships are symmetric; there are no beneficiaries or victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification reflects the inherent nature of the Lorenz system. Mislabeling it as another type (e.g., Snare) would ignore its deterministic and inevitable behavior. Sensitivity to initial conditions can be used to mislead, however, if a system is presented as predictable when it is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lorenz_attractor, 0, 100).

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
