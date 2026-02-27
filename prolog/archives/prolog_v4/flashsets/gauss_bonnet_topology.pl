% ============================================================================
% CONSTRAINT STORY: gauss_bonnet_topology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gauss_bonnet_topology, []).

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
 *   constraint_id: gauss_bonnet_topology
 *   human_readable: Gauss-Bonnet Theorem (Curvature-Topology Link)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Gauss-Bonnet theorem is a fundamental result in differential geometry
 *   that links the local Gaussian curvature of a surface to its global
 *   topological Euler characteristic. It is a constraint on the possible
 *   geometries of surfaces.
 *
 * KEY AGENTS:
 *   - Naive Geometer: Powerless/Trapped - cannot violate the theorem.
 *   - Mathematical Community: Institutional/Analytical - accepts and uses the theorem.
 *   - Analytical Observer: Analytical/Analytical - recognizes the theorem as a fundamental truth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gauss_bonnet_topology, 0.05).
domain_priors:suppression_score(gauss_bonnet_topology, 0.01).
domain_priors:theater_ratio(gauss_bonnet_topology, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gauss_bonnet_topology, extractiveness, 0.05).
narrative_ontology:constraint_metric(gauss_bonnet_topology, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(gauss_bonnet_topology, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gauss_bonnet_topology, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(gauss_bonnet_topology, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gauss_bonnet_topology, mountain).
narrative_ontology:human_readable(gauss_bonnet_topology, "Gauss-Bonnet Theorem (Curvature-Topology Link)").
narrative_ontology:topic_domain(gauss_bonnet_topology, "mathematical").

domain_priors:emerges_naturally(gauss_bonnet_topology).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Even a novice geometer cannot violate this relationship; it's a fundamental constraint.
constraint_indexing:constraint_classification(gauss_bonnet_topology, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The mathematical community accepts this theorem as a cornerstone of differential geometry and topology.
constraint_indexing:constraint_classification(gauss_bonnet_topology, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Analytical observer sees the Gauss-Bonnet theorem as a fundamental link between geometry and topology, a mathematical truth with no exceptions.
constraint_indexing:constraint_classification(gauss_bonnet_topology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gauss_bonnet_topology_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(gauss_bonnet_topology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gauss_bonnet_topology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gauss_bonnet_topology, ExtMetricName, E),
    domain_priors:suppression_score(gauss_bonnet_topology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gauss_bonnet_topology),
    narrative_ontology:constraint_metric(gauss_bonnet_topology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gauss_bonnet_topology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gauss_bonnet_topology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are minimal as the theorem is a fundamental mathematical truth. Theater ratio is also minimal as there is no performative aspect to the theorem.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap as all agents recognize the Gauss-Bonnet theorem as a fundamental mathematical truth.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims as it's a fundamental law.
 *
 * MANDATROPHY ANALYSIS:
 *   N/A as extractiveness is near zero.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gauss_bonnet_topology, 0, 100).

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
