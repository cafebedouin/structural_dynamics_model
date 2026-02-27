% ============================================================================
% CONSTRAINT STORY: fgh_hierarchy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fgh_hierarchy_2026, []).

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
 *   constraint_id: fgh_hierarchy_2026
 *   human_readable: The Fast-Growing Hierarchy
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   The Fast-Growing Hierarchy (FGH) is a family of functions indexed by
 *   transfinite ordinals that classifies the growth rates of computable
 *   functions. It provides a fundamental framework for understanding the
 *   limits of computation and the relative complexity of different
 *   algorithms. From all perspectives, it represents a fundamental limit
 *   imposed by mathematics, classifying as a Mountain.
 *
 * KEY AGENTS:
 *   - Mathematical Logician: Analytical observer who uses the FGH to study the limits of computability (analytical/analytical)
 *   - Naive Computer Scientist: Someone who encounters the FGH as a limit on algorithm efficiency (powerless/analytical)
 *   - Theoretical Computer Science Community: The FGH is a shared foundation (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fgh_hierarchy_2026, 0.15).
domain_priors:suppression_score(fgh_hierarchy_2026, 0.05).
domain_priors:theater_ratio(fgh_hierarchy_2026, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fgh_hierarchy_2026, extractiveness, 0.15).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fgh_hierarchy_2026, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fgh_hierarchy_2026, mountain).
narrative_ontology:human_readable(fgh_hierarchy_2026, "The Fast-Growing Hierarchy").
narrative_ontology:topic_domain(fgh_hierarchy_2026, "mathematical/computational").

domain_priors:emerges_naturally(fgh_hierarchy_2026).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The FGH provides a fundamental classification of the growth rates of computable functions, representing an inherent mathematical structure.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even without deep knowledge of computability theory, the FGH represents a limit on the efficiency of algorithms.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The FGH represents a stable and foundational concept in theoretical computer science.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fgh_hierarchy_2026_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, ExtMetricName, E),
    domain_priors:suppression_score(fgh_hierarchy_2026, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fgh_hierarchy_2026),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fgh_hierarchy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Fast-Growing Hierarchy is a mathematical construct describing a fundamental limit on growth rates. It emerges naturally from recursion theory. Extractiveness is low because it is a description of limitations, not an active constraint. Suppression is low as the hierarchy is descriptive, not prescriptive. Theater ratio is low because it accurately describes computational limits.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. Regardless of power, exit, time, or scope, the FGH is a Mountain because it represents a fundamental and unchangeable limit imposed by mathematics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable as this is a mountain. There are no beneficiaries or victims; the FGH is a fundamental mathematical construct.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply as this is a Mountain. It cannot be misclassified as a Snare or Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fgh_hierarchy_2026, 0, 100).

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
