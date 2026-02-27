% ============================================================================
% CONSTRAINT STORY: fast_growing_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fast_growing_hierarchy, []).

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
 *   constraint_id: fast_growing_hierarchy
 *   human_readable: The Fast-Growing Hierarchy (FGH)
 *   domain: technological
 *
 * SUMMARY:
 *   The Fast-Growing Hierarchy is a family of functions indexed by ordinals
 *   that classifies the growth rate of computable functions. It is a
 *   fundamental concept in computability theory and provides a framework for
 *   understanding the limits of computation.
 *
 * KEY AGENTS:
 *   - Mathematical Logician: Analyzes the formal properties of the hierarchy.
 *   - Theoretical Computer Scientist: Applies the hierarchy to classify the complexity of algorithms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fast_growing_hierarchy, 0.1).
domain_priors:suppression_score(fast_growing_hierarchy, 0.02).
domain_priors:theater_ratio(fast_growing_hierarchy, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fast_growing_hierarchy, extractiveness, 0.1).
narrative_ontology:constraint_metric(fast_growing_hierarchy, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fast_growing_hierarchy, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fast_growing_hierarchy, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(fast_growing_hierarchy, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fast_growing_hierarchy, mountain).
narrative_ontology:human_readable(fast_growing_hierarchy, "The Fast-Growing Hierarchy (FGH)").
narrative_ontology:topic_domain(fast_growing_hierarchy, "technological").

domain_priors:emerges_naturally(fast_growing_hierarchy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The FGH provides a fundamental framework for understanding computational complexity and growth rates.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The FGH serves as a benchmark for classifying the complexity of algorithms and computational models.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fast_growing_hierarchy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, ExtMetricName, E),
    domain_priors:suppression_score(fast_growing_hierarchy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fast_growing_hierarchy),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fast_growing_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The FGH is a mathematical structure with well-defined properties, independent of any particular implementation or application. It is a fundamental limit on computation.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives view the FGH as a Mountain because it represents an inherent limit, although their reasons vary.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mathematical structure with inherent properties that are objective. There are no beneficiaries or victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fast_growing_hierarchy, 0, 100).

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
