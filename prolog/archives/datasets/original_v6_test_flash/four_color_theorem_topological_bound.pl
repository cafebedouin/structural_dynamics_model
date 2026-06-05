% ============================================================================
% CONSTRAINT STORY: four_color_theorem_topological_bound
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_four_color_theorem_topological_bound, []).

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
 *   constraint_id: four_color_theorem_topological_bound
 *   human_readable: The Four Color Theorem
 *   domain: mathematical/topological
 *
 * SUMMARY:
 *   The Four Color Theorem states that no more than four colors are required
 *   to color the regions of any map on a plane such that no two adjacent
 *   regions have the same color. This is a fundamental topological
 *   constraint.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Sees the theorem as a mathematical truth.
 *   - Naive Map Maker: Discovers the constraint empirically.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(four_color_theorem_topological_bound, 0.0).
domain_priors:suppression_score(four_color_theorem_topological_bound, 0.0).
domain_priors:theater_ratio(four_color_theorem_topological_bound, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, extractiveness, 0.0).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(four_color_theorem_topological_bound, mountain).
narrative_ontology:human_readable(four_color_theorem_topological_bound, "The Four Color Theorem").
narrative_ontology:topic_domain(four_color_theorem_topological_bound, "mathematical/topological").

domain_priors:emerges_naturally(four_color_theorem_topological_bound).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The theorem, once proven, is a fundamental mathematical truth.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even someone unfamiliar with advanced mathematics will find that they cannot construct a planar map requiring more than four colors.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(four_color_theorem_topological_bound_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, ExtMetricName, E),
    domain_priors:suppression_score(four_color_theorem_topological_bound, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(four_color_theorem_topological_bound),
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(four_color_theorem_topological_bound_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are both zero because the theorem is a fundamental constraint, not an imposed one. The theater ratio is zero because there is no performative aspect. Accessibility collapse is 1.0 because the constraint is universally accessible. Resistance is 0.0 because there is no resistance to a mathematical truth.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All observers classify the theorem as a mountain. Even those not mathematically sophisticated will be unable to construct a planar map requiring more than four colors.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable since this is a mountain classification. There are no beneficiaries or victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable, since this is a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(four_color_theorem_topological_bound, 0, 100).

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
