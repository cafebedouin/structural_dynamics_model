% ============================================================================
% CONSTRAINT STORY: kleene_recursion_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kleene_recursion_theorem, []).

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
 *   constraint_id: kleene_recursion_theorem
 *   human_readable: Kleene's Second Recursion Theorem
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   Kleene's Second Recursion Theorem demonstrates the existence of
 *   self-referential programs. It is a fundamental result in computability
 *   theory and places limitations on the kinds of programs that can be
 *   created. The theorem proves that for any computable function that
 *   transforms programs, there exists a 'fixed-point' program that can access
 *   its own source code. This is viewed as a mountain because it represents a
 *   fundamental limitation.
 *
 * KEY AGENTS:
 *   - Uncomputable Functions: Primary target (powerless/trapped) - are limited by the implications of the theorem.
 *   - Theoretical Computer Science Community: Primary beneficiary (institutional/analytical) - uses the theorem as a tool.
 *   - Analytical Observer: Sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kleene_recursion_theorem, 0.1).
domain_priors:suppression_score(kleene_recursion_theorem, 0.05).
domain_priors:theater_ratio(kleene_recursion_theorem, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kleene_recursion_theorem, extractiveness, 0.1).
narrative_ontology:constraint_metric(kleene_recursion_theorem, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kleene_recursion_theorem, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kleene_recursion_theorem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kleene_recursion_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kleene_recursion_theorem, mountain).
narrative_ontology:human_readable(kleene_recursion_theorem, "Kleene's Second Recursion Theorem").
narrative_ontology:topic_domain(kleene_recursion_theorem, "mathematical/technological").

domain_priors:emerges_naturally(kleene_recursion_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Uncomputable functions are fundamentally limited. They are 'trapped' by the theorem's implications. The limitations imposed by the theorem are inescapable, as it is a foundational result.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Perspective 2: Theoretical computer scientists use this theorem as a tool, seeing it as a fixed property of computation. The community analytically utilizes the theorem, but cannot alter its fundamental nature. Exit is analytical acceptance of its inherent nature.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective 3: The theorem is an inherent property of computation. It cannot be circumvented or altered. An analytical observer acknowledges the theorem as a fixed constraint on computability.
constraint_indexing:constraint_classification(kleene_recursion_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kleene_recursion_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(kleene_recursion_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kleene_recursion_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kleene_recursion_theorem, ExtMetricName, E),
    domain_priors:suppression_score(kleene_recursion_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kleene_recursion_theorem),
    narrative_ontology:constraint_metric(kleene_recursion_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kleene_recursion_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kleene_recursion_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low as it doesn't actively extract anything; it reveals a limitation. Suppression is also low because it doesn't prevent computation, just reveals the necessary existence of programs that have access to their source code. The theater ratio is low, as the application of the theorem is mostly direct and functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify the theorem as a Mountain because it's seen as a fundamental property of computation that cannot be changed. Although the Computer Science community uses it as a tool, they cannot circumvent its fundamental nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Since the theorem is considered a fundamental truth, the relationship with the key agents is minimal extraction. The uncomputable functions are considered powerless because they are fundamentally limited by the theorem.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable, as the constraint is classified as a Mountain, where the primary function is not easily mislabeled as pure extraction, or vice versa.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kleene_recursion_theorem, 0, 100).

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
