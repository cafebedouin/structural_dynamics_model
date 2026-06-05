% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_of_calculus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_theorem_of_calculus, []).

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
 *   constraint_id: fundamental_theorem_of_calculus
 *   human_readable: Fundamental Theorem of Calculus
 *   domain: technological
 *
 * SUMMARY:
 *   The Fundamental Theorem of Calculus establishes a relationship between
 *   differentiation and integration. It is a cornerstone of calculus,
 *   providing a powerful tool for evaluating definite integrals and solving
 *   differential equations. The theorem's validity is based on mathematical
 *   proof and is independent of human interpretation.
 *
 * KEY AGENTS:
 *   - Novice Learner: powerless/trapped
 *   - Mathematical Community: institutional/analytical
 *   - Analytical Observer: analytical/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_theorem_of_calculus, 0.01).
domain_priors:suppression_score(fundamental_theorem_of_calculus, 0.01).
domain_priors:theater_ratio(fundamental_theorem_of_calculus, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, extractiveness, 0.01).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_theorem_of_calculus, mountain).
narrative_ontology:human_readable(fundamental_theorem_of_calculus, "Fundamental Theorem of Calculus").
narrative_ontology:topic_domain(fundamental_theorem_of_calculus, "technological").

domain_priors:emerges_naturally(fundamental_theorem_of_calculus).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a novice learner, the theorem might initially seem complex and difficult to grasp, but its truth is not subject to their ability to understand it. It remains a fixed mathematical principle.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% The mathematical community relies on this theorem as a foundational element of calculus, essential for various calculations and proofs. Its validity is not contested within the accepted axioms of mathematics.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From an analytical perspective, the Fundamental Theorem of Calculus is a fundamental mathematical truth, independent of human actions or technological advancements. It's a cornerstone of calculus and analysis.
constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_theorem_of_calculus_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_theorem_of_calculus, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, ExtMetricName, E),
    domain_priors:suppression_score(fundamental_theorem_of_calculus, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fundamental_theorem_of_calculus),
    narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fundamental_theorem_of_calculus, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fundamental_theorem_of_calculus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are near zero because the theorem does not extract from any agent. The theorem emerges naturally and has high accessibility collapse and low resistance.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this as a mountain, illustrating a uniform type.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared as this is a mountain. The theorem is a fundamental truth that applies universally.
 *
 * MANDATROPHY ANALYSIS:
 *   As a mountain, this constraint does not permit mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_theorem_of_calculus, 0, 100).

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
