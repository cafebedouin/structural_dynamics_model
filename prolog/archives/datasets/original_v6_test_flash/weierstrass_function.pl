% ============================================================================
% CONSTRAINT STORY: weierstrass_function
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_weierstrass_function, []).

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
 *   constraint_id: weierstrass_function
 *   human_readable: The Existence of Continuous, Nowhere-Differentiable Functions
 *   domain: mathematics
 *
 * SUMMARY:
 *   The Weierstrass function is a canonical example in mathematics of a
 *   function that is continuous everywhere but differentiable nowhere. Its
 *   existence is a fundamental result in real analysis, demonstrating that
 *   continuity does not imply differentiability. This constraint highlights a
 *   limit on what can be inferred from continuity alone. The proof is
 *   mathematically rigorous and universally accepted.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Fully understands and accepts the mathematical proof.
 *   - Novice Student: Initially struggles with the concept but eventually accepts it.
 *   - Mathematical Community: Universally accepts the existence of these functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(weierstrass_function, 0.1).
domain_priors:suppression_score(weierstrass_function, 0.02).
domain_priors:theater_ratio(weierstrass_function, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(weierstrass_function, extractiveness, 0.1).
narrative_ontology:constraint_metric(weierstrass_function, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(weierstrass_function, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(weierstrass_function, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(weierstrass_function, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(weierstrass_function, mountain).
narrative_ontology:human_readable(weierstrass_function, "The Existence of Continuous, Nowhere-Differentiable Functions").
narrative_ontology:topic_domain(weierstrass_function, "mathematics").

domain_priors:emerges_naturally(weierstrass_function).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical perspective, the existence of continuous, nowhere-differentiable functions is a fundamental mathematical truth.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The student encountering this concept for the first time might feel trapped by its counter-intuitiveness, but ultimately accepts it as a mathematical reality.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% The mathematical community universally accepts the existence of these functions as a cornerstone of real analysis.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(weierstrass_function_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(weierstrass_function, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(weierstrass_function, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(weierstrass_function, ExtMetricName, E),
    domain_priors:suppression_score(weierstrass_function, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(weierstrass_function),
    narrative_ontology:constraint_metric(weierstrass_function, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(weierstrass_function, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(weierstrass_function_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because the function's existence is a mathematical fact rather than a source of significant extraction. The suppression is also low because alternative viewpoints are not actively suppressed; the function's existence is simply a proven result. The theater ratio is low because there is little performative activity associated with this constraint; it is a purely mathematical concept.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives are similar because the existence of the Weierstrass function is a mathematical truth that is accepted across different levels of mathematical understanding. Any initial difficulty a student has is due to their own learning curve, not a fundamental conflict in perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no significant beneficiaries or victims associated with the constraint, as it is a mathematical fact. All directionality values are therefore neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a mountain because it is a fundamental mathematical truth that cannot be changed or manipulated. It is not a rope, snare, scaffold, or piton because it does not involve coordination, extraction, temporary support, or degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(weierstrass_function, 0, 100).

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
