% ============================================================================
% CONSTRAINT STORY: three_body_unpredicability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_three_body_unpredicability, []).

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
 *   constraint_id: three_body_unpredicability
 *   human_readable: The Three-Body Problem (Computational Irreducibility)
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   The Three-Body Problem describes the motion of three celestial bodies
 *   interacting via gravity. Unlike the two-body problem, it generally does
 *   not have a closed-form solution and is computationally irreducible,
 *   meaning that its future state cannot be predicted exactly through any
 *   analytical method faster than simply simulating its evolution. This
 *   computational irreducibility places a limit on predictability.
 *
 * KEY AGENTS:
 *   - Powerless Computer: Primary target (powerless/trapped) - limited by computational resources.
 *   - Analytical Observer: Analytical observer (analytical/analytical) - views it as fundamental limit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(three_body_unpredicability, 0.15).
domain_priors:suppression_score(three_body_unpredicability, 0.05).
domain_priors:theater_ratio(three_body_unpredicability, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(three_body_unpredicability, extractiveness, 0.15).
narrative_ontology:constraint_metric(three_body_unpredicability, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(three_body_unpredicability, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(three_body_unpredicability, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(three_body_unpredicability, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(three_body_unpredicability, mountain).
narrative_ontology:human_readable(three_body_unpredicability, "The Three-Body Problem (Computational Irreducibility)").
narrative_ontology:topic_domain(three_body_unpredicability, "technological/scientific").

domain_priors:emerges_naturally(three_body_unpredicability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A computer trying to solve the three-body problem is trapped by the computational limits.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% From the perspective of an analytical observer, the computational irreducibility of the three-body problem represents a fundamental limit.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(three_body_unpredicability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(three_body_unpredicability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(three_body_unpredicability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(three_body_unpredicability, ExtMetricName, E),
    domain_priors:suppression_score(three_body_unpredicability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(three_body_unpredicability),
    narrative_ontology:constraint_metric(three_body_unpredicability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(three_body_unpredicability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(three_body_unpredicability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The constraint does not extract resources, but represents a fundamental limitation. Suppression (0.05): Very low. There is no active suppression, the constraint arises from the nature of the problem itself. Theater ratio (0.10): Low. Limited performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives (powerless computer and analytical observer) classify the problem as a mountain, due to the inherent computational limits. While approximations exist, the problem is not exactly solvable.
 *
 * DIRECTIONALITY LOGIC:
 *   The computer, unable to escape the constraint of computational complexity, is assigned 'trapped' exit options, leading to d = 1.0. The analytical observer, having the full picture and no direct involvement, assesses the constraint as a hard limit, also resulting in high directionality, effectively treating it as a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination, since the three-body problem presents a fundamental limit, rather than a coordination problem. While approximation methods may be seen as forms of coordination, these do not sidestep the computational irreducibility of the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(three_body_unpredicability, 0, 100).

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
