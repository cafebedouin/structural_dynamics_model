% ============================================================================
% CONSTRAINT STORY: collatz_conjecture_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collatz_conjecture_determinism, []).

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
 *   constraint_id: collatz_conjecture_determinism
 *   human_readable: The Collatz Conjecture (3n + 1) Determinism
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Collatz conjecture is a famous unsolved problem in mathematics. It
 *   states that for any positive integer, repeatedly applying the function
 *   (n/2 if n is even, 3n+1 if n is odd) will always eventually reach 1. This
 *   constraint story focuses on the inherent determinism of the conjecture:
 *   either it is true for all positive integers, or there exists a
 *   counterexample. The conjecture's status is independent of human knowledge
 *   or computational power; it is a fixed property of the mathematical
 *   universe.
 *
 * KEY AGENTS:
 *   - Mathematician: Attempts to prove or disprove the conjecture (analytical/analytical)
 *   - Computer Scientist: Searches for counterexamples using computational resources (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collatz_conjecture_determinism, 0.01).
domain_priors:suppression_score(collatz_conjecture_determinism, 0.01).
domain_priors:theater_ratio(collatz_conjecture_determinism, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collatz_conjecture_determinism, extractiveness, 0.01).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(collatz_conjecture_determinism, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collatz_conjecture_determinism, mountain).
narrative_ontology:human_readable(collatz_conjecture_determinism, "The Collatz Conjecture (3n + 1) Determinism").
narrative_ontology:topic_domain(collatz_conjecture_determinism, "mathematical/logical").

domain_priors:emerges_naturally(collatz_conjecture_determinism).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From a universal perspective, either the conjecture holds for all numbers, or there is a counterexample. The truth is fixed and independent of any agent's actions.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of a computer scientist attempting to find a counterexample, the determinism is still a fixed property that governs computation. The resources available do not change the underlying truth.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collatz_conjecture_determinism_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, ExtMetricName, E),
    domain_priors:suppression_score(collatz_conjecture_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(collatz_conjecture_determinism),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(collatz_conjecture_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness, suppression, and theater ratio are all very low because the conjecture is believed to be an objective truth, independent of any actor's power or actions. The focus is on the existing or non-existing counterexample to the conjecture, not its possible influence on mathematical or other activities.
 *
 * PERSPECTIVAL GAP:
 *   Since this is a Mountain constraint, all perspectives classify as Mountain. The truth of the conjecture is independent of the observer's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The Collatz conjecture is a statement about a mathematical truth. There are no direct beneficiaries or victims. The agents are researchers who seek to understand the truth, and their efforts do not change the truth value of the conjecture itself. The deterministic nature implies a fixed state that isn't 'extracted' or 'suppressed' by any agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a Mountain, reflecting its deterministic nature and independence from human intervention. The alternative classifications are not applicable, as the Collatz Conjecture's truth is considered an intrinsic property rather than the result of coordination, extraction, or temporary support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collatz_conjecture_determinism, 0, 100).

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
