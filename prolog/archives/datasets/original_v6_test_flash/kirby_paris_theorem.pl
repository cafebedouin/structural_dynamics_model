% ============================================================================
% CONSTRAINT STORY: kirby_paris_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kirby_paris_theorem, []).

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
 *   constraint_id: kirby_paris_theorem
 *   human_readable: The Kirby-Paris Theorem (Independence of Goodstein's Theorem)
 *   domain: mathematical_logic
 *
 * SUMMARY:
 *   The Kirby-Paris theorem demonstrates that Goodstein's theorem—a true
 *   statement about the termination of specific sequences of natural
 *   numbers—is unprovable within Peano Arithmetic (PA). This highlights an
 *   inherent limitation in the power of formal systems to prove all true
 *   statements, making it a fundamental concept in mathematical logic.
 *
 * KEY AGENTS:
 *   - The Unprovable Statement: A mathematical statement that is true but cannot be proven within Peano Arithmetic (powerless/trapped)
 *   - The Analytical Observer: A mathematician or logician who understands the implications of the theorem (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kirby_paris_theorem, 0.1).
domain_priors:suppression_score(kirby_paris_theorem, 0.01).
domain_priors:theater_ratio(kirby_paris_theorem, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kirby_paris_theorem, extractiveness, 0.1).
narrative_ontology:constraint_metric(kirby_paris_theorem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(kirby_paris_theorem, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kirby_paris_theorem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kirby_paris_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kirby_paris_theorem, mountain).
narrative_ontology:human_readable(kirby_paris_theorem, "The Kirby-Paris Theorem (Independence of Goodstein's Theorem)").
narrative_ontology:topic_domain(kirby_paris_theorem, "mathematical_logic").

domain_priors:emerges_naturally(kirby_paris_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a statement unprovable within PA, it's a mountain: an immutable limit of the formal system. No escape.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The analytical observer, understanding the theorem, sees an inherent limitation of Peano Arithmetic. It's a mountain due to the inherent nature of the system.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kirby_paris_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(kirby_paris_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kirby_paris_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kirby_paris_theorem, ExtMetricName, E),
    domain_priors:suppression_score(kirby_paris_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kirby_paris_theorem),
    narrative_ontology:constraint_metric(kirby_paris_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kirby_paris_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kirby_paris_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because the theorem doesn't actively extract from anything; it reveals a limitation. Suppression is low as well. The theorem doesn't suppress other theorems, it just shows what PA can't prove. Theater ratio is essentially nonexistent.
 *
 * PERSPECTIVAL GAP:
 *   Since this is a mountain classification, all perspectives are in agreement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kirby_paris_theorem, 0, 100).

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
