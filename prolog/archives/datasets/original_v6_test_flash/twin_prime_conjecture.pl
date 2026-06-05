% ============================================================================
% CONSTRAINT STORY: twin_prime_conjecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_twin_prime_conjecture, []).

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
 *   constraint_id: twin_prime_conjecture
 *   human_readable: The Unproven Nature of the Twin Prime Conjecture
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The twin prime conjecture, a long-standing problem in number theory,
 *   states that there are infinitely many pairs of prime numbers that differ
 *   by 2 (twin primes). Despite significant effort, a definitive proof
 *   remains elusive. This constraint captures the inherent difficulty and
 *   unproven status of the conjecture.
 *
 * KEY AGENTS:
 *   - Frustrated Number Theorist: Powerless/Trapped - Individual mathematician struggling to prove the conjecture.
 *   - Analytical Observer: Analytical/Analytical - An objective observer assessing the problem's difficulty.
 *   - Mathematical Community: Institutional/Analytical - The collective body of mathematicians acknowledging the unproven nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(twin_prime_conjecture, 0.1).
domain_priors:suppression_score(twin_prime_conjecture, 0.05).
domain_priors:theater_ratio(twin_prime_conjecture, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(twin_prime_conjecture, extractiveness, 0.1).
narrative_ontology:constraint_metric(twin_prime_conjecture, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(twin_prime_conjecture, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(twin_prime_conjecture, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(twin_prime_conjecture, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(twin_prime_conjecture, mountain).
narrative_ontology:human_readable(twin_prime_conjecture, "The Unproven Nature of the Twin Prime Conjecture").
narrative_ontology:topic_domain(twin_prime_conjecture, "mathematical/logical").

domain_priors:emerges_naturally(twin_prime_conjecture).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Frustrated Number Theorist (Mountain) - Despite attempts, they cannot prove the conjecture. Feels trapped by the problem's difficulty, powerlessness to overcome it.
constraint_indexing:constraint_classification(twin_prime_conjecture, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Perspective 2: The Analytical Observer (Mountain) - Views the conjecture's unproven status as an inherent property of the mathematical system. No agent can alter this, it is a fundamental limit given current mathematical tools.
constraint_indexing:constraint_classification(twin_prime_conjecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective 3: The Mathematical Community (Mountain) - The community as a whole accepts the unproven nature as a given. The conjecture remains a challenge, but its unproven status itself has become a mountain that frames research directions.
constraint_indexing:constraint_classification(twin_prime_conjecture, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(twin_prime_conjecture_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(twin_prime_conjecture, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(twin_prime_conjecture, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(twin_prime_conjecture, ExtMetricName, E),
    domain_priors:suppression_score(twin_prime_conjecture, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(twin_prime_conjecture),
    narrative_ontology:constraint_metric(twin_prime_conjecture, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(twin_prime_conjecture, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(twin_prime_conjecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.10) because the unproven nature does not directly extract resources, but rather redirects efforts. Suppression is low (0.05) as researchers are free to attempt to prove or disprove the conjecture. Theater Ratio is also low (0.15) as the community generally agrees with the current unproven status; and does not need to engage in performative action to maintain said status. These all point to the classification of a mountain.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives agree on the mountain classification. The number theorist feels trapped by the problem, the analyst sees it as an inherent limit, and the community treats it as a structural given.
 *
 * DIRECTIONALITY LOGIC:
 *   Due to its mountain classification no beneficiaries or victims are declared.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(twin_prime_conjecture, 0, 100).

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
