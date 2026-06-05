% ============================================================================
% CONSTRAINT STORY: busy_beaver_noncomputability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_busy_beaver_noncomputability, []).

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
 *   constraint_id: busy_beaver_noncomputability
 *   human_readable: The Non-Computability of the Busy Beaver Function (Σ)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   The Busy Beaver function, Σ(n), defines the maximum number of steps a
 *   halting Turing machine with n states can take. Its non-computability
 *   means there exists no algorithm that can compute Σ(n) for all n. This is
 *   a fundamental limitation in computation.
 *
 * KEY AGENTS:
 *   - Novice Programmer: Powerless/Trapped - Limited by the non-computability.
 *   - Theoretical Computer Science Community: Institutional/Analytical - Understands and works within the limitation.
 *   - Analytical Observer: Analytical/Analytical - Recognizes the fundamental nature of the limit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(busy_beaver_noncomputability, 0.1).
domain_priors:suppression_score(busy_beaver_noncomputability, 0.05).
domain_priors:theater_ratio(busy_beaver_noncomputability, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, extractiveness, 0.1).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(busy_beaver_noncomputability, mountain).
narrative_ontology:human_readable(busy_beaver_noncomputability, "The Non-Computability of the Busy Beaver Function (Σ)").
narrative_ontology:topic_domain(busy_beaver_noncomputability, "technological/mathematical").

domain_priors:emerges_naturally(busy_beaver_noncomputability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVICE PROGRAMMER (MOUNTAIN) - Regardless of programming skill, the novice programmer will always be limited by the inherent non-computability of the Busy Beaver function. No exit available.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THEORETICAL COMPUTER SCIENCE COMMUNITY (MOUNTAIN) - As an institution, the theoretical computer science community is limited by the inherent non-computability of the Busy Beaver function.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) - The analytical observer recognizes the non-computability of the Busy Beaver function as an inherent mathematical limitation.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(busy_beaver_noncomputability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, ExtMetricName, E),
    domain_priors:suppression_score(busy_beaver_noncomputability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(busy_beaver_noncomputability),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(busy_beaver_noncomputability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Busy Beaver function's non-computability is a mathematical fact. Its extractiveness is low, as it doesn't actively extract resources but limits what can be computed. Suppression is low because it doesn't prevent research, only limits its scope. The theater ratio is low, as there is little performative activity associated with this constraint.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because the non-computability is a fundamental mathematical truth, perceived the same way by different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents are affected by this constraint equally, perceiving it as a fundamental limitation.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Mountain constraint. It cannot be misidentified as a form of extraction because it is an inherent mathematical limitation, not a social or economic constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(busy_beaver_noncomputability, 0, 100).

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
