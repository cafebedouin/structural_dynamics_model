% ============================================================================
% CONSTRAINT STORY: hilberts_hotel_infinity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hilberts_hotel_infinity, []).

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
 *   constraint_id: hilberts_hotel_infinity
 *   human_readable: Hilbert's Paradox of the Grand Hotel
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Hilbert's Hotel is a thought experiment illustrating the
 *   counter-intuitive properties of infinite sets. It demonstrates that a
 *   fully occupied hotel with infinitely many rooms can accommodate
 *   additional guests, even infinitely many, without anyone having to leave
 *   the hotel. This is possible due to the properties of infinity.
 *
 * KEY AGENTS:
 *   - Naive Guest: Lacks understanding of set theory
 *   - Hotel Manager: Understands and applies the shifting principle.
 *   - Analytical Observer: Understands the mathematical principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hilberts_hotel_infinity, 0.01).
domain_priors:suppression_score(hilberts_hotel_infinity, 0.01).
domain_priors:theater_ratio(hilberts_hotel_infinity, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hilberts_hotel_infinity, extractiveness, 0.01).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hilberts_hotel_infinity, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hilberts_hotel_infinity, mountain).
narrative_ontology:human_readable(hilberts_hotel_infinity, "Hilbert's Paradox of the Grand Hotel").
narrative_ontology:topic_domain(hilberts_hotel_infinity, "mathematical/logical").

domain_priors:emerges_naturally(hilberts_hotel_infinity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A guest with no understanding of set theory is trapped by the seeming impossibility, but the hotel can always make space.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The hotel manager understands how to shift guests around to make room. Analytical exit.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The observer understands the mathematical principle at work.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hilberts_hotel_infinity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, ExtMetricName, E),
    domain_priors:suppression_score(hilberts_hotel_infinity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hilberts_hotel_infinity),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hilberts_hotel_infinity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness, suppression and theater ratio are all near zero, as the scenario simply demonstrates a mathematical truth. High accessibility collapse and low resistance, characteristic of natural laws.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives agree on the 'mountain' classification, as the mathematical properties are universal. The difference is in the understanding of *why* it is a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is universally true, so directionality is not relevant.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy issue because no extraction takes place. The only point of concern might be that it is presented as 'obvious', though not intuitive, and so possibly a kind of theatrical mountain. The theater ratio, however, is minimal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hilberts_hotel_infinity, 0, 1).

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
