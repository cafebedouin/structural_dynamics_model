% ============================================================================
% CONSTRAINT STORY: van_der_waerden
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_van_der_waerden, []).

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
 *   constraint_id: van_der_waerden
 *   human_readable: Van der Waerden's Theorem
 *   domain: mathematical
 *
 * SUMMARY:
 *   Van der Waerden's theorem is a fundamental result in Ramsey theory,
 *   stating that for any given positive integers r and k, there exists a
 *   number N such that if the integers {1, 2, ..., N} are colored with r
 *   different colors, then there necessarily exists an arithmetic progression
 *   of length k all of whose terms are the same color. It is an example of a
 *   mathematical truth that is considered a 'mountain' constraint,
 *   representing a fixed and unchangeable aspect of mathematical reality.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Sees the theorem as an immutable mathematical truth.
 *   - Beginning Student: Learns and understands the theorem as an established fact.
 *   - Mathematician: Uses the theorem as a building block for further research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(van_der_waerden, 0.01).
domain_priors:suppression_score(van_der_waerden, 0.0).
domain_priors:theater_ratio(van_der_waerden, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(van_der_waerden, extractiveness, 0.01).
narrative_ontology:constraint_metric(van_der_waerden, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(van_der_waerden, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(van_der_waerden, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(van_der_waerden, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(van_der_waerden, mountain).
narrative_ontology:human_readable(van_der_waerden, "Van der Waerden's Theorem").
narrative_ontology:topic_domain(van_der_waerden, "mathematical").

domain_priors:emerges_naturally(van_der_waerden).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical perspective, Van der Waerden's theorem is a fundamental mathematical truth, regardless of the observer.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even a beginning student, once they understand the proof, will see the theorem as an immutable truth.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(van_der_waerden_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(van_der_waerden, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(van_der_waerden, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(van_der_waerden, ExtMetricName, E),
    domain_priors:suppression_score(van_der_waerden, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(van_der_waerden),
    narrative_ontology:constraint_metric(van_der_waerden, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(van_der_waerden, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(van_der_waerden_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness and suppression are both very low, as the theorem does not extract resources or suppress alternatives. The theater ratio is also low, as there is no performative aspect to the theorem. The theorem emerges naturally from the axioms of mathematics.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap, as the theorem is universally accepted as a fundamental mathematical truth. All observers, regardless of their power or exit options, will classify the theorem as a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Since it's a mathematical theorem, directionality is not applicable. There are no beneficiaries or victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not applicable as the theorem is a fundamental mathematical truth, not a social or political construct that could be misconstrued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(van_der_waerden, 0, 100).

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
