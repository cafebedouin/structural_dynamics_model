% ============================================================================
% CONSTRAINT STORY: peano_curve_mapping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peano_curve_mapping, []).

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
 *   constraint_id: peano_curve_mapping
 *   human_readable: Peano Space-Filling Curve
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   The Peano Curve is the first discovered space-filling curve, a continuous
 *   mapping from a one-dimensional interval onto a two-dimensional square.
 *   This is a mathematical result with no known exceptions, and few actors
 *   would disagree with this assessment.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Observer (analytical/analytical)
 *   - Technological Community: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peano_curve_mapping, 0.1).
domain_priors:suppression_score(peano_curve_mapping, 0.02).
domain_priors:theater_ratio(peano_curve_mapping, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peano_curve_mapping, extractiveness, 0.1).
narrative_ontology:constraint_metric(peano_curve_mapping, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(peano_curve_mapping, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(peano_curve_mapping, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(peano_curve_mapping, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peano_curve_mapping, mountain).
narrative_ontology:human_readable(peano_curve_mapping, "Peano Space-Filling Curve").
narrative_ontology:topic_domain(peano_curve_mapping, "mathematical/technological").

domain_priors:emerges_naturally(peano_curve_mapping).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Peano curve is a fundamental mathematical construct, an example of a continuous surjective mapping from 1D to 2D space. This mathematical fact is immutable.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of a computer scientist, the properties of Peano curves are mathematically fixed.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peano_curve_mapping_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(peano_curve_mapping, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peano_curve_mapping, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(peano_curve_mapping, ExtMetricName, E),
    domain_priors:suppression_score(peano_curve_mapping, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(peano_curve_mapping),
    narrative_ontology:constraint_metric(peano_curve_mapping, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(peano_curve_mapping, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(peano_curve_mapping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Peano curve is a mathematical result. The metrics are reflective of an immutable result. The extractiveness reflects the degree of potential use (low), suppression is limited to the degree to which one could disagree (close to zero), and theater ratio is minimal (the curve 'just is').
 *
 * PERSPECTIVAL GAP:
 *   There is no real perspectival gap: all valid perspectives classify this as a Mountain. The mathematical result is independent of the observer.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint is a mathematical property, so directionality is not applicable. The beneficiaries are all observers who use the mathematical result. No victim exists.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling coordination as pure extraction because it's a mathematical result, not a social arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peano_curve_mapping, 0, 100).

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
