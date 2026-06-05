% ============================================================================
% CONSTRAINT STORY: ship_of_theseus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ship_of_theseus, []).

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
 *   constraint_id: ship_of_theseus
 *   human_readable: Identity Continuity Constraint
 *   domain: philosophical
 *
 * SUMMARY:
 *   The Ship of Theseus is a philosophical thought experiment that raises the
 *   question of whether an object that has had all of its component parts
 *   replaced remains fundamentally the same object. This poses a constraint
 *   on how we define identity and continuity.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Sees a fundamental limit on defining identity materially.
 *   - Naive Observer: Views the paradox as representing absurdities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ship_of_theseus, 0.1).
domain_priors:suppression_score(ship_of_theseus, 0.05).
domain_priors:theater_ratio(ship_of_theseus, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ship_of_theseus, extractiveness, 0.1).
narrative_ontology:constraint_metric(ship_of_theseus, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(ship_of_theseus, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ship_of_theseus, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ship_of_theseus, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ship_of_theseus, mountain).
narrative_ontology:human_readable(ship_of_theseus, "Identity Continuity Constraint").
narrative_ontology:topic_domain(ship_of_theseus, "philosophical").

domain_priors:emerges_naturally(ship_of_theseus).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical perspective, the constraint represents a fundamental limit on defining identity through material composition. The ship's continued existence relies on a conceptual framework rather than a fixed set of components.
constraint_indexing:constraint_classification(ship_of_theseus, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% A person with no philosophical training might find the paradox unsolvable, or the concept of identity as fixed in time and space to be so. They might view the idea that replacing parts creates a new ship as absurd, or they may believe in an immutable essence.
constraint_indexing:constraint_classification(ship_of_theseus, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ship_of_theseus_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ship_of_theseus, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ship_of_theseus, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ship_of_theseus, ExtMetricName, E),
    domain_priors:suppression_score(ship_of_theseus, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ship_of_theseus),
    narrative_ontology:constraint_metric(ship_of_theseus, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ship_of_theseus, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ship_of_theseus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low as the constraint does not extract resources but poses a conceptual challenge. Suppression is low as there are alternative viewpoints and interpretations of identity. Theater ratio is low, as the constraint is primarily a conceptual exercise.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives classify as mountain, reflecting the immutable nature of the conceptual problem. The analytical observer understands the underlying logical structure, while a naive observer simply perceives an unsolvable paradox.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ship_of_theseus, 0, 100).

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
