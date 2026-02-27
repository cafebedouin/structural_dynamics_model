% ============================================================================
% CONSTRAINT STORY: metabolic_constraint_cognition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metabolic_constraint_cognition, []).

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
 *   constraint_id: metabolic_constraint_cognition
 *   human_readable: The ATP Ceiling as an Exploitable Limit
 *   domain: biological/technological/economic
 *
 * SUMMARY:
 *   This constraint represents the hard biological limit of the human brain's
 *   metabolic capacity, specifically the rate at which adenosine triphosphate
 *   (ATP) can be produced and utilized. This limit constrains cognitive
 *   function and is largely immutable under current biological conditions.
 *   It's a fundamental physical law governing the speed and intensity of
 *   neural processing.
 *
 * KEY AGENTS:
 *   - Individual Brain: Powerless/Trapped - limited by inherent biological constraints.
 *   - Analytical Observer: Analytical/Analytical - observes the constraint as a fundamental limit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metabolic_constraint_cognition, 0.15).
domain_priors:suppression_score(metabolic_constraint_cognition, 0.01).
domain_priors:theater_ratio(metabolic_constraint_cognition, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metabolic_constraint_cognition, extractiveness, 0.15).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(metabolic_constraint_cognition, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metabolic_constraint_cognition, mountain).
narrative_ontology:human_readable(metabolic_constraint_cognition, "The ATP Ceiling as an Exploitable Limit").
narrative_ontology:topic_domain(metabolic_constraint_cognition, "biological/technological/economic").

domain_priors:emerges_naturally(metabolic_constraint_cognition).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the individual brain, unable to exceed its ATP limit.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Analytical perspective viewing the ATP ceiling as a fundamental biological constraint.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metabolic_constraint_cognition_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(metabolic_constraint_cognition, ExtMetricName, E),
    domain_priors:suppression_score(metabolic_constraint_cognition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(metabolic_constraint_cognition),
    narrative_ontology:constraint_metric(metabolic_constraint_cognition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(metabolic_constraint_cognition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(metabolic_constraint_cognition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint doesn't extract anything; it simply limits. Suppression is also low because there's no active suppression of alternatives; it's a natural limit. The theater ratio is low as there is no performative activity involved.
 *
 * PERSPECTIVAL GAP:
 *   There isn't much of a perspectival gap as the constraint is viewed as a fundamental limit from both perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is minimal because the ATP ceiling is not an exploitable or manipulable constraint, but a hard limit of a biological system.
 *
 * MANDATROPHY ANALYSIS:
 *   N/A, as the constraint is a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metabolic_constraint_cognition, 0, 100).

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
