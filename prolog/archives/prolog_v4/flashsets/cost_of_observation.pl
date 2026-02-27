% ============================================================================
% CONSTRAINT STORY: cost_of_observation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cost_of_observation, []).

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
 *   constraint_id: cost_of_observation
 *   human_readable: The Evolutionary Cost of Observation
 *   domain: philosophical/evolutionary
 *
 * SUMMARY:
 *   This meta-constraint describes the 'tax' paid by any situated observer.
 *   Observation requires energy, resources, and processing power. This tax is
 *   inherent to any system attempting to model or interact with its
 *   environment. No observer escapes this cost, regardless of scale or
 *   complexity. Because the 'cost' is universal and unavoidable, the mountain
 *   type is appropriate.
 *
 * KEY AGENTS:
 *   - Powerless Organism: Bears the cost of observation within its niche (powerless/trapped)
 *   - Analytical Observer: Recognizes the universal cost of observation across all systems (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cost_of_observation, 0.15).
domain_priors:suppression_score(cost_of_observation, 0.05).
domain_priors:theater_ratio(cost_of_observation, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cost_of_observation, extractiveness, 0.15).
narrative_ontology:constraint_metric(cost_of_observation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(cost_of_observation, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cost_of_observation, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(cost_of_observation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cost_of_observation, mountain).
narrative_ontology:human_readable(cost_of_observation, "The Evolutionary Cost of Observation").
narrative_ontology:topic_domain(cost_of_observation, "philosophical/evolutionary").

domain_priors:emerges_naturally(cost_of_observation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a powerless organism, the cost of observation is a fundamental constraint. It is trapped in its niche and cannot escape the need to expend energy and resources to perceive its environment.
constraint_indexing:constraint_classification(cost_of_observation, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% An analytical observer, considering the long-term evolution of life, recognizes that any system capable of observation must expend resources to do so. This is a fundamental physical limit.
constraint_indexing:constraint_classification(cost_of_observation, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cost_of_observation_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cost_of_observation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cost_of_observation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cost_of_observation, ExtMetricName, E),
    domain_priors:suppression_score(cost_of_observation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cost_of_observation),
    narrative_ontology:constraint_metric(cost_of_observation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cost_of_observation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cost_of_observation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint is not actively extracting resources, but merely reflecting the inherent cost of any observation. Suppression is very low (0.05) because there is effectively no alternative. This cost emerges naturally.
 *
 * PERSPECTIVAL GAP:
 *   Both the powerless organism and the analytical observer view the cost of observation as a fundamental, inescapable constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is largely irrelevant as this is a mountain constraint. The cost exists for all observers regardless of their position or power.
 *
 * MANDATROPHY ANALYSIS:
 *   The mountain classification is appropriate because the cost of observation is not a social construct or an artifact of a specific system. It is a fundamental consequence of the laws of physics and information theory. It is not a rope because there is no coordination involved. It is not a snare because there is no active extraction. It is not a scaffold or a piton because there is no temporary or degraded structure involved. It is simply a fundamental limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cost_of_observation, 0, 100).

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
