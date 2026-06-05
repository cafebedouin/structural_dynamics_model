% ============================================================================
% CONSTRAINT STORY: silver_scarcity_mountain_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silver_scarcity_mountain_2026, []).

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
 *   constraint_id: silver_scarcity_mountain_2026
 *   human_readable: The Silver Physical Scarcity Mountain
 *   domain: economic/industrial/geopolitical
 *
 * SUMMARY:
 *   By 2026, silver has been designated a 'Critical Mineral' by the USGS,
 *   recognizing its essential role in various industries and its limited
 *   availability. While substitution is possible in some applications,
 *   silver's unique properties make it irreplaceable in many crucial
 *   technologies. The Earth's crust contains a finite amount of silver,
 *   creating an absolute limit to its supply.
 *
 * KEY AGENTS:
 *   - The Earth's Crust: Sets the ultimate physical limit.
 *   - Industrial Consumers: Constrained by the limited supply.
 *   - Analytical Observer: Understands the physical limits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silver_scarcity_mountain_2026, 0.05).
domain_priors:suppression_score(silver_scarcity_mountain_2026, 0.01).
domain_priors:theater_ratio(silver_scarcity_mountain_2026, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, extractiveness, 0.05).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silver_scarcity_mountain_2026, mountain).
narrative_ontology:human_readable(silver_scarcity_mountain_2026, "The Silver Physical Scarcity Mountain").
narrative_ontology:topic_domain(silver_scarcity_mountain_2026, "economic/industrial/geopolitical").

domain_priors:emerges_naturally(silver_scarcity_mountain_2026).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The total amount of silver is capped by the Earth's crust. There's no escape from this constraint.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% Industrial consumers are constrained by physical limits. Substitution is possible but only up to a point.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer understands the physical limits to silver availability.
constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silver_scarcity_mountain_2026_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silver_scarcity_mountain_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, ExtMetricName, E),
    domain_priors:suppression_score(silver_scarcity_mountain_2026, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(silver_scarcity_mountain_2026),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(silver_scarcity_mountain_2026, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(silver_scarcity_mountain_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Very low. The constraint isn't actively extracting value but represents a hard physical limit. Suppression (0.01): Extremely low. There's little active suppression, the constraint derives from the inherent scarcity. Theater ratio (0.01): Minimal. Almost no performative activity associated with the physical constraint.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on the 'Mountain' classification, reflecting the universal and immutable nature of the physical limit.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value is near 1.0 for all agents, given their universal subjection to the Silver's limited availability. There are no beneficiaries or victims because it is a physical constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification correctly identifies this constraint as a hard physical limit, preventing misclassification as a Snare (artificial scarcity) or Tangled Rope (managed allocation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silver_scarcity_mountain_2026, 2026, 2050).

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
