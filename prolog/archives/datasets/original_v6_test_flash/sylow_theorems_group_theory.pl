% ============================================================================
% CONSTRAINT STORY: sylow_theorems_group_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sylow_theorems_group_theory, []).

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
 *   constraint_id: sylow_theorems_group_theory
 *   human_readable: Sylow Theorems (Finite Group Structure)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Sylow Theorems are a fundamental result in finite group theory that
 *   guarantee the existence and properties of subgroups of prime power order.
 *   They provide powerful tools for understanding the structure of finite
 *   groups and are considered a cornerstone of the field.
 *
 * KEY AGENTS:
 *   - Novice Group Theorist: (powerless/analytical) - Learns and applies the theorems.
 *   - Group Theory Research Community: (institutional/analytical) - Uses and extends the theorems.
 *   - Analytical Observer: (analytical/analytical) - Recognizes the theorems as a mathematical truth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sylow_theorems_group_theory, 0.0).
domain_priors:suppression_score(sylow_theorems_group_theory, 0.0).
domain_priors:theater_ratio(sylow_theorems_group_theory, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sylow_theorems_group_theory, extractiveness, 0.0).
narrative_ontology:constraint_metric(sylow_theorems_group_theory, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sylow_theorems_group_theory, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sylow_theorems_group_theory, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sylow_theorems_group_theory, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sylow_theorems_group_theory, mountain).
narrative_ontology:human_readable(sylow_theorems_group_theory, "Sylow Theorems (Finite Group Structure)").
narrative_ontology:topic_domain(sylow_theorems_group_theory, "mathematical").

domain_priors:emerges_naturally(sylow_theorems_group_theory).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Theorems hold regardless of computational skill.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Theorems are foundational to group theory research.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Theorems are a fundamental mathematical truth about finite groups.
constraint_indexing:constraint_classification(sylow_theorems_group_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sylow_theorems_group_theory_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sylow_theorems_group_theory, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sylow_theorems_group_theory, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sylow_theorems_group_theory, ExtMetricName, E),
    domain_priors:suppression_score(sylow_theorems_group_theory, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sylow_theorems_group_theory),
    narrative_ontology:constraint_metric(sylow_theorems_group_theory, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sylow_theorems_group_theory, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sylow_theorems_group_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness, suppression and theater ratio are all 0 as this is a mathematical theorem. Accessibility collapse is high and resistance is low, indicating its a mountain. All the perspectives see it as a mountain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. Everyone with sufficient knowledge of mathematics sees the Sylow theorems as a fundamental mathematical truth.
 *
 * DIRECTIONALITY LOGIC:
 *   The Sylow Theorems are a fundamental mathematical truth. There are no beneficiaries or victims. The theorems simply exist as a part of the structure of mathematics.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable as extractiveness is zero
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sylow_theorems_group_theory, 0, 100).

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
