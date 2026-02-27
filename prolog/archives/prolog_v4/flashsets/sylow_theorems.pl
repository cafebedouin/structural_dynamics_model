% ============================================================================
% CONSTRAINT STORY: sylow_theorems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sylow_theorems, []).

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
 *   constraint_id: sylow_theorems
 *   human_readable: Sylow Theorems
 *   domain: mathematics/group_theory
 *
 * SUMMARY:
 *   Sylow Theorems are fundamental results in group theory that provide
 *   powerful constraints on the structure of finite groups. They are used
 *   extensively in classifying finite groups and are a cornerstone of modern
 *   algebra.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sylow_theorems, 0.01).
domain_priors:suppression_score(sylow_theorems, 0.01).
domain_priors:theater_ratio(sylow_theorems, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sylow_theorems, extractiveness, 0.01).
narrative_ontology:constraint_metric(sylow_theorems, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(sylow_theorems, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sylow_theorems, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sylow_theorems, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sylow_theorems, mountain).
narrative_ontology:human_readable(sylow_theorems, "Sylow Theorems").
narrative_ontology:topic_domain(sylow_theorems, "mathematics/group_theory").

domain_priors:emerges_naturally(sylow_theorems).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a powerless agent, the Sylow theorems are a fixed mathematical truth. They cannot be changed or avoided.
constraint_indexing:constraint_classification(sylow_theorems, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of a mathematical institution, the Sylow theorems are a foundational element, unchangeable and universally applicable.
constraint_indexing:constraint_classification(sylow_theorems, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From an analytical perspective, the Sylow theorems are fundamental mathematical truths, arising from the axioms of group theory.
constraint_indexing:constraint_classification(sylow_theorems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sylow_theorems_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sylow_theorems, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sylow_theorems, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sylow_theorems, ExtMetricName, E),
    domain_priors:suppression_score(sylow_theorems, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sylow_theorems),
    narrative_ontology:constraint_metric(sylow_theorems, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sylow_theorems, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sylow_theorems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Sylow theorems are mathematical theorems. They are true by definition and cannot be violated. Therefore, extractiveness and suppression are very low, and the theater ratio is also very low because there is no performance associated with a theorem.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap because the Sylow theorems are a fundamental truth regardless of the observer's power, time horizon, or exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint, there is no concept of beneficiaries or victims. The theorem simply exists and applies universally.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not relevant for mathematical theorems. The classification is based on the objective truth of the theorem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sylow_theorems, 0, 100).

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
