% ============================================================================
% CONSTRAINT STORY: tractarian_logic_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tractarian_logic_limit, []).

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
 *   constraint_id: tractarian_logic_limit
 *   human_readable: The Limits of Language (Tractatus)
 *   domain: philosophical/logical
 *
 * SUMMARY:
 *   The Tractarian logic limit posits that the world consists of facts in
 *   logical space, and language can only picture these facts. What cannot be
 *   pictured—the logical form itself, ethics, aesthetics, metaphysics—lies
 *   beyond the limits of language and can only be shown, not said. This
 *   limit, within the Tractarian system, is considered a fundamental
 *   constraint on meaningful discourse.
 *
 * KEY AGENTS:
 *   - The Unspeakable: That which lies beyond the limits of language.
 *   - The Analytical Observer: One who understands the system's constraints.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tractarian_logic_limit, 0.05).
domain_priors:suppression_score(tractarian_logic_limit, 0.01).
domain_priors:theater_ratio(tractarian_logic_limit, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tractarian_logic_limit, extractiveness, 0.05).
narrative_ontology:constraint_metric(tractarian_logic_limit, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(tractarian_logic_limit, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tractarian_logic_limit, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(tractarian_logic_limit, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tractarian_logic_limit, mountain).
narrative_ontology:human_readable(tractarian_logic_limit, "The Limits of Language (Tractatus)").
narrative_ontology:topic_domain(tractarian_logic_limit, "philosophical/logical").

domain_priors:emerges_naturally(tractarian_logic_limit).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of what cannot be said but only shown, the limits are absolute and unavoidable.
constraint_indexing:constraint_classification(tractarian_logic_limit, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% An analytical observer understands the Tractarian system as a fixed, logical constraint. Language is limited by its structure, and this limitation is inherent to its nature.
constraint_indexing:constraint_classification(tractarian_logic_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tractarian_logic_limit_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(tractarian_logic_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tractarian_logic_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tractarian_logic_limit, ExtMetricName, E),
    domain_priors:suppression_score(tractarian_logic_limit, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tractarian_logic_limit),
    narrative_ontology:constraint_metric(tractarian_logic_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tractarian_logic_limit, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tractarian_logic_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and Suppression are low because the constraint is seen as a fundamental limit rather than an actively enforced restriction. The system is based on a concept that does not create victims or beneficiaries. It is primarily about what can be expressed logically.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap because both perspectives see the limit as an inherent feature of logic and language. What cannot be expressed, cannot be expressed from any perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The absence of clear beneficiaries and victims in the Tractarian framework leads to a natural emergence where no directionality or enforcement can be identified.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint naturally resolves the problem of mischaracterizing coordination as extraction. It states that not everything can be expressed through language, and this limitation is inherent to its structure, rather than being imposed by any external factor. The system is a fundamental principle, not a manufactured social or political constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tractarian_logic_limit, 0, 1).

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
