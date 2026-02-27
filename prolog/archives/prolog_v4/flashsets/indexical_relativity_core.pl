% ============================================================================
% CONSTRAINT STORY: indexical_relativity_core
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indexical_relativity_core, []).

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
 *   constraint_id: indexical_relativity_core
 *   human_readable: The Law of Indexical Relativity
 *   domain: epistemological
 *
 * SUMMARY:
 *   This constraint establishes that the classification of any social or
 *   physical limitation is not an intrinsic property of the object itself,
 *   but instead depends entirely on the indexical context from which it is
 *   observed. The classification of a limitation (as Mountain, Rope, etc.) is
 *   relative to the observer's power, time horizon, exit options, and spatial
 *   scope. This is a fundamental principle, akin to a law of physics in
 *   epistemology.
 *
 * KEY AGENTS:
 *   - Any agent (powerless/trapped): Experiences limitations as relative, not absolute
 *   - Any institution (institutional/arbitrage): Classifies constraints based on its own structural position
 *   - Analytical observer (analytical/analytical): Sees the observer-relativity of constraint classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indexical_relativity_core, 0.15).
domain_priors:suppression_score(indexical_relativity_core, 0.03).
domain_priors:theater_ratio(indexical_relativity_core, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indexical_relativity_core, extractiveness, 0.15).
narrative_ontology:constraint_metric(indexical_relativity_core, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(indexical_relativity_core, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(indexical_relativity_core, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(indexical_relativity_core, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indexical_relativity_core, mountain).
narrative_ontology:human_readable(indexical_relativity_core, "The Law of Indexical Relativity").
narrative_ontology:topic_domain(indexical_relativity_core, "epistemological").

domain_priors:emerges_naturally(indexical_relativity_core).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a powerless agent, the law of indexical relativity demonstrates that their perceived limitations are contingent and context-dependent, a fixed epistemological feature.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From an institutional perspective, the law highlights that classifications of constraints are not inherent but rather shaped by the power dynamics and vantage points from which they are observed.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% An analytical observer recognizes that constraint classifications are observer-relative, not intrinsic properties. This is an irreducible epistemological principle.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indexical_relativity_core_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(indexical_relativity_core, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indexical_relativity_core, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(indexical_relativity_core, ExtMetricName, E),
    domain_priors:suppression_score(indexical_relativity_core, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(indexical_relativity_core),
    narrative_ontology:constraint_metric(indexical_relativity_core, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(indexical_relativity_core, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(indexical_relativity_core_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Mountain because the relativity of indexicality is itself an immutable principle in epistemology. Extractiveness and suppression are minimal because the law itself is not coercive but rather descriptive. The theater ratio is also low because the validity of the law does not depend on performative displays.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap because all observers, regardless of their structural position, should recognize the observer-relativity of constraint classification. Any apparent differences in classification stem from incomplete information or misapplication of the principle.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'law' does not directly benefit or harm any specific group, it just explains how classifications are made. Any actor that claims to be harmed is failing to acknowledge the dependence of constraint classifications on observer context.
 *
 * MANDATROPHY ANALYSIS:
 *   The law prevents mislabeling constraints by highlighting the dependence on observer context. Any attempt to claim a constraint is a 'pure snare' or a 'true mountain' is a category error -- the classification is relative. The analytical task is to identify the conditions that create the experience of a snare or mountain, not to claim it as an inherent property.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indexical_relativity_core, 0, 100).

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
