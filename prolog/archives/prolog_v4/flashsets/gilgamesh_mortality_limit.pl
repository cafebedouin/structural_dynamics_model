% ============================================================================
% CONSTRAINT STORY: gilgamesh_mortality_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gilgamesh_mortality_limit, []).

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
 *   constraint_id: gilgamesh_mortality_limit
 *   human_readable: The Allotment of Mortality
 *   domain: philosophical/religious
 *
 * SUMMARY:
 *   This constraint models mortality as the ultimate physical limit as
 *   highlighted in the Epic of Gilgamesh. Mortality is an inescapable fact of
 *   human existence, representing a fundamental limitation on individual
 *   potential and experience. The inevitability of death shapes human values,
 *   motivations, and the search for meaning.
 *
 * KEY AGENTS:
 *   - Mortal Individual: (powerless/trapped) - Experiences the constraint directly as the limit of life.
 *   - Religious Institutions: (institutional/analytical) - Provide frameworks for understanding and coping with mortality.
 *   - Analytical Observer: (analytical/analytical) - Studies mortality as a scientific and philosophical phenomenon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gilgamesh_mortality_limit, 0.05).
domain_priors:suppression_score(gilgamesh_mortality_limit, 0.01).
domain_priors:theater_ratio(gilgamesh_mortality_limit, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, extractiveness, 0.05).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gilgamesh_mortality_limit, mountain).
narrative_ontology:human_readable(gilgamesh_mortality_limit, "The Allotment of Mortality").
narrative_ontology:topic_domain(gilgamesh_mortality_limit, "philosophical/religious").

domain_priors:emerges_naturally(gilgamesh_mortality_limit).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a mortal individual, death is an inescapable reality, a fixed limit to their existence. Trapped in mortality with no exit.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% Religious institutions, over long timescales, acknowledge mortality as a fundamental aspect of the human condition, even as they may offer spiritual pathways to transcend it. Analytical view - interpreting the natural world.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From an analytical perspective, the limit of mortality is a fixed, universal constraint based on the current understanding of biology and physics.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gilgamesh_mortality_limit_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, ExtMetricName, E),
    domain_priors:suppression_score(gilgamesh_mortality_limit, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gilgamesh_mortality_limit),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gilgamesh_mortality_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Mortality, for the purposes of this model, is treated as a Mountain. Extractiveness is minimal (0.05) as mortality does not actively extract from individuals but rather represents a boundary. Suppression is also minimal (0.01) as there is no active force suppressing alternatives, though alternatives are not viable. The theater ratio is low (0.01) as mortality is a biological reality, not a performance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives all converge on a mountain classification, reflecting the universal and inescapable nature of mortality, although the individual experiences it most directly. The religious perspective provides coping mechanisms while acknowledging the reality of mortality. The analytical perspective sees it as a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared because this is a Mountain and all agents experience the limit without a clear extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a result of misinterpreting coordination as pure extraction. Instead, it is a fundamental limit that transcends any specific societal structure or individual power dynamic. It is classified as a Mountain precisely because it lacks the characteristics of extraction; no agent actively benefits from or enforces it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gilgamesh_mortality_limit, 0, 100).

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
