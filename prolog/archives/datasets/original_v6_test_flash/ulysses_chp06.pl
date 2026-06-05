% ============================================================================
% CONSTRAINT STORY: ulysses_chp06
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp06, []).

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
 *   constraint_id: ulysses_chp06
 *   human_readable: The Funerary Mountain (Prospect Cemetery)
 *   domain: social/religious/technological
 *
 * SUMMARY:
 *   Leopold Bloom attends the funeral of Paddy Dignam, navigating the
 *   communal Ropes of mourning while confronting the absolute Mountain of
 *   death. The cemetery, as a physical and social space, embodies the tension
 *   between individual grief and collective ritual.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Bereaved individual (moderate/constrained)
 *   - Paddy Dignam: The deceased (powerless/trapped)
 *   - Mourning community: Collective of mourners (moderate/mobile)
 *   - Narrator/Reader: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp06, 0.15).
domain_priors:suppression_score(ulysses_chp06, 0.02).
domain_priors:theater_ratio(ulysses_chp06, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp06, extractiveness, 0.15).
narrative_ontology:constraint_metric(ulysses_chp06, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ulysses_chp06, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ulysses_chp06, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ulysses_chp06, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp06, mountain).
narrative_ontology:human_readable(ulysses_chp06, "The Funerary Mountain (Prospect Cemetery)").
narrative_ontology:topic_domain(ulysses_chp06, "social/religious/technological").

domain_priors:emerges_naturally(ulysses_chp06).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of the deceased, death is an absolute, inescapable mountain.
constraint_indexing:constraint_classification(ulysses_chp06, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% For the mourning community, the inevitability of death is a fixed point, a mountain in the landscape of life.
constraint_indexing:constraint_classification(ulysses_chp06, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp06_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ulysses_chp06, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp06, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ulysses_chp06, ExtMetricName, E),
    domain_priors:suppression_score(ulysses_chp06, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ulysses_chp06),
    narrative_ontology:constraint_metric(ulysses_chp06, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ulysses_chp06, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ulysses_chp06_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because death is perceived as a natural phenomenon rather than an exploitative force. Suppression is also low as alternative beliefs or coping mechanisms are not actively suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives converge on a mountain classification due to the fundamental nature of mortality. While individual experiences of grief may vary, the ultimate fact of death remains a fixed point.
 *
 * DIRECTIONALITY LOGIC:
 *   Death is a universal constant, a mountain from all perspectives. Beneficiaries are not applicable in this scenario, as death is not something that provides a benefit. The victims, or affected parties, are the mourners, who experience grief and loss. The power levels are relative to the constraint of death; Bloom is moderate as he is affected by it but has agency, while the deceased is powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a mountain prevents mislabeling coordination, as death itself is not a coordination mechanism. The funerary rituals may involve coordination, but the underlying reality of death is a fixed constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp06, 1904, 1904).

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
