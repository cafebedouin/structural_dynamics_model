% ============================================================================
% CONSTRAINT STORY: geophysics_superionic_core
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geophysics_superionic_core, []).

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
 *   constraint_id: geophysics_superionic_core
 *   human_readable: Super-ionic state of matter in Earth's inner core
 *   domain: scientific/physical
 *
 * SUMMARY:
 *   Based on advanced seismic wave analysis, geophysicists have discovered
 *   that Earth's inner core exists in a 'super-ionic' state. This state is
 *   characterized by hydrogen ions moving freely within a solid iron lattice.
 *   This discovery has implications for our understanding of Earth's magnetic
 *   field and the planet's overall thermal history.
 *
 * KEY AGENTS:
 *   - Earth's Inner Core: Exhibiting the super-ionic state.
 *   - Geophysicists: Analytical observers studying this phenomenon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geophysics_superionic_core, 0.15).
domain_priors:suppression_score(geophysics_superionic_core, 0.02).
domain_priors:theater_ratio(geophysics_superionic_core, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geophysics_superionic_core, extractiveness, 0.15).
narrative_ontology:constraint_metric(geophysics_superionic_core, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(geophysics_superionic_core, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geophysics_superionic_core, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(geophysics_superionic_core, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geophysics_superionic_core, mountain).
narrative_ontology:human_readable(geophysics_superionic_core, "Super-ionic state of matter in Earth's inner core").
narrative_ontology:topic_domain(geophysics_superionic_core, "scientific/physical").

domain_priors:emerges_naturally(geophysics_superionic_core).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical and universal perspective, the super-ionic state of matter in Earth's core is a physical property that arises from the fundamental laws of physics and material science under extreme pressure and temperature conditions.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The geophysics community, using seismological data and computational models, recognizes the super-ionic state as a fundamental characteristic of the Earth's inner core. Research is aimed at understanding how it influences the planet's dynamics.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geophysics_superionic_core_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(geophysics_superionic_core, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geophysics_superionic_core, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(geophysics_superionic_core, ExtMetricName, E),
    domain_priors:suppression_score(geophysics_superionic_core, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(geophysics_superionic_core),
    narrative_ontology:constraint_metric(geophysics_superionic_core, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(geophysics_superionic_core, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(geophysics_superionic_core_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low because the super-ionic state is an intrinsic physical property, not an exploitable resource or condition. Suppression is also low, as there's no known mechanism preventing observation or study. Theater ratio is low as well, due to the relatively direct observation via seismic waves and modeling.
 *
 * PERSPECTIVAL GAP:
 *   There's minimal perspectival gap because the super-ionic state is a physical characteristic that is objectively measurable, leading to a Mountain classification from multiple perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable here, as it's a physical property and not a constraint involving beneficiaries or victims. It's a natural phenomenon under study.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geophysics_superionic_core, 0, 100).

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
