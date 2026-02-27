% ============================================================================
% CONSTRAINT STORY: deep_earth_hydrogen_availability
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deep_earth_hydrogen_availability, []).

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
 *   constraint_id: deep_earth_hydrogen_availability
 *   human_readable: Deep Earth Hydrogen Availability Limit
 *   domain: technological
 *
 * SUMMARY:
 *   The amount of hydrogen available in the Earth's core and mantle is
 *   limited by geophysical and geochemical processes. This constraint is
 *   classified as a mountain because the available hydrogen is ultimately
 *   determined by the Earth's formation and subsequent evolution,
 *   representing a fundamental limitation on potential hydrogen resources.
 *
 * KEY AGENTS:
 *   - The Earth's Core: The physical system that defines the constraint.
 *   - Geophysical Research Community: Studies the hydrogen availability.
 *   - Analytical Observer: Civilizational view.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deep_earth_hydrogen_availability, 0.1).
domain_priors:suppression_score(deep_earth_hydrogen_availability, 0.05).
domain_priors:theater_ratio(deep_earth_hydrogen_availability, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, extractiveness, 0.1).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deep_earth_hydrogen_availability, mountain).
narrative_ontology:human_readable(deep_earth_hydrogen_availability, "Deep Earth Hydrogen Availability Limit").
narrative_ontology:topic_domain(deep_earth_hydrogen_availability, "technological").

domain_priors:emerges_naturally(deep_earth_hydrogen_availability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Earth's core - Hydrogen availability is limited by the physical and chemical processes within the core, presenting an immutable constraint.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Perspective 2: Geophysical Research Community - Academic and research institutions see the hydrogen availability as a fixed constraint imposed by the planet's geology.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective 3: Analytical Observer - The total amount of hydrogen is limited by the Earth's initial formation conditions and subsequent chemical and physical evolution.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deep_earth_hydrogen_availability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, ExtMetricName, E),
    domain_priors:suppression_score(deep_earth_hydrogen_availability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(deep_earth_hydrogen_availability),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(deep_earth_hydrogen_availability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low as the natural limit on deep earth hydrogen availability is a non-extractive property of the system. Suppression is low as there's no enforcement necessary. The constraint emerges naturally as a consequence of the Earth's formation and geological processes.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify the constraint as a mountain, indicating a shared understanding of it as a fixed physical limitation.
 *
 * DIRECTIONALITY LOGIC:
 *   The hydrogen availability is a physical constraint with no direct beneficiaries or victims. The research community studies the constraint as an observer.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deep_earth_hydrogen_availability, 0, 1000000000).

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
