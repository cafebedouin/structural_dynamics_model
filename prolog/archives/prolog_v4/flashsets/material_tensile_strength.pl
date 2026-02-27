% ============================================================================
% CONSTRAINT STORY: material_tensile_strength
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_material_tensile_strength, []).

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
 *   constraint_id: material_tensile_strength
 *   human_readable: Ultimate Tensile Strength (UTS)
 *   domain: technological
 *
 * SUMMARY:
 *   Tensile strength is the maximum stress that a material can withstand
 *   while being stretched or pulled before breaking. It is a fundamental
 *   material property dictated by intermolecular bonding and defects. As
 *   such, from all perspectives, it represents a natural limit, a mountain
 *   constraint that engineering must respect.
 *
 * KEY AGENTS:
 *   - Engineers: (moderate/constrained) Design within the limits of tensile strength.
 *   - Material Scientists: (analytical/analytical) Seek to understand and improve tensile strength.
 *   - End Users: (powerless/trapped) Rely on products that adhere to tensile strength limits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(material_tensile_strength, 0.05).
domain_priors:suppression_score(material_tensile_strength, 0.01).
domain_priors:theater_ratio(material_tensile_strength, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(material_tensile_strength, extractiveness, 0.05).
narrative_ontology:constraint_metric(material_tensile_strength, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(material_tensile_strength, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(material_tensile_strength, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(material_tensile_strength, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(material_tensile_strength, mountain).
narrative_ontology:human_readable(material_tensile_strength, "Ultimate Tensile Strength (UTS)").
narrative_ontology:topic_domain(material_tensile_strength, "technological").

domain_priors:emerges_naturally(material_tensile_strength).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The ultimate tensile strength is a material property that is a physical limit. There is no escaping this constraint.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Material science institutions must adhere to tensile strength limits as they design new materials.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% UTS is an inherent material property; models must conform to observed UTS values.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(material_tensile_strength_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(material_tensile_strength, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(material_tensile_strength, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(material_tensile_strength, ExtMetricName, E),
    domain_priors:suppression_score(material_tensile_strength, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(material_tensile_strength),
    narrative_ontology:constraint_metric(material_tensile_strength, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(material_tensile_strength, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(material_tensile_strength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Very Low. The constraint is a natural limit rather than an extractive mechanism.  Suppression (0.01): Very Low. Attempts to circumvent tensile strength limitations are futile due to the immutable laws of physics.
 *
 * PERSPECTIVAL GAP:
 *   There is no real perspectival gap, as UTS is a physical constraint, so any perspective will yield a 'mountain' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is fundamentally a physical limit. Material scientists and engineers must respect this limit.
 *
 * MANDATROPHY ANALYSIS:
 *   Since tensile strength is correctly categorized as a mountain constraint across all perspectives, the risk of misidentifying it as a snare or tangled rope is negligible. Mandatrophy is prevented as the classification accurately reflects the true nature of the constraint as an immutable physical property.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(material_tensile_strength, 0, 100).

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
