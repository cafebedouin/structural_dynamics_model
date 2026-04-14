% ============================================================================
% CONSTRAINT STORY: right_angle_construction_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_right_angle_construction_constraint, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: right_angle_construction_constraint
 *   human_readable: Right Angle Construction via Compass and Straightedge
 *   domain: geometry/mathematics
 *
 * SUMMARY:
 *   Right angle construction via compass and straightedge is a foundational
 *   constraint in Euclidean geometry. Given any line segment or pair of
 *   lines, an agent can always construct a perpendicular (90-degree angle)
 *   using classical geometric tools. This constraint exhibits the defining
 *   properties of a Mountain: it emerges from first principles of geometry,
 *   imposes no extraction upon any agent, and admits no loopholes or
 *   alternative pathways. The construction method has been known since
 *   ancient times and has never required institutional enforcement or
 *   suppression. No group benefits while another group bears costs. The
 *   constraint is invariant across all observational contexts and cultural
 *   frameworks — perpendicularity is a mathematical fact independent of human
 *   preference or institutional arrangement.
 *
 * KEY AGENTS:
 *   - Any Geometer: Structurally neutral (powerless/trapped in geometric space) — must accept the perpendicularity property as immutable
 *   - Mathematical Community: Institutional observer (institutional/mobile) — documents the constraint as proven fact; makes no extractive claims
 *   - Educational Systems: Institutional transmitter (institutional/mobile) — teaches the constraint as objective knowledge; no asymmetric extraction
 *   - Analytical Observer: Universal perspective (analytical/analytical) — recognizes the constraint as logical necessity, not contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(right_angle_construction_constraint, 0.08).
domain_priors:suppression_score(right_angle_construction_constraint, 0.02).
domain_priors:theater_ratio(right_angle_construction_constraint, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(right_angle_construction_constraint, extractiveness, 0.08).
narrative_ontology:constraint_metric(right_angle_construction_constraint, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(right_angle_construction_constraint, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(right_angle_construction_constraint, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(right_angle_construction_constraint, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(right_angle_construction_constraint, mountain).
narrative_ontology:human_readable(right_angle_construction_constraint, "Right Angle Construction via Compass and Straightedge").
narrative_ontology:topic_domain(right_angle_construction_constraint, "geometry/mathematics").

domain_priors:emerges_naturally(right_angle_construction_constraint).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GEOMETER UNDER CONSTRAINT (MOUNTAIN) — An agent tasked with constructing a right angle has no choice but to operate within the geometric laws that define perpendicularity. No material barriers, coercion, or enforcement needed. The constraint simply is. Accessibility collapse is absolute: there is no accessible alternative to the mathematical structure of right angles.
constraint_indexing:constraint_classification(right_angle_construction_constraint, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From the standpoint of universal logical and mathematical truth, right angle construction is constrained by irreducible properties of Euclidean geometry. The perpendicularity relation follows from first principles. No observational ambiguity, no institutional wrapping, no extractive mechanism. Pure natural law.
constraint_indexing:constraint_classification(right_angle_construction_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICS EDUCATION (MOUNTAIN) — Educational institutions teach right angle construction as a mathematical fact, not an institutional arrangement. Students may find it difficult or unintuitive, but no amount of social reorganization changes the underlying constraint. The perpendicularity property is invariant across all human institutions.
constraint_indexing:constraint_classification(right_angle_construction_constraint, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(right_angle_construction_constraint_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(right_angle_construction_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(right_angle_construction_constraint, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(right_angle_construction_constraint, ExtMetricName, E),
    domain_priors:suppression_score(right_angle_construction_constraint, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(right_angle_construction_constraint),
    narrative_ontology:constraint_metric(right_angle_construction_constraint, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(right_angle_construction_constraint, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(right_angle_construction_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. No agent extracts value from the constraint at the expense of another. The constraint does not transfer resources, suppress alternatives, or concentrate benefits. All geometers have equal access to the construction method. Suppression (0.02): Negligible. There are no barriers to learning or using right angle construction. The method is taught openly, requires only basic tools, and has no licensing or gatekeeping. Theater ratio (0.05): Minimal performative content. The constraint's function is identical to its form — the mathematical proof is the entire story; there is no gap between what right angle construction claims to do and what it actually does. Accessibility collapse (0.92): Catastrophic for alternatives. There is no accessible way to construct a perpendicular line other than through the geometric principles that define perpendicularity itself. Any construction that appears to bypass geometry would, upon analysis, be found to implement the same perpendicular principle.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap across all observation points. All perspectives classify as Mountain because the constraint's mathematical structure is invariant to observer position. A powerless agent and an institutional agent experience identical perpendicularity when attempting to construct a right angle. The analytical observer sees no hidden extraction mechanism to unmask. This uniformity is the defining feature of natural law constraints — they are perspective-invariant because they encode logical necessity rather than institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation is applicable. The constraint has no beneficiaries and no victims because it performs no extraction. The mathematical property of perpendicularity does not concentrate power, suppress alternatives, or create asymmetric access. All agents occupy equivalent positions relative to the constraint — they are all equally subject to its logical necessity and all equally able to use its methods. The absence of beneficiary/victim structure is not a data gap but a defining feature of the Mountain type.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy trivially: it is pure coordination (Rope from the perspective of resource-efficient knowledge transmission) only if one ignores its mathematical content and views it as a cultural practice. When analyzed at the mathematical level, it is pure natural law (Mountain). No institutional actor extracts value from teaching perpendicularity; no suppression maintains the constraint; no beneficiary class exists. The mandatrophy is resolved by recognizing that the constraint's essence is mathematical, not institutional. Educational framing does not transform a mathematical law into an extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    euclidean_vs_non_euclidean,
    'Is the right angle constraint specific to Euclidean geometry, or does it generalize to non-Euclidean spaces?',
    'Formal analysis of perpendicularity definitions in Riemannian and hyperbolic geometries; comparison of construct methods across geometric systems',
    'If Euclidean-only: the constraint depends on the axiom system chosen. If generalizable: the constraint is deeper than any single geometric framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(euclidean_vs_non_euclidean, conceptual, 'Whether right angle constraint is specific to Euclidean geometry').

omega_variable(
    physical_realizability,
    'To what extent do physical constraints in real-world construction (precision limits, material properties) map to the mathematical constraint, versus represent independent physical limitations?',
    'Analysis of measurement precision in physical right angle constructions; comparison of mathematical ideal vs physical achievable angles',
    'If largely independent: the mathematical constraint and physical constraint are separate stories. If tightly coupled: they may be aspects of a single constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_realizability, empirical, 'Relationship between mathematical and physical realizability of right angles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(right_angle_construction_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rac_tr_t0, right_angle_construction_constraint, theater_ratio, 0, 0.03).
narrative_ontology:measurement(rac_tr_t5, right_angle_construction_constraint, theater_ratio, 5, 0.04).
narrative_ontology:measurement(rac_tr_t10, right_angle_construction_constraint, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(rac_be_t0, right_angle_construction_constraint, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(rac_be_t5, right_angle_construction_constraint, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(rac_be_t10, right_angle_construction_constraint, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(right_angle_construction_constraint, information_standard).

% DUAL FORMULATION NOTE:
% Right angle construction is a standalone constraint with no structural dependencies on other constraints. It does not depend on institutional arrangements or social contingencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
