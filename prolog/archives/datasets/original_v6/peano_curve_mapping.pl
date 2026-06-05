% ============================================================================
% CONSTRAINT STORY: peano_curve_mapping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peano_curve_mapping, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: peano_curve_mapping
 *   human_readable: Peano Space-Filling Curve
 *   domain: mathematical/theoretical
 *
 * SUMMARY:
 *   The Peano Curve is a mathematical limit case: a continuous function
 *   f:[0,1]→[0,1]² that is surjective (onto) but not injective (not
 *   one-to-one). Discovered by Giuseppe Peano in 1890, it resolved a
 *   conceptual crisis in topology by demonstrating that 'continuous' does not
 *   imply 'dimension-preserving.' The constraint classifies as Mountain from
 *   all perspectives because its existence follows from logical principles
 *   and cannot be negotiated, suppressed, or extracted value from. No agent
 *   benefits by hiding the Peano Curve; no agent can escape it through
 *   institutional innovation or resource investment. The constraint is
 *   invariant across all observables: whether viewed through pure topology,
 *   computational geometry, or pedagogy, the dimensional collapse is
 *   immutable. Unlike constraints that blend coordination and extraction
 *   (Tangled Rope) or that depend on measurement basis (Piton), the Peano
 *   Curve is a prototype of an irreducible structural fact.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Analytical stakeholder — observes and articulates the constraint; cannot negotiate its existence
 *   - Topological Foundation: The abstract system itself — defines the constraint through definitional necessity
 *   - Computational Engineers: Practical agents (powerful/analytical) — encounter the constraint when building spatial indexing systems; cannot circumvent it regardless of resources
 *   - Students and Learners: Biographical agents (moderate/analytical) — experience accessibility collapse when intuitions about dimension are violated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peano_curve_mapping, 0.08).
domain_priors:suppression_score(peano_curve_mapping, 0.02).
domain_priors:theater_ratio(peano_curve_mapping, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peano_curve_mapping, extractiveness, 0.08).
narrative_ontology:constraint_metric(peano_curve_mapping, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(peano_curve_mapping, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(peano_curve_mapping, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(peano_curve_mapping, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peano_curve_mapping, mountain).
narrative_ontology:human_readable(peano_curve_mapping, "Peano Space-Filling Curve").
narrative_ontology:topic_domain(peano_curve_mapping, "mathematical/theoretical").

domain_priors:emerges_naturally(peano_curve_mapping).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL COMMUNITY (MOUNTAIN) — The existence of a continuous surjection from [0,1] onto [0,1]² is a logical necessity, not contingent on institutional arrangement or convention. No agent can extract value by denying or suppressing the mapping; no agent benefits from its existence. The constraint is purely structural — it defines the boundary of what continuous functions can accomplish.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: TOPOLOGICAL FOUNDATION (MOUNTAIN) — From the perspective of topology itself, the Peano Curve exemplifies an invariant: continuous maps preserve connectedness but not dimension. This is not something anyone 'enforces'—it follows from the definition of continuity and connectedness. Every continuous surjection from 1D to 2D exhibits the same dimensional collapse. Zero degrees of freedom.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: COMPUTATIONAL/ENGINEERING PERSPECTIVE (MOUNTAIN) — Even agents seeking to exploit or circumvent space-filling curves for data compression or spatial indexing encounter the same constraint: no continuous bijection from [0,1] onto [0,1]² exists. Engineers building databases (Z-order curves, Hilbert curves) must accept the trade-off between continuity and injectivity. The constraint is inescapable regardless of power or resources.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: PEDAGOGICAL PERSPECTIVE (MOUNTAIN) — From the student's view, the Peano Curve is an accessibility collapse: it violates intuition about dimension and injectivity, forcing fundamental revision of conceptual understanding. The resistance to accepting the result is high initially (does this really mean a 1D interval can map onto a 2D square?), but the proof is airtight. No alternative framing makes the constraint disappear.
constraint_indexing:constraint_classification(peano_curve_mapping, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peano_curve_mapping_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(peano_curve_mapping, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peano_curve_mapping, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(peano_curve_mapping, ExtMetricName, E),
    domain_priors:suppression_score(peano_curve_mapping, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(peano_curve_mapping),
    narrative_ontology:constraint_metric(peano_curve_mapping, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(peano_curve_mapping, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(peano_curve_mapping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The Peano Curve generates no asymmetric value extraction. No agent captures rents or benefits from asymmetric information. The mathematical fact is public, stable, and equally available to all. Even computational engineers cannot exploit it as an extraction mechanism—they must work within its constraints, not benefit from suppressing alternatives. Suppression (0.02): Negligible. The result is mathematically proven; no coercive power is required to maintain it. It cannot be forgotten because it is rediscovered independently by each mathematician learning topology. Theater ratio (0.15): Low. Pedagogical exposition of the Peano Curve involves some performative elements (drawing approximations, illustrating the limit process), but the underlying claim is purely structural, not theatrical. The proof is complete and transparent.
 *
 * PERSPECTIVAL GAP:
 *   The Peano Curve exhibits zero perspectival gap—all four analytical contexts (mathematics, topology, engineering, pedagogy) classify it identically as Mountain. This uniformity is itself diagnostic. When a constraint produces the same classification from powerless, moderate, powerful, organized, institutional, and analytical perspectives, the constraint is a natural law. The absence of divergent perspectives confirms that no extraction mechanism exists. The student's high accessibility_collapse (difficulty of intuitive understanding) is not extraction—it is cognitive effort required to align understanding with mathematical reality, not institutional coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is undefined for the Peano Curve in the standard sense because there are no beneficiaries or victims. The constraint is not asymmetric in who it advantages or disadvantages. The sigmoid function f(d) is not applied because the constraint does not involve extraction. The chi formula χ = ε × f(d) × σ(S) reduces to zero extraction regardless of f(d) or σ(S) because ε=0.08 and no agent directs the constraint toward or away from themselves. The constraint is invariant across all (P,T,E,S) tuples.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    construction_versus_existence,
    'Does the abstract mathematical existence of the Peano Curve differ structurally from its explicit recursive construction?',
    'Formal comparison of non-constructive existence proofs (topology via Brouwer''s theorem) versus explicit iterative construction (Peano''s 1890 definition). Analysis of whether both produce the same constraint or represent distinct claims.',
    'If different: two constraint stories needed (abstract existence vs constructibility). If same: confirms mountain classification is robust across proof methods.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(construction_versus_existence, conceptual, 'Distinction between abstract existence and explicit construction of the curve').

omega_variable(
    dimension_preservation_across_measures,
    'Does the Peano Curve''s dimensional behavior remain invariant under non-Euclidean metrics or alternative topologies?',
    'Mathematical analysis of space-filling curves in metric spaces with different dimension definitions (Hausdorff, box dimension, topological dimension). Testing whether the constraint holds in non-standard geometries.',
    'If invariant: confirms universal scope. If metric-dependent: constraint may require refinement to ''Peano Curve in Euclidean space'' and decompose into separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dimension_preservation_across_measures, empirical, 'Whether dimensional collapse holds across alternative topologies').

omega_variable(
    computational_approximation_limits,
    'Do discrete approximations to the Peano Curve (finite-resolution grids) asymptotically approach the continuous limit, or do they exhibit systematic errors?',
    'Numerical analysis of discretized space-filling curves; measurement of coverage gaps, self-intersection rates, and convergence to the continuous limit under resolution refinement.',
    'If approaches limit: discrete implementations are faithful approximations (Rope classification for engineering). If systematic errors persist: discrete space-filling curves are a distinct constraint with partial extraction hidden in approximation (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_approximation_limits, empirical, 'Convergence properties of discrete approximations to continuous Peano Curve').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peano_curve_mapping, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peano_tr_t0, peano_curve_mapping, theater_ratio, 0, 0.1).
narrative_ontology:measurement(peano_tr_t50, peano_curve_mapping, theater_ratio, 50, 0.14).
narrative_ontology:measurement(peano_tr_t100, peano_curve_mapping, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(peano_be_t0, peano_curve_mapping, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(peano_be_t50, peano_curve_mapping, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(peano_be_t100, peano_curve_mapping, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peano_curve_mapping, information_standard).
narrative_ontology:affects_constraint(peano_curve_mapping, space_filling_curve_family).
narrative_ontology:affects_constraint(peano_curve_mapping, dimension_preservation_in_continuous_maps).
narrative_ontology:affects_constraint(peano_curve_mapping, topological_invariant_universality).

% DUAL FORMULATION NOTE:
% The Peano Curve is upstream in a constraint family. Downstream constraints include discrete space-filling approximations (which may introduce extraction through discretization trade-offs) and applications to computational geometry (which may exhibit partial extraction in spatial indexing schemes). Each downstream constraint has higher extractiveness than the abstract mathematical principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
