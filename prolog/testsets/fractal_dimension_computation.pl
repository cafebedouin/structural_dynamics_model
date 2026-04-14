% ============================================================================
% CONSTRAINT STORY: fractal_dimension_computation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fractal_dimension_computation, []).

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
 *   constraint_id: fractal_dimension_computation
 *   human_readable: Fractal Dimension Computation Limit
 *   domain: mathematics/computational_geometry
 *
 * SUMMARY:
 *   Fractal dimension computation represents a mathematical limit intrinsic
 *   to the definition of fractals themselves. A fractal's dimension is
 *   defined as the value d such that the object exhibits self-similarity
 *   across scales — finer detail repeats the coarser structure. Computing
 *   this dimension requires sampling the object at multiple resolutions and
 *   measuring how the measure (typically count or mass) scales with
 *   resolution. The fundamental constraint is that any finite algorithm can
 *   only sample a finite number of scales, yet the fractal definition is
 *   inherently infinite. This is not a policy choice, institutional
 *   arrangement, or extractive mechanism — it is a consequence of the
 *   mathematical object's definition. The constraint is invariant across all
 *   observers, all time horizons, and all resource levels.
 *
 * KEY AGENTS:
 *   - Computational Agent (powerless/trapped) — any system attempting to compute the dimension faces the resolution limit
 *   - Well-Resourced Researcher (powerful/trapped) — even with unlimited resources, the fundamental mathematical limit remains
 *   - Mathematical Observer (analytical/analytical) — sees the constraint as a pure logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fractal_dimension_computation, 0.12).
domain_priors:suppression_score(fractal_dimension_computation, 0.03).
domain_priors:theater_ratio(fractal_dimension_computation, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fractal_dimension_computation, extractiveness, 0.12).
narrative_ontology:constraint_metric(fractal_dimension_computation, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fractal_dimension_computation, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fractal_dimension_computation, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fractal_dimension_computation, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fractal_dimension_computation, mountain).
narrative_ontology:human_readable(fractal_dimension_computation, "Fractal Dimension Computation Limit").
narrative_ontology:topic_domain(fractal_dimension_computation, "mathematics/computational_geometry").

domain_priors:emerges_naturally(fractal_dimension_computation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL AGENT (MOUNTAIN) — Any algorithm computing fractal dimension must confront the box-counting limit: the resolution achievable is bounded by available memory and processing time. This is not a policy choice or institutional arrangement; it is a mathematical-physical limit. No exit from this constraint.
constraint_indexing:constraint_classification(fractal_dimension_computation, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: WELL-RESOURCED RESEARCHER (MOUNTAIN) — Even with unlimited computational resources, the fundamental limit on distinguishing self-similarity across scales remains: fractal dimension emerges from the relationship between structure and scale, and computing it requires sampling that relationship across multiple scales. The constraint is invariant to resources.
constraint_indexing:constraint_classification(fractal_dimension_computation, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL OBSERVER (MOUNTAIN) — From the analytical perspective, fractal dimension computation derives from the Hausdorff dimension limit: the set being measured has an intrinsic dimension value, and finite algorithms can only approximate it. The approximation error is fundamental, not institutional. All perspectives converge on the same classification.
constraint_indexing:constraint_classification(fractal_dimension_computation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fractal_dimension_computation_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fractal_dimension_computation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fractal_dimension_computation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fractal_dimension_computation, ExtMetricName, E),
    domain_priors:suppression_score(fractal_dimension_computation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fractal_dimension_computation),
    narrative_ontology:constraint_metric(fractal_dimension_computation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fractal_dimension_computation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fractal_dimension_computation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. No extraction occurs from one agent to another. The constraint is not about redistribution or taking resources from one party and giving to another. The 0.12 value reflects the minimal measurement noise and approximation error inherent to any finite computation, not actual extraction. Suppression (0.03): Minimal. There is no coercion mechanism. The constraint is transparent — everyone understands the limit. Agents can choose to compute fractal dimension or not; there is no force preventing exit from the problem. Theater ratio (0.08): Negligible. The constraint has no performative content. Either the algorithm converges on a dimension value or it doesn't. The mathematics is objectively verifiable.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All six perspective positions (powerless/powerful/analytical × trapped/analytical) yield mountain classification. The constraint is invariant under all observational contexts. This is the signature of a true natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality applies because there are no beneficiaries or victims. The constraint is not redistributive. All observers experience the same mathematical limit. The d-value would be 0.5 (symmetric) or undefined (not applicable), but since there is no extraction mechanism, the power and exit parameters do not drive classification. The mountain classification derives purely from the constraint's mathematical nature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy vacuously: there is no mixed coordination-extraction to resolve. The constraint is pure mathematical definition, not a social or institutional arrangement. All perspectives converge. No false summit risk because the underlying structure is transparent and universal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    computational_vs_theoretical_dimension,
    'Is the constraint the mathematical definition of Hausdorff dimension (theoretical limit) or the practical box-counting algorithm (computational limit)?',
    'Clarify whether the constraint concerns the existence of the dimension value or the practical computation of it. These are mathematically equivalent but pedagogically distinct.',
    'If theoretical: the constraint is about measurement resolution. If computational: the constraint includes algorithm choice dependencies. Both yield mountain classification but with different emphasis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_vs_theoretical_dimension, conceptual, 'Whether the constraint is theoretical or computational in nature').

omega_variable(
    scale_sampling_sufficiency,
    'How many scales must be sampled to approximate fractal dimension within a given error tolerance?',
    'Established in fractal geometry literature: the number of scales needed grows logarithmically with desired precision. But the practical stopping point depends on when self-similarity breaks down (the fractal is not truly infinite).',
    'If self-similarity is persistent: fractal dimension remains well-defined across scales. If self-similarity breaks down quickly: practical dimension is less meaningful. Classification remains mountain in both cases, but the domain of applicability shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scale_sampling_sufficiency, empirical, 'Number of scales required for dimension approximation').

omega_variable(
    fractal_definition_scope,
    'Does the constraint apply to mathematical fractals (strictly self-similar, infinite) or natural fractals (approximately self-similar, finite)?',
    'Distinguish definitions: Hausdorff dimension applies rigorously to infinite mathematical fractals; box-counting applies approximately to finite natural objects. The constraint''s universality depends on the scope.',
    'If universal (mathematical fractals): mountain classification is unambiguous. If domain-limited (natural fractals): the constraint is mountain for theoretical case, rope or scaffold for practical estimation case. Recommend constraining to mathematical definition for mountain purity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fractal_definition_scope, conceptual, 'Scope of fractal definition (mathematical vs natural)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fractal_dimension_computation, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(fractal_dimension_computation, fractal_brownian_motion_simulation).
narrative_ontology:affects_constraint(fractal_dimension_computation, self_affine_surface_estimation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
