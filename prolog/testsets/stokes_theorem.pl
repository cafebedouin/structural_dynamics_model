% ============================================================================
% CONSTRAINT STORY: stokes_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stokes_theorem, []).

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
 *   constraint_id: stokes_theorem
 *   human_readable: Stokes Theorem: Relationship Between Surface and Boundary Integration
 *   domain: mathematics/differential_geometry
 *
 * SUMMARY:
 *   Stokes Theorem is a foundational result in differential geometry
 *   establishing the relationship between a surface integral and a line
 *   integral around the surface's boundary. Formally: the integral of a
 *   differential form over a manifold's boundary equals the integral of its
 *   exterior derivative over the manifold itself. This constraint exhibits
 *   the defining characteristics of a natural law: it is invariant across all
 *   coordinate systems, all practical measurement methodologies, all domains
 *   of application, and all time periods of mathematical development. The
 *   theorem cannot be circumvented, negotiated, or evaded through
 *   institutional arrangement, choice of formalism, or contextual variation.
 *   Its universality and necessity place it unambiguously in the mountain
 *   category.
 *
 * KEY AGENTS:
 *   - Computational Practitioners (powerless/trapped): Practitioners in engineering, physics, and applied mathematics must accept Stokes Theorem as an immutable constraint on vector field calculations — no exit option exists
 *   - Mathematical Theorists (powerful/mobile): Mathematicians with access to abstract formalisms and alternative representations still encounter the theorem as necessary — formal power does not provide escape
 *   - Analytical Observer (analytical/analytical): Cross-domain observers see the theorem's necessity emerge from fundamental properties of differential geometry and topological structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stokes_theorem, 0.08).
domain_priors:suppression_score(stokes_theorem, 0.02).
domain_priors:theater_ratio(stokes_theorem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stokes_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(stokes_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(stokes_theorem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stokes_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(stokes_theorem, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stokes_theorem, mountain).
narrative_ontology:human_readable(stokes_theorem, "Stokes Theorem: Relationship Between Surface and Boundary Integration").
narrative_ontology:topic_domain(stokes_theorem, "mathematics/differential_geometry").

domain_priors:emerges_naturally(stokes_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL PRACTITIONER (MOUNTAIN) — An engineer or physicist attempting to evaluate a vector field across a surface faces an absolute constraint: the flux through the surface equals the circulation around its boundary. This relationship is invariant across all practical measurement contexts, coordinate systems, and material domains. No exit option exists — the theorem cannot be circumvented or negotiated.
constraint_indexing:constraint_classification(stokes_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL THEORIST (MOUNTAIN) — Despite possessing sophisticated mathematical tools and the ability to work in alternative formalisms, the theorist encounters Stokes Theorem as an immutable consequence of differential geometry. Reparameterization, coordinate transformation, or choice of representation cannot escape the fundamental relationship. The theorem's necessity is structural, not contextual.
constraint_indexing:constraint_classification(stokes_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a cross-domain analytical position examining physics, mathematics, and engineering simultaneously, Stokes Theorem appears as a natural law underlying vector field behavior. Its universality across all observables, all coordinate systems, and all scales indicates emergence from structural properties of space and differentiation itself, not from institutional or contingent constraints.
constraint_indexing:constraint_classification(stokes_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stokes_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(stokes_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stokes_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(stokes_theorem, ExtMetricName, E),
    domain_priors:suppression_score(stokes_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(stokes_theorem),
    narrative_ontology:constraint_metric(stokes_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(stokes_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(stokes_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Stokes Theorem imposes no extraction from one agent to another — it is a pure structural relationship. The value reflects only the minimal 'cost' of applying the theorem in computation (it requires effort to implement, but this is not extraction in the DR sense). Suppression (0.02): Near zero. The theorem provides zero suppression of alternatives because no alternative exists — agents do not face coercion, they face mathematical necessity. Theater ratio (0.05): Negligible. The theorem's application requires no performative rituals or symbolic legitimation — it works identically in open and closed settings, with audiences or without.
 *
 * PERSPECTIVAL GAP:
 *   Unlike extractive constraints that appear different from different positions, Stokes Theorem appears identically from all perspectives. The computational practitioner, the theorist, and the analytical observer all encounter the same necessary relationship. This uniform classification across all perspectives is diagnostic of mountain status: the constraint does not depend on observer position, institutional context, or measurement choice.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality analysis applies to this constraint because no extraction flow exists. There is no beneficiary and no victim — Stokes Theorem is a pure structural fact about differential geometry that all agents encounter equally. The theorem is neither asymmetric nor coordinating in the institutional sense. It is simply true.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalization_scope,
    'Does Stokes Theorem represent a fundamental law of differential geometry or a special case of more general topological principles?',
    'Analysis of Stokes Theorem as a special case of the generalized Stokes Theorem (de Rham cohomology); examination of validity across different manifold types and topological structures',
    'If special case: classification remains mountain but with contextual boundary. If fundamental: confirms mountain classification at maximum universality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalization_scope, conceptual, 'Scope of Stokes Theorem within differential topology').

omega_variable(
    formalism_invariance,
    'Is the theorem truly coordinate-independent, or does its appearance of coordinate-independence depend on implicit topological assumptions?',
    'Rigorous examination of the theorem''s validity on non-orientable manifolds, manifolds with boundary singularities, and exotic topological spaces; testing invariance claims in pathological cases',
    'If fully invariant: mountain classification confirmed. If dependent on hidden assumptions: may degrade to rope (coordination via topological convention).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalism_invariance, empirical, 'Whether Stokes Theorem is coordinate-independent across all manifold types').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stokes_theorem, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stokes_tr_t0, stokes_theorem, theater_ratio, 0, 0.05).
narrative_ontology:measurement(stokes_tr_t500, stokes_theorem, theater_ratio, 500, 0.04).
narrative_ontology:measurement(stokes_tr_t1000, stokes_theorem, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(stokes_be_t0, stokes_theorem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(stokes_be_t500, stokes_theorem, base_extractiveness, 500, 0.07).
narrative_ontology:measurement(stokes_be_t1000, stokes_theorem, base_extractiveness, 1000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stokes_theorem, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
