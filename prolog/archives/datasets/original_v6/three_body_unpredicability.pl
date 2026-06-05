% ============================================================================
% CONSTRAINT STORY: three_body_unpredicability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_three_body_unpredicability, []).

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
 *   constraint_id: three_body_unpredicability
 *   human_readable: The Three-Body Problem (Computational Irreducibility)
 *   domain: mathematics/physics/computational_science
 *
 * SUMMARY:
 *   The Three-Body Problem is a mathematical limit that has persisted
 *   unchanged since Newton. It describes the motion of three masses
 *   interacting gravitationally and demonstrates that, unlike the two-body
 *   problem, no closed-form analytical solution exists for the general case.
 *   The constraint manifests as computational irreducibility: predicting
 *   future states requires integrating the equations of motion numerically,
 *   and the system exhibits sensitive dependence on initial conditions
 *   (chaos), making long-term predictions unreliable even with perfect
 *   initial data. This is a genuine natural law — not enforced by any agent,
 *   not contingent on technology or policy, but intrinsic to the mathematical
 *   structure of nonlinear dynamics. The three-body problem is invariant
 *   across all observation contexts: it appears as a mountain from every
 *   structural position because no agent, institution, or computational
 *   resource can circumvent the fundamental barrier.
 *
 * KEY AGENTS:
 *   - Computational Agent: Seeks deterministic prediction of three-body trajectories; encounters irreducible computational barrier; no exit option
 *   - Astronomer/Orbital Mechanicist: Requires long-term celestial prediction for mission planning; confronts practical prediction horizon limits (~100 years maximum); works within bounded-prediction approximations
 *   - Mathematical Institution: Recognizes three-body problem as canonical exemplar of nonlinear dynamics and chaos; advances understanding of what can and cannot be computed
 *   - Analytical Observer: Civilizational/universal perspective recognizing the constraint as a mathematical limit, not a policy or enforcement mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(three_body_unpredicability, 0.12).
domain_priors:suppression_score(three_body_unpredicability, 0.03).
domain_priors:theater_ratio(three_body_unpredicability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(three_body_unpredicability, extractiveness, 0.12).
narrative_ontology:constraint_metric(three_body_unpredicability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(three_body_unpredicability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(three_body_unpredicability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(three_body_unpredicability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(three_body_unpredicability, mountain).
narrative_ontology:human_readable(three_body_unpredicability, "The Three-Body Problem (Computational Irreducibility)").
narrative_ontology:topic_domain(three_body_unpredicability, "mathematics/physics/computational_science").

domain_priors:emerges_naturally(three_body_unpredicability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL AGENT (MOUNTAIN) — Any agent attempting to predict three-body trajectories confronts an irreducible computational barrier. Sensitivity to initial conditions and the absence of closed-form general solutions create a fundamental limit independent of computational power. No exit exists from this constraint for agents requiring deterministic prediction.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ASTRONOMER (MOUNTAIN) — Classical astronomers modeling celestial mechanics encounter the three-body problem as an irreducible barrier to long-term orbital prediction. Attempts to improve predictions through higher-precision measurement or computational refinement reveal only that the barrier is intrinsic to the problem structure, not to measurement or algorithmic limitations.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL INSTITUTION (MOUNTAIN) — Mathematical physics and dynamical systems theory recognize the three-body problem as a fundamental exemplar of nonlinear dynamics and chaotic systems. The constraint manifests as a limit on what can be analytically computed — not a policy or enforcement mechanism, but a property of mathematical structure itself.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational analytical standpoint, the three-body problem represents a genuine natural law: the conjunction of nonlinearity, sensitivity to initial conditions, and the absence of conserved quantities (beyond energy and momentum) produces irreducible computational complexity. This is not a contingent limitation of current technology or knowledge — it is a property of the problem's mathematical structure.
constraint_indexing:constraint_classification(three_body_unpredicability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(three_body_unpredicability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(three_body_unpredicability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(three_body_unpredicability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(three_body_unpredicability, ExtMetricName, E),
    domain_priors:suppression_score(three_body_unpredicability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(three_body_unpredicability),
    narrative_ontology:constraint_metric(three_body_unpredicability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(three_body_unpredicability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(three_body_unpredicability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The three-body problem is not an extraction mechanism — no agent benefits at another's expense. The constraint is purely a limit on predictability. The extractiveness score reflects the minimal structural friction (some computational cost to simulate, some uncertainty in real-world applications), but the fundamental barrier is not one agent extracting from another. It is a property of mathematics itself. Suppression (0.03): Minimal. There is no coercion, no alternative suppression, no lack of exit mechanisms — the barrier is not one that suppression creates. Agents cannot 'choose' to exit the constraint any more than they can choose to exit gravity. Theater ratio (0.15): Very low. The three-body problem is not theatrical — there is no performative component. The computational barrier is real and fundamental; it is not maintained by institutional inertia or social agreement. The slight nonzero theater reflects only that some practical applications use approximation methods that are calibrated to specific use cases, introducing small elements of domain-specific convention. Accessibility collapse (0.92): Very high. The constraint is irreducibly inaccessible — no amount of computational power, better measurement, or algorithmic innovation can overcome the fundamental barrier. Resistance (0.08): Very low. The constraint does not resist — it simply is. There is no sense in which anyone 'resists' the three-body problem or 'enforces' it.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this constraint. All four perspectives (computational agent, astronomer, mathematical institution, analytical observer) classify it as mountain because the three-body problem is a universal natural law. The barrier appears identical regardless of who observes it or what framework they use. This uniformity is not a failure of the indexical system — it is the system correctly identifying that some constraints are invariant across all observation contexts. The mountain classification is the same from every (P, T, E, S) tuple because the constraint is not a social, institutional, or contingent phenomenon. It is a property of nonlinear mathematics.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this constraint. The three-body problem is a mountain (natural law) from all perspectives, and there is no risk of mislabeling coordination as extraction or vice versa. The constraint has no beneficiaries or victims — it is not a mechanism of coordination or extraction, but a limit on what can be computed. All six constraint types (mountain, rope, tangled_rope, snare, scaffold, piton) represent human institutional or coordination phenomena. The three-body problem transcends this taxonomy because it is not a human phenomenon at all. It would persist unchanged even if all human institutions were dissolved. The analytical observer's perspective correctly identifies it as a mathematical law, not a social or institutional structure. No mandatrophy resolution is needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    computational_irreducibility_vs_chaos,
    'Is the three-body problem''s unpredictability fundamentally due to computational irreducibility (Wolfram''s sense) or due to classical chaos and sensitive dependence on initial conditions?',
    'Formal definition and proof of computational irreducibility as distinct from chaos; examination of whether any polynomial-time algorithm exists for three-body trajectory prediction',
    'If irreducibility is fundamental: the constraint is a mountain reflecting Wolfram''s computational principle. If chaos alone explains unpredictability: the constraint may be reclassified as a rope (coordination problem for approximate methods) or scaffold (temporary limitation as computational power grows).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_irreducibility_vs_chaos, conceptual, 'Whether unpredictability is computational irreducibility or classical chaos').

omega_variable(
    bounded_prediction_sufficiency,
    'For practical purposes (satellite launches, orbital mechanics, collision avoidance), is bounded-time prediction (accurate to 100 years or less) sufficient, making the three-body constraint a solved problem for real applications?',
    'Empirical review of aerospace and astronomical applications; assessment of whether practical systems require or achieve beyond-bounded predictions; cost-benefit analysis of brute-force numerical integration vs closed-form solution attempts',
    'If bounded prediction is sufficient: the constraint transforms from Mountain (absolute barrier) to Scaffold or Rope (solvable within practical time horizons). If unbounded prediction is required for critical applications: the Mountain classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bounded_prediction_sufficiency, preference, 'Whether practical applications require unbounded or bounded prediction').

omega_variable(
    natural_law_universality,
    'Does the three-body problem represent a universal natural law (mountain) or is it an artifact of classical mechanics that may be circumvented or dissolved in other theoretical frameworks (quantum gravity, modified dynamics)?',
    'Investigation of whether quantum mechanics, general relativistic effects, or alternative gravitational theories alter the computational properties of three-body systems; examination of whether the constraint persists across multiple theoretical frameworks',
    'If universal: the mountain classification is robust across all known physics. If framework-dependent: the constraint is actually a rope (classical mechanics coordination framework) that dissolves under different assumptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_universality, conceptual, 'Whether the constraint is universal or framework-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(three_body_unpredicability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tbp_tr_t0, three_body_unpredicability, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tbp_tr_t5, three_body_unpredicability, theater_ratio, 5, 0.14).
narrative_ontology:measurement(tbp_tr_t10, three_body_unpredicability, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(tbp_be_t0, three_body_unpredicability, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tbp_be_t5, three_body_unpredicability, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(tbp_be_t10, three_body_unpredicability, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(three_body_unpredicability, information_standard).
narrative_ontology:affects_constraint(three_body_unpredicability, n_body_computational_complexity).
narrative_ontology:affects_constraint(three_body_unpredicability, chaos_trajectory_divergence).
narrative_ontology:affects_constraint(three_body_unpredicability, bounded_prediction_horizon).

% DUAL FORMULATION NOTE:
% The three-body problem is the canonical exemplar of classical chaos and computational irreducibility. Downstream constraints (n-body systems, trajectory divergence rates, bounded prediction horizons in specific applications) inherit the fundamental unpredictability from this core natural law. All are linked by the shared mathematical structure of nonlinear dynamical systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
