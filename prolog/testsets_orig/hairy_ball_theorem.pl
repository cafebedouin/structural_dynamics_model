% ============================================================================
% CONSTRAINT STORY: hairy_ball_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hairy_ball_theorem, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hairy_ball_theorem
 *   human_readable: Hairy Ball Theorem: Topological Immutability of Continuous Vector Fields on Even-Dimensional Spheres
 *   domain: topology/mathematics
 *
 * SUMMARY:
 *   The hairy ball theorem is a foundational result in algebraic topology
 *   stating that every continuous vector field tangent to an even-dimensional
 *   sphere must vanish at least once. Formally: a 2n-sphere (n ≥ 0) admits no
 *   continuous non-vanishing tangent vector field. The theorem is not a
 *   constraint that emerges from institutions, power asymmetries, or
 *   extraction mechanisms — it is a mathematical law with no beneficiaries or
 *   victims. No agent benefits from the theorem's truth; no agent is harmed
 *   by it. The constraint is invariant across all observer positions and time
 *   scales. This is the prototype of a pure mountain constraint: it is not
 *   suppressed by institutional power, it does not extract from anyone, and
 *   there are zero degrees of freedom for any mathematical or physical agent
 *   to escape it.
 *
 * KEY AGENTS:
 *   - Topological Reality: The mathematical structure itself — the intrinsic geometry of even-dimensional spheres and the definition of continuous vector fields
 *   - Physicists and Engineers: Users of the constraint — they confront it when attempting to design systems (planetary wind models, spherical conductor fields) that violate the theorem
 *   - Mathematicians: Analysts of the constraint — they cannot disprove or circumvent it despite their institutional and intellectual resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hairy_ball_theorem, 0.12).
domain_priors:suppression_score(hairy_ball_theorem, 0.02).
domain_priors:theater_ratio(hairy_ball_theorem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hairy_ball_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(hairy_ball_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hairy_ball_theorem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hairy_ball_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hairy_ball_theorem, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hairy_ball_theorem, mountain).
narrative_ontology:human_readable(hairy_ball_theorem, "Hairy Ball Theorem: Topological Immutability of Continuous Vector Fields on Even-Dimensional Spheres").
narrative_ontology:topic_domain(hairy_ball_theorem, "topology/mathematics").

domain_priors:emerges_naturally(hairy_ball_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TOPOLOGICAL INVARIANT (MOUNTAIN) — The hairy ball theorem is a topological law: any continuous vector field on an even-dimensional sphere must have at least one point where the field vanishes. This is invariant across all mathematical frameworks, observer positions, and time scales. The constraint emerges necessarily from the intrinsic geometry of spheres and the definition of continuous vector fields. Zero degrees of freedom for any mathematical agent.
constraint_indexing:constraint_classification(hairy_ball_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PHYSICAL APPLICANT (MOUNTAIN) — A physicist attempting to construct a continuous, non-vanishing tangential field (e.g., for a weather model on a planet, a smoothly rotating charged particle distribution on a spherical conductor, or a global wind pattern with no calm spots) confronts an immutable barrier. The theorem's force is independent of measurement technique, computational precision, or physical ingenuity. No physical system can escape this constraint. The accessibility of alternatives collapses to zero.
constraint_indexing:constraint_classification(hairy_ball_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ENGINEERING CONSTRAINT (MOUNTAIN) — Engineers designing systems that require smooth, non-vanishing vector fields on spherical domains (fluid dynamics on planetary surfaces, electrostatic field configurations on spherical conductors, tensor field networks on global topologies) face an irreducible natural law. No institutional mandate, regulatory framework, or technological innovation can circumvent the theorem. The constraint is independent of power and resource availability.
constraint_indexing:constraint_classification(hairy_ball_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ORGANIZED MATHEMATICS (MOUNTAIN) — Professional mathematicians, despite their collective expertise and institutional resources, cannot prove the theorem false or construct a counterexample. The constraint is not subject to community consensus, funding allocation, or paradigm shifts. Even at immediate time scales, where institutions might hope for 'breakthroughs,' the theorem's truth is unchanging and independent of the observer's organizational status.
constraint_indexing:constraint_classification(hairy_ball_theorem, mountain,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hairy_ball_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hairy_ball_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hairy_ball_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hairy_ball_theorem, ExtMetricName, E),
    domain_priors:suppression_score(hairy_ball_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hairy_ball_theorem),
    narrative_ontology:constraint_metric(hairy_ball_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hairy_ball_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hairy_ball_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The theorem does not extract value from any agent — no beneficiary accumulates resources or power from the theorem's truth. The small non-zero value reflects that the theorem constrains possibility space itself, which is a form of 'extraction' only in the formal sense that it removes options. Suppression (0.02): Negligible. The theorem is not maintained through coercion, alternative suppression, or denial. Its truth is recognized and accepted universally. Theater ratio (0.05): Near-zero. The theorem has no performative dimension. Its truth is not sustained through ritual, institutional inertia, or theatrical display — it is maintained through valid mathematical proof.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists. All observers — from powerless physicists to institutional engineering teams to the analytical mathematical community — perceive the theorem as an immutable mountain. The constraint is invariant across all indexical positions (P, T, E, S). This uniformity is diagnostic of a true mountain: the constraint does not depend on the observer's power level, time horizon, exit options, or spatial scope. Every perspective produces the same classification because the constraint's force is independent of context.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality derivation does not apply to mountain constraints. The hairy ball theorem has no beneficiary or victim structure — no agent benefits at another's expense. The constraint is a feature of mathematical reality, not a social or institutional mechanism. No directionality value (d) is computed because there is no extraction flow to model.
 *
 * MANDATROPHY ANALYSIS:
 *   The hairy ball theorem does not require mandatrophy resolution because it is a pure natural law with zero extraction and zero suppression. The mandatrophy arises when a constraint appears to be pure coordination (rope) but reveals asymmetric extraction (snare) under analysis — forcing reclassification as tangled rope. The hairy ball theorem has no coordination function and no extraction, so the mandatrophy question does not arise. The theorem's universal mountain classification across all perspectives is not a failure case — it is the expected outcome when analyzing a genuine topological law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_boundary_assumption,
    'Does the theorem''s inevitability depend on the assumption of true continuity, or could discrete approximations or non-standard topologies escape the constraint?',
    'Formal analysis of theorem''s dependency on continuity axiom; investigation of discrete approximations on graph-theoretic spheres; exploration of non-Hausdorff or constructivist frameworks',
    'If continuity is essential: mountain classification holds universally. If discrete systems can escape: constraint is conditional on choice of mathematical framework (mountain within classical topology only).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_boundary_assumption, conceptual, 'Whether continuity assumption is essential to theorem''s force').

omega_variable(
    even_dimensional_necessity,
    'Is the restriction to even-dimensional spheres a fundamental feature or a artifact of the proof technique?',
    'Formal proof analysis; investigation of odd-dimensional sphere behavior; topological investigation of whether odd-dimensional surfaces admit non-vanishing fields universally',
    'If restriction is fundamental: constraint applies universally only to even-dimensional contexts. If artifact: constraint may be stronger in odd dimensions or reveal different underlying structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(even_dimensional_necessity, conceptual, 'Whether theorem''s dimensional restriction is fundamental').

omega_variable(
    physical_realization_fidelity,
    'Do physical systems in nature actually realize perfect continuity and exact sphericity, or do quantum effects, discrete atomic structure, and topological defects allow escape from the theorem''s implications?',
    'Quantum topology analysis; investigation of physical systems as continuous approximations to discrete substrates; empirical study of field configurations on planet-scale systems',
    'If physical systems are true instantiations: theorem''s natural law force is guaranteed. If quantum/discrete substrate rescues physical systems from pure continuity: constraint is mathematical law, not physical law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_realization_fidelity, empirical, 'Whether physical systems realize perfect continuity assumed by theorem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hairy_ball_theorem, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% The hairy ball theorem is not decomposed into sub-constraints. It is a single, unified topological law. Related constraints (brouwer_fixed_point_theorem, lefschetz_fixed_point_theorem) share similar topological mechanisms but are distinct mathematical claims with different ε values depending on their specific domain and formulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
