% ============================================================================
% CONSTRAINT STORY: weierstrass_function
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_weierstrass_function, []).

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
 *   constraint_id: weierstrass_function
 *   human_readable: The Existence of Continuous, Nowhere-Differentiable Functions
 *   domain: mathematical/foundational
 *
 * SUMMARY:
 *   The Weierstrass function is the canonical example of a continuous,
 *   nowhere-differentiable function. It exists on the real line as a
 *   rigorously defined limit of an infinite series and possesses no tangent
 *   line at any point despite being continuous everywhere. This constraint
 *   represents a fundamental mathematical truth: continuity and
 *   differentiability are logically independent properties of functions. The
 *   constraint is not enforced by any agent, does not benefit any group, and
 *   cannot be negotiated or circumvented. It is a natural law of mathematics
 *   — an invariant feature of the logical structure underlying real analysis.
 *   The existence of such pathological functions has profound implications
 *   for applied mathematics, engineering, physics, and computer science,
 *   forcing practitioners to accept that idealized smooth models of physical
 *   phenomena may be incomplete. The constraint appears differently from
 *   different perspectives: as a pure logical boundary (analytical), as a
 *   limit on physical modeling (applied engineer), as a computability barrier
 *   (computer science), as a curriculum requirement (educational
 *   institutions), as a cognitive reorganization (learning process), and as
 *   an intuition-shattering discovery (naive student). Notably, the
 *   constraint exhibits zero extraction, zero suppression, and minimal
 *   theater — all hallmarks of a genuine natural law.
 *
 * KEY AGENTS:
 *   - The Mathematical Universe: No agent; defines the structural landscape
 *   - Applied Engineers: Powerful institutional actors who must accept that detailed models may not be smooth
 *   - Computer Scientists: Organized community encountering algorithmic limits on derivative computation
 *   - Educational Institutions: Institutional beneficiary of the constraint (it defines rigor boundaries, but gains no extractive advantage)
 *   - Graduate Students: Moderate individual actors undergoing cognitive reorganization
 *   - Naive Student Intuition: Powerless perspective encountering fundamental intuition-boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(weierstrass_function, 0.12).
domain_priors:suppression_score(weierstrass_function, 0.03).
domain_priors:theater_ratio(weierstrass_function, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(weierstrass_function, extractiveness, 0.12).
narrative_ontology:constraint_metric(weierstrass_function, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(weierstrass_function, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(weierstrass_function, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(weierstrass_function, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(weierstrass_function, mountain).
narrative_ontology:human_readable(weierstrass_function, "The Existence of Continuous, Nowhere-Differentiable Functions").
narrative_ontology:topic_domain(weierstrass_function, "mathematical/foundational").

domain_priors:emerges_naturally(weierstrass_function).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL UNIVERSE (MOUNTAIN) — The existence of continuous, nowhere-differentiable functions is a logical consequence of the real number system and the definitions of continuity and differentiability. No agent escapes this constraint; it is constitutive of the mathematical landscape itself. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14. Accessibility collapse = 0.92: once the definitions are grasped, the existence proof is inevitable. Resistance = 0.08: no force can overturn this constraint.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED ENGINEER (MOUNTAIN) — The nowhere-differentiable property appears as a fundamental limit on idealized smooth models of physical phenomena. Real materials and processes exhibit scale-dependent roughness; the Weierstrass function represents the logical endpoint of increasingly fine-grained measurement. Engineers cannot exit this; they can only accept that sufficiently detailed models of real phenomena may lack tangent planes. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.07. The constraint is a natural boundary condition on engineering epistemology, not a choice.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPUTER SCIENCE COMMUNITY (MOUNTAIN) — No finite algorithm can compute the pointwise derivative of a Weierstrass function at any point. This is not a limitation of current technology but a structural impossibility flowing from the definition. Organized computing communities encounter this as an irreducible boundary: some functions are representable, others are not. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.09. The constraint defines what is computable vs uncomputable.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: MATHEMATICAL EDUCATION SYSTEM (MOUNTAIN) — Institutions teaching real analysis must confront the existence of continuous, nowhere-differentiable functions. The constraint cannot be evaded or negotiated. Every calculus curriculum that aspires to rigor must eventually acknowledge that continuity does not imply differentiability. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.001. The constraint defines the boundary of undergraduate vs advanced mathematics, but does not extract from educational institutions — it merely enforces a structural hierarchy in mathematical knowledge.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: GRADUATE STUDENT (PROOF-DEPENDENT LEARNING) (MOUNTAIN) — Individual learners confronting the Weierstrass function for the first time encounter a fundamental reorganization of intuition. The constraint appears as an irreducible cognitive boundary: you either grasp that continuity and differentiability are independent properties or you do not. There is no way to relax the definition to make the constraint disappear. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.14. For the individual, this is a high-accessibility-collapse moment: comprehension forces a restructuring of prior belief.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: STUDENT INTUITION / NAIVE CALCULUS (MOUNTAIN) — Pre-rigorous mathematical intuition (smooth functions are differentiable) encounters an absolute structural boundary. The Weierstrass function is a proof that naive intuition is incomplete. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.13. The constraint shows that certain forms of understanding are impossible without rigorous definition. This perspective is not trapped by the Weierstrass function but by the landscape it inhabits — the mathematical universe in which continuity and differentiability are independent.
constraint_indexing:constraint_classification(weierstrass_function, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(weierstrass_function_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(weierstrass_function, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(weierstrass_function, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(weierstrass_function, ExtMetricName, E),
    domain_priors:suppression_score(weierstrass_function, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(weierstrass_function),
    narrative_ontology:constraint_metric(weierstrass_function, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(weierstrass_function, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(weierstrass_function_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The Weierstrass function does not extract resources from anyone. It is not a coordination mechanism. It is not coercive. It simply defines what is logically possible within the real number system. The minimal extractiveness reflects the canonical definition of a mountain: base extraction ≤ 0.25. The 0.12 value is derived from the entropy in the definition itself — there is infinitesimal freedom in how one constructs the Weierstrass function, but this does not constitute extraction. Suppression (0.03): Negligible. The constraint does not suppress alternatives or create barriers to exit. It simply states: IF you accept the definitions of real numbers, continuity, and differentiability, THEN nowhere-differentiable continuous functions exist. There is no force or coercion, only logical necessity. Theater ratio (0.08): Minimal. The mathematical proof of the Weierstrass function's existence is nearly pure functionality with negligible performative content. The proof either works or it does not; there is no negotiation or ritual. The small theater reflects minor expository choices in how the result is presented, not the substantive claim.
 *
 * PERSPECTIVAL GAP:
 *   All six perspectives agree on the classification (mountain) but experience the constraint's binding force differently. The analytical observer sees universal logical necessity. The engineer sees a modeling limitation. The computer scientist sees an algorithmic boundary. The educational institution sees a curriculum requirement. The graduate student sees cognitive reorganization. The naive student sees an intuition-shattering discovery. These are not disagreements about the constraint's existence or nature but different positions relative to how it intersects with their work and understanding. The perspectival gap does not generate alternative classifications; it generates alternative *experiences* of the same immutable boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   The Weierstrass function has no beneficiaries or victims in the structural sense. No agent extracts value from others through this constraint. The directionality derivation yields near-zero χ from all perspectives because there is no meaningful directionality — the constraint is not aimed at anyone. The canonical fallback values apply: analytical → d≈0.72, f(d)≈1.15, but without beneficiary/victim data, the derivation yields no extractiveness. This is the structural signature of a pure natural law: d values vary by perspective (different observers inhabit different epistemic positions), but χ remains near-zero because there is no extraction function to be scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   PURE MOUNTAIN DIAGNOSIS: The Weierstrass function resolves mandatrophy by exemplifying a constraint with zero extraction, zero suppression, and zero beneficiary/victim asymmetry. The constraint does not mandate a deceptive use of language (mandatrophy). It does not mislabel coordination as extraction or vice versa. It simply states a logical boundary. The classification as mountain is transparent and justified by all six base metrics. There is no hidden extraction mechanism, no coordination function being mischaracterized, no theater substituting for function. The Weierstrass function is the ideal case where the indexical classification system produces maximal clarity: the constraint exists, it binds all observers equally, and its nature is not obscured by power asymmetries or institutional deception.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_vs_classical,
    'Does the Weierstrass function ''exist'' in constructive mathematics where the intermediate value theorem requires computable witnesses?',
    'Formal analysis of constructive real analysis frameworks; comparison of Weierstrass existence proof with constructivist requirements for real numbers',
    'If Weierstrass-type functions do not exist constructively: the constraint is foundational assumption-dependent (classical logic), not universal. If they do exist constructively: the constraint is preserved across foundational frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_vs_classical, conceptual, 'Whether Weierstrass functions exist in constructive mathematics').

omega_variable(
    physical_realizability,
    'Is the Weierstrass function''s nowhere-differentiability a mathematical limit or a mischaracterization of how physical continuity should be defined?',
    'Foundational physics analysis of whether continuous physical observables must be differentiable; investigation of whether quantum mechanics or relativistic field theory require smooth fields',
    'If physical continuity need not be smooth: Weierstrass represents a structural feature of real analysis but not of physical law, and engineers should not treat it as a constraint on physical models. If physical fields must be smooth: Weierstrass represents a genuine boundary condition on physical science.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_realizability, empirical, 'Whether physical continuity implies differentiability').

omega_variable(
    approximation_sufficiency,
    'For practical purposes, are smooth approximations to nowhere-differentiable functions sufficiently accurate that the existence of non-smooth continuous functions is epistemically negligible?',
    'Error analysis of smooth approximations (mollified approximations, spectral approximations) to Weierstrass-type functions; measurement of approximation quality vs application requirements',
    'If smooth approximations are always sufficient within specified error bounds: the Weierstrass constraint is theoretically binding but practically irrelevant, downgrading it from mountain to piton. If some applications require non-smooth continuous models: the constraint retains mountain status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(approximation_sufficiency, empirical, 'Whether smooth approximations suffice for practical nowhere-differentiable functions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(weierstrass_function, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(weierstrass_theater_initial, weierstrass_function, theater_ratio, 0, 0.05).
narrative_ontology:measurement(weierstrass_theater_midpoint, weierstrass_function, theater_ratio, 100, 0.08).
narrative_ontology:measurement(weierstrass_theater_final, weierstrass_function, theater_ratio, 200, 0.08).

% Extraction over time
narrative_ontology:measurement(weierstrass_extract_initial, weierstrass_function, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(weierstrass_extract_midpoint, weierstrass_function, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(weierstrass_extract_final, weierstrass_function, base_extractiveness, 200, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(weierstrass_function, intermediate_value_theorem).
narrative_ontology:affects_constraint(weierstrass_function, fundamental_theorem_calculus).
narrative_ontology:affects_constraint(weierstrass_function, hausdorff_dimension_fractals).

% DUAL FORMULATION NOTE:
% The Weierstrass function sits at the intersection of real analysis, topology, and harmonic analysis. It is upstream of fractal theory (self-similar nowhere-differentiable curves), downstream of the intermediate value theorem (which establishes continuity without differentiability), and orthogonal to the fundamental theorem of calculus (which assumes differentiability). It is not a constraint family decomposition but a network node in the foundational mathematics dependency graph.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
