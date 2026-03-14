% ============================================================================
% CONSTRAINT STORY: functor_naturality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_functor_naturality, []).

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
 *   constraint_id: functor_naturality
 *   human_readable: Functor Naturality in Category Theory
 *   domain: mathematics/category_theory
 *
 * SUMMARY:
 *   Functor naturality is a foundational requirement in category theory: any
 *   natural transformation between functors F, G: C → D must satisfy the
 *   naturality condition — for every morphism f: X → Y in C, the square G(f)
 *   ∘ η_X = η_Y ∘ F(f) commutes. This constraint is not enforced by external
 *   authority or contingent institutional practice. It is a logical
 *   consequence of the axioms of category theory. No alternative definition
 *   of 'natural transformation' that violates this square can maintain
 *   categorical coherence. The constraint exhibits all structural signatures
 *   of a mathematical mountain: zero degrees of freedom for all agents,
 *   perfect accessibility collapse (any attempt to violate it produces
 *   immediate logical contradiction), and emergence from first principles
 *   rather than convention or negotiation.
 *
 * KEY AGENTS:
 *   - Category theorists: Institutional/arbitrage — work within the constraint; perceive it as enabling rather than limiting; can move between different categorical frameworks while preserving naturality
 *   - Pure mathematicians: Powerful/mobile — recognize naturality as a logical necessity but operate in domains where the constraint is not always relevant; can choose to work outside category theory
 *   - Students and pedagogical institutions: Powerless/trapped and institutional/arbitrage respectively — encounter naturality as a fixed definition to be learned; cannot negotiate or modify the requirement
 *   - Mathematical foundations: Analytical/analytical — treat functor naturality as a structural feature of the category-theoretic axiom system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(functor_naturality, 0.12).
domain_priors:suppression_score(functor_naturality, 0.03).
domain_priors:theater_ratio(functor_naturality, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(functor_naturality, extractiveness, 0.12).
narrative_ontology:constraint_metric(functor_naturality, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(functor_naturality, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(functor_naturality, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(functor_naturality, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(functor_naturality, mountain).
narrative_ontology:human_readable(functor_naturality, "Functor Naturality in Category Theory").
narrative_ontology:topic_domain(functor_naturality, "mathematics/category_theory").

domain_priors:emerges_naturally(functor_naturality).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — Functor naturality is a logical necessity in category theory. A natural transformation between functors F and G must satisfy the naturality square commutation for ALL objects in the source category. This is not contingent on representation, measurement, or convention — it is an irreducible structural requirement. The axioms of category theory entail naturality necessarily.
constraint_indexing:constraint_classification(functor_naturality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PURE MATHEMATICIAN (MOUNTAIN) — From the perspective of those working within category-theoretic frameworks, naturality is an immutable constraint on any coherent definition of natural transformation. No working mathematician can define a natural transformation that violates the commutative squares — doing so would produce a logical contradiction. The constraint has zero degrees of freedom.
constraint_indexing:constraint_classification(functor_naturality, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: STUDENT PERSPECTIVE (MOUNTAIN) — A student encountering the definition of natural transformation cannot escape the requirement. The definition is given; the student cannot modify, negotiate, or work around it. Functor naturality presents itself as a fixed logical framework within which all work in category theory must occur. Appears as an immutable natural law from the pedagogical standpoint.
constraint_indexing:constraint_classification(functor_naturality, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICAL INSTITUTION (MOUNTAIN) — Category theory is taught, published, and formalized as a unified logical framework. Mathematical institutions enforce the axioms of category theory not through coercion but through logical consistency. Any institution that attempted to define a non-natural transformation would immediately lose coherence. The constraint is institutional in scope but immutable in character.
constraint_indexing:constraint_classification(functor_naturality, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(functor_naturality_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(functor_naturality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(functor_naturality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(functor_naturality, ExtMetricName, E),
    domain_priors:suppression_score(functor_naturality, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(functor_naturality),
    narrative_ontology:constraint_metric(functor_naturality, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(functor_naturality, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(functor_naturality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint imposes no extraction from any agent — it is a structural feature that benefits all users of category theory equally by ensuring coherence. The small non-zero value reflects that the constraint's universality does require learning overhead for those entering the field, but this is legitimate knowledge cost, not extraction. Suppression (0.03): Minimal. There are no barriers to understanding or applying functor naturality beyond the general cognitive effort required to learn category theory itself. The constraint admits no alternatives, but this is logical necessity, not coercive suppression. Theater ratio (0.15): Very low. Functor naturality has no performative component — it is either satisfied or the definition fails. No agent benefits from maintaining the appearance of naturality while violating it; no ritual or ceremonial element masks the constraint's operation. Accessibility collapse (0.92): Very high. Any attempt to violate the naturality square produces immediate logical contradiction. There is no gray zone, no context-dependent interpretation, no workaround. The constraint is perfectly rigid. Resistance (0.08): Very low. The constraint is so intrinsic to the categorical framework that resistance is conceptually incoherent — proposing a non-natural transformation is equivalent to proposing a non-transformation, which is category-theoretically meaningless.
 *
 * PERSPECTIVAL GAP:
 *   Minimal to none. All perspectives classify the constraint as Mountain. The analytical observer, the pure mathematician, the student, and the mathematical institution all perceive functor naturality as an immutable logical requirement. The perspectival agreement is complete because the constraint is logically necessary — no agent experiences it differently. This is the signature of a genuine mathematical natural law: invariance across all observational positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Functor naturality exhibits zero directionality variation across agents. There is no beneficiary and no victim — the constraint benefits all users of category theory by maintaining coherence and enabling rigorous proof. The sigmoid directionality function f(d) is not relevant because the constraint is not extractive. All agents occupy the same position relative to the constraint: subjects of its necessity, not targets of its extraction. This uniform non-extractiveness is the key signature distinguishing genuine mathematical mountains from contingent institutional constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN-ONLY CONSTRAINT: Functor naturality resolves the mandatrophy by classification invariance. All perspectives produce Mountain. The constraint admits no ambiguity between coordination and extraction because it performs neither function — it is a logical requirement that enables mathematical coherence. The mandatrophy question 'is this coordination or extraction?' does not apply to mathematical necessities. The constraint is neither; it is a structural precondition for both. This exemplifies the narrow but genuine class of constraints that are truly universal and immutable natural laws rather than contingent institutional arrangements masked as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_categorical_foundations,
    'Are there alternative categorical foundations that do not require functor naturality, yielding a genuinely different mathematical structure?',
    'Survey of non-standard categorical frameworks (enriched categories, higher categories, type-theoretic foundations); analysis of whether these frameworks eliminate or redefine naturality',
    'If true alternatives exist: functor naturality is contingent on Eilenberg-Mac Lane axiom choice, not a universal natural law. If only one coherent foundation: naturality is mathematically necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_categorical_foundations, conceptual, 'Whether alternative categorical axioms eliminate naturality').

omega_variable(
    interpretive_flexibility_in_definitions,
    'Does the naturality requirement admit interpretive flexibility in how the commutative square is instantiated across different object categories?',
    'Formal analysis of whether naturality can be weakened (pseudonatural transformations, lax natural transformations) while preserving categorical coherence',
    'If flexibility exists: naturality is a rigid framework choice, not an absolute law. If rigidity is total: the constraint is fully immutable across all coherent interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_flexibility_in_definitions, empirical, 'Whether naturality admits weakened or alternative formulations').

omega_variable(
    foundations_independence_from_set_theory,
    'Is functor naturality independent of the foundational choice (set theory vs type theory vs homotopy type theory)?',
    'Formalization in multiple foundational systems; comparison of naturality definitions across ZFC, type theory, and HoTT',
    'If independent: naturality is a genuine mathematical universal. If dependent: naturality is contingent on foundational axiom choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundations_independence_from_set_theory, conceptual, 'Whether naturality is foundationally independent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(functor_naturality, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(functor_naturality, information_standard).
narrative_ontology:affects_constraint(functor_naturality, adjoint_functor_definition).
narrative_ontology:affects_constraint(functor_naturality, category_theory_axioms).

% DUAL FORMULATION NOTE:
% Functor naturality is a foundational constraint upstream of all category-theoretic structures. It does not decompose into distinct stories — the naturality requirement is identical across all categorical domains. No ε-invariance ambiguity exists because the constraint is measured only one way: logical satisfaction of the commutative square.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
