% ============================================================================
% CONSTRAINT STORY: representable_functors
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_representable_functors, []).

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
 *   constraint_id: representable_functors
 *   human_readable: Representable Functors in Category Theory
 *   domain: mathematics/category_theory
 *
 * SUMMARY:
 *   Representable functors are a fundamental structural fact of category
 *   theory with zero degrees of freedom. The constraint emerges necessarily
 *   from the Yoneda lemma: any functor from a locally small category into Set
 *   is representable if and only if it is naturally isomorphic to a Hom
 *   functor Hom(−, X) for some object X. This is not a convention, not a
 *   choice, and not negotiable within category-theoretic foundations. The
 *   constraint is a mathematical law, irreducible and invariant across all
 *   mathematical perspectives. No agent — whether a researcher, a
 *   computational system, or an abstract observer — can escape or modify the
 *   structural reality that representable functors describe.
 *
 * KEY AGENTS:
 *   - Functor Users: Any mathematical agent working with functors into Set faces the representability constraint as an invariant. Powerless in the sense that they cannot negotiate with the mathematical structure; constrained to work within its boundaries.
 *   - Category Theorists: Research communities studying category theory generate and refine understanding of representability but cannot change the underlying logical necessity. Organized but constrained by the axioms they adopt.
 *   - Analytical Observer: The position that sees representability as a pure mathematical fact, independent of any observer's perspective or preference.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(representable_functors, 0.12).
domain_priors:suppression_score(representable_functors, 0.03).
domain_priors:theater_ratio(representable_functors, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(representable_functors, extractiveness, 0.12).
narrative_ontology:constraint_metric(representable_functors, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(representable_functors, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(representable_functors, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(representable_functors, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(representable_functors, mountain).
narrative_ontology:human_readable(representable_functors, "Representable Functors in Category Theory").
narrative_ontology:topic_domain(representable_functors, "mathematics/category_theory").

domain_priors:emerges_naturally(representable_functors).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUNCTOR USER (MOUNTAIN) — Any functor mapping from a locally small category into Set faces an invariant constraint: representability is determined by whether it naturally transforms to the Hom functor. This is not a choice — it is a structural fact. The constraint has zero degrees of freedom; the user cannot negotiate or escape it. The mathematical structure admits no alternatives.
constraint_indexing:constraint_classification(representable_functors, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CATEGORY THEORIST (MOUNTAIN) — Representability follows from the Yoneda lemma as a logical necessity. Given the axioms of category theory and set theory, the existence and properties of representable functors cannot be otherwise. Researchers can apply or avoid the concept, but the mathematical reality it describes is invariant. No workaround or alternative framework changes the underlying structure.
constraint_indexing:constraint_classification(representable_functors, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — Representable functors constitute a natural law of abstract algebra. The constraint emerges necessarily from the axioms of category theory. The embedding theorem (Yoneda) is proven, not negotiated. The Hom-Set adjunction is a structural invariant. From the analytical position, this is an unchangeable logical fact.
constraint_indexing:constraint_classification(representable_functors, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(representable_functors_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(representable_functors, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(representable_functors, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(representable_functors, ExtMetricName, E),
    domain_priors:suppression_score(representable_functors, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(representable_functors),
    narrative_ontology:constraint_metric(representable_functors, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(representable_functors, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(representable_functors_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint imposes a structural fact on functors but does not extract value from any agent — it is a pure structural invariant. The small positive value reflects that representability does require a category and a target object X; these inputs are 'costs' in the minimal sense that the constraint requires them to be present. No agent bears asymmetric costs; no agent benefits unfairly. Suppression (0.03): Negligible. No alternatives exist, and there are no degrees of freedom to suppress — the structure simply is what it is. The minimal value reflects that accessibility to the constraint is limited (understanding category theory requires mathematical training) but this is not suppression — it is the natural difficulty of abstract mathematics, not a mechanism preventing exit or alternatives. Theater ratio (0.08): Negligible. Representable functors are a proven theorem with no performative content. The Yoneda lemma's proof is constructive; the result is exact. The small value accounts for pedagogical exposition (theorems must be explained) but the underlying mathematics is purely functional.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint as mountain because the representability constraint is invariant across all mathematical positions. A functor user, a category theorist, and an analytical observer all experience the same mathematical fact: representability is determined by the Yoneda lemma. The absence of perspectival gap is itself diagnostic — it indicates that the constraint is truly a natural law (mountain), not a social arrangement or negotiable institutional choice that different agents experience differently. The uniformity of classification across organized, powerless, and analytical perspectives is the signature of a hard mathematical constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no directionality in the normal sense because there is no extraction or asymmetric benefit. Every perspective applies the canonical d-value for its power atom based on the mathematics itself, not on beneficiary/victim relationships. Powerless agents face d ≈ 1.0 (full target status) in their structural position relative to the constraint, but the constraint's extractiveness χ is so low that even with high f(d), the experienced extraction is minimal. The mathematics itself is the 'agent' — the axioms and theorems are invariant, not negotiable. There is no beneficiary or victim of representable functors as a mathematical fact.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY. The representable functor constraint is a pure mountain — it has no hidden alternative classification that would resolve a mandatrophy. The constraint does not claim to be coordination while secretly extracting (Tangled Rope trap), nor does it claim to be extractive while actually coordinating (Snare trap). The mathematical fact is transparent and unique: representability is what it is. The constraint resolves all ambiguity by its logical necessity. The base properties and all perspectives agree: this is a natural law of category theory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_set_theory_dependence,
    'Does the representability constraint depend on classical set theory and the axiom of choice, or is it invariant across constructive and non-classical foundations?',
    'Formalization in constructive type theory (HoTT), intuitionistic logic, and topoi frameworks; comparison of representability results across foundational systems',
    'If foundational-dependent: representability is conditional (soft mountain). If universal: representability is absolute (hard mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_set_theory_dependence, conceptual, 'Whether representability is foundationally contingent or universal').

omega_variable(
    locally_small_category_necessity,
    'Is the restriction to locally small categories a genuine mathematical limitation or a pragmatic constraint of set-theoretic formalism?',
    'Investigation of representable functors in large categories using Grothendieck universes, class-based foundations, and infinity-categorical analogues',
    'If genuine limitation: representability is truly constrained. If pragmatic: the constraint could be reformulated for larger categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(locally_small_category_necessity, conceptual, 'Whether local smallness is a necessary or pragmatic restriction').

omega_variable(
    yoneda_lemma_computational_content,
    'Beyond the existence claim, what computational or algorithmic content does the Yoneda lemma provide for deciding representability in concrete categories?',
    'Extraction of decision procedures from the Yoneda proof; investigation of representability as a decidable property in specific categories (finite categories, algebraic structures, topological spaces)',
    'If highly decidable: representability becomes an implementable tool. If undecidable in many categories: the constraint remains primarily theoretical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yoneda_lemma_computational_content, empirical, 'Computational decidability of representability in concrete categories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(representable_functors, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repfunc_tr_t0, representable_functors, theater_ratio, 0, 0.05).
narrative_ontology:measurement(repfunc_tr_t50, representable_functors, theater_ratio, 50, 0.08).
narrative_ontology:measurement(repfunc_tr_t100, representable_functors, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(repfunc_be_t0, representable_functors, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(repfunc_be_t50, representable_functors, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(repfunc_be_t100, representable_functors, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(representable_functors, information_standard).
narrative_ontology:affects_constraint(representable_functors, yoneda_embedding).
narrative_ontology:affects_constraint(representable_functors, adjoint_functor_theorem).

% DUAL FORMULATION NOTE:
% Representable functors are a foundational structure upstream of adjoint functors (which are characterized through representability) and the Yoneda embedding (which realizes any category as a full subcategory of presheaves). All three form a constraint family, with representability as the base structural fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
