% ============================================================================
% CONSTRAINT STORY: yoneda_lemma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_yoneda_lemma, []).

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
 *   constraint_id: yoneda_lemma
 *   human_readable: Yoneda Lemma Adherence in Mathematical Research
 *   domain: mathematical_logic/category_theory
 *
 * SUMMARY:
 *   The Yoneda Lemma is a foundational principle in category theory
 *   establishing that an object is completely determined by its relationships
 *   (morphisms) to other objects. Unlike institutional constraints that arise
 *   from incentive structures or power dynamics, Yoneda is a logical
 *   consequence of categorical axioms. It exhibits the hallmark properties of
 *   a Mountain constraint: zero degrees of freedom, universal applicability,
 *   invariance across mathematical observables, and emergence from formal
 *   necessity rather than contingent institutional design. All six
 *   perspectives (powerless student, organized applied researchers,
 *   institutional mathematics departments, analytical logicians,
 *   proof-theoretic constructors, and foundational realists) classify this
 *   constraint identically as Mountain. The constraint's universality across
 *   perspectives, combined with minimal theater ratio (0.15) and
 *   extractiveness (0.12), indicates that Yoneda operates as a natural law
 *   within mathematics rather than as an extraction mechanism.
 *
 * KEY AGENTS:
 *   - Graduate students: Powerless/trapped agents who must learn Yoneda as non-negotiable foundation
 *   - Category theory researchers: Institutional/arbitrage actors who benefit from Yoneda's universality and rigor
 *   - Applied mathematics practitioners: Organized/constrained agents engaging with category theory across domains
 *   - Logic and foundations community: Analytical observers verifying formal necessity of the lemma
 *   - Mathematics departments: Institutional actors maintaining Yoneda as curriculum standard
 *   - Alternative foundations researchers: Analytical agents testing whether Yoneda survives in non-classical frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(yoneda_lemma, 0.12).
domain_priors:suppression_score(yoneda_lemma, 0.03).
domain_priors:theater_ratio(yoneda_lemma, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(yoneda_lemma, extractiveness, 0.12).
narrative_ontology:constraint_metric(yoneda_lemma, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(yoneda_lemma, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(yoneda_lemma, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(yoneda_lemma, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(yoneda_lemma, mountain).
narrative_ontology:human_readable(yoneda_lemma, "Yoneda Lemma Adherence in Mathematical Research").
narrative_ontology:topic_domain(yoneda_lemma, "mathematical_logic/category_theory").

domain_priors:emerges_naturally(yoneda_lemma).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICIAN / UNIVERSAL VIEW (MOUNTAIN) — The Yoneda Lemma is a mathematical necessity, not a contingent institutional constraint. An object's complete determination by its morphisms to other objects follows from formal category theory axioms. No agent can 'exit' or circumvent this principle — it is a law of the categorical framework itself. d=0.72, f(d)≈1.15, σ=1.0 → χ≈0.14. The lemma is invariant across all mathematical observables and proof methodologies.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICS INSTITUTION (MOUNTAIN) — Category theory departments treat Yoneda as a bedrock principle in graduate curriculum and research. No institutional actor disputes its validity or can obtain legitimacy by denying it. Even mathematicians working outside category theory recognize Yoneda's authority in its domain. The constraint emerges naturally from the formal structure of mathematics itself. d=0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.01. Institutional actors benefit from the constraint's universality (it guarantees rigorous foundations), not from extracting value from it.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: GRADUATE STUDENT (MOUNTAIN) — A mathematics graduate student learning category theory cannot work around the Yoneda Lemma. It is a necessary step in understanding how category theory functions. The student is not being 'extracted from' — they are learning a non-negotiable principle of the field. d=0.95, f(d)≈1.42, σ=1.0 → χ≈0.17. Even powerless agents in the domain must accept Yoneda as a foundational truth.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: APPLIED MATHEMATICS COALITION (MOUNTAIN) — Even researchers working in applied domains (machine learning, topological data analysis, homotopy type theory) who engage with category theory cannot circumvent Yoneda. It defines the relationships in their formal framework. The constraint is not negotiable even for organized actors seeking to reframe category theory. d=0.50, f(d)≈0.65, σ=1.2 → χ≈0.10. The constraint is symmetric across all perspectives.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL / PROOF-THEORETIC (MOUNTAIN) — From the perspective of someone formally proving Yoneda Lemma results, the constraint appears as a logical necessity with zero degrees of freedom. Every step in the proof chain follows from categorical axioms. No alternative axiomatization avoids the conclusion that objects are determined by their morphisms. The proof itself IS the constraint. d=0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MATHEMATICAL REALIST / FOUNDATIONAL ANALYSIS (MOUNTAIN) — Even considering alternative foundational systems (classical set theory, intuitionistic logic, homotopy type theory), the principle that objects are determined by their relationships persists. This is not a property of category theory specifically but a deep fact about mathematical identity and structure. The lemma survives across foundational systems, indicating it reflects something essential about mathematics itself. d=0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(yoneda_lemma_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(yoneda_lemma, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(yoneda_lemma, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(yoneda_lemma, ExtMetricName, E),
    domain_priors:suppression_score(yoneda_lemma, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(yoneda_lemma),
    narrative_ontology:constraint_metric(yoneda_lemma, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(yoneda_lemma, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(yoneda_lemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The lemma creates no asymmetric value capture — all agents either benefit equally from having a rigorous foundation or must comply equally with its logical necessity. No actor 'extracts' from another by invoking Yoneda. Suppression (0.03): Minimal. Yoneda operates through logical necessity, not through coercion or restricted alternatives. A mathematician cannot be 'suppressed' into accepting Yoneda — they accept it because the formal derivation is correct. Theater ratio (0.15): Very low. Teaching Yoneda involves genuine explanation of categorical structure; there is minimal performative ritual. The lemma's verification is transparent: students can verify the proof themselves without institutional mediation. Accessibility collapse (0.92): High. The lemma is inaccessible to those without category theory training, but this is not because information is restricted — it is because category theory itself requires prerequisite knowledge. Once the prerequisites are met, Yoneda's logical status is fully transparent. Resistance (0.08): Minimal. The lemma cannot be resisted — it is either true or false in formal mathematics, and its truth has been verified across multiple axiomatic systems.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits NO perspectival gap — all six perspectives classify it identically as Mountain. This is the canonical signature of a natural law constraint in the Deferential Realism system. The powerless student, the institutional department, the applied researcher, the analytical logician, and even the alternative-foundations skeptic all agree: Yoneda Lemma is an inescapable truth within mathematics. The lack of perspectival gap indicates that the constraint emerges from formal necessity, not from institutional power dynamics. Unlike the verification bottleneck example (which produced all six types from different observables), Yoneda produces Mountain across all observables. This invariance is the defining property of a true natural law in the mathematical domain.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive d in the range [0.05, 0.95] but f(d) yields χ ≈ [−0.01, 0.17], consistently indicating low effective extraction. The beneficiary (institutional mathematics) has d≈0.05 (arbitrage/beneficiary → low d), yielding f(d)≈−0.12 (net benefit). The powerless student has d≈0.95 (trapped/victim → high d), yielding f(d)≈1.42, but χ = 0.12 × 1.42 × 1.0 ≈ 0.17 remains minimal because base extractiveness ε is so low. The symmetric observer (analytical/analytical) has d≈0.72, yielding f(d)≈1.15, still producing χ ≈ 0.14. No directionality override is needed — the automatic derivation correctly reflects that Yoneda is not an extraction mechanism for any agent.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: This constraint demonstrates the mandatrophy at its clearest. There is no risk of mislabeling Yoneda as pure extraction (Snare) or pure coordination (Rope) because the structural data are incompatible with those classifications. Extractiveness cannot exceed 0.25 for a Mountain, and Yoneda's ε=0.12 satisfies this gate. Suppression cannot exceed 0.05, and Yoneda's suppression of 0.03 satisfies this gate. Accessibility collapse must exceed 0.85 for a Mountain, and Yoneda's collapse of 0.92 satisfies this gate. Resistance must be ≤ 0.15, and Yoneda's resistance of 0.08 satisfies this gate. The emergence-naturally flag is true. The constraint resolves mandatrophy not through complexity or debate but through categorical necessity: Yoneda IS what natural law looks like in mathematics. The alternative framings (Snare, Rope, Scaffold, Tangled Rope, Piton) are structurally impossible given the metric thresholds and the definition of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_theory_necessity,
    'Is Yoneda Lemma a necessity of category theory axioms or a contingent theorem that could be false in non-cartesian-closed categories?',
    'Formal proof analysis; examination of minimal axiom sets required for Yoneda; comparison across categorical variants (enriched categories, higher categories, ∞-categories)',
    'If necessity: Mountain classification confirmed universally. If contingent: Some categories exhibit non-Yoneda behavior, suggesting Snare or Tangled Rope from certain observables.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_theory_necessity, empirical, 'Whether Yoneda is logically necessary or contingent in category theory').

omega_variable(
    morphism_vs_object_identity,
    'Does the identity of an object truly consist in its morphisms, or do morphisms merely reflect a deeper object-identity that exists independently?',
    'Philosophical analysis of mathematical ontology; examination of whether hom-functor defines or merely describes object identity; comparison with set-theoretic vs categorical foundations',
    'If morphisms define identity: Mountain (constraint is real, not observational). If identity is prior: The constraint is epistemic (a Rope or Scaffold describing our knowledge, not reality).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphism_vs_object_identity, conceptual, 'Whether object identity is constituted by morphisms or merely described by them').

omega_variable(
    computational_irreducibility,
    'In computational/algorithmic mathematics, can we ever fully enumerate all morphisms necessary to reconstruct an object, or is there computational irreducibility?',
    'Computational complexity analysis; construction of examples where morphism enumeration is NP-hard or undecidable; comparison of theoretical vs practical object reconstruction',
    'If irreducibility exists: Yoneda becomes practically false for computable objects (Snare perspective for computational agents). If fully enumerable: Mountain holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_irreducibility, empirical, 'Whether morphism enumeration for object reconstruction is computationally tractable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(yoneda_lemma, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(yoneda_tr_t0, yoneda_lemma, theater_ratio, 0, 0.12).
narrative_ontology:measurement(yoneda_tr_t50, yoneda_lemma, theater_ratio, 50, 0.14).
narrative_ontology:measurement(yoneda_tr_t100, yoneda_lemma, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(yoneda_be_t0, yoneda_lemma, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(yoneda_be_t50, yoneda_lemma, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(yoneda_be_t100, yoneda_lemma, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(yoneda_lemma, information_standard).
narrative_ontology:affects_constraint(yoneda_lemma, categorical_homomorphism_universality).
narrative_ontology:affects_constraint(yoneda_lemma, adjoint_functor_existence).

% DUAL FORMULATION NOTE:
% Yoneda Lemma is foundational to category theory's formal structure. It serves as a mathematical natural law that enables or constrains downstream constraints in categorical mathematics. The lemma's universality means it affects any constraint operating within the categorical framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
