% ============================================================================
% CONSTRAINT STORY: rolle_theorem_equivalence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rolle_theorem_equivalence, []).

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
 *   constraint_id: rolle_theorem_equivalence
 *   human_readable: Rolle's Theorem and the Intermediate Value Theorem Equivalence
 *   domain: mathematics/real_analysis
 *
 * SUMMARY:
 *   Rolle's Theorem and the Intermediate Value Theorem (IVT) represent one of
 *   the purest examples of logical equivalence in mathematics. For any
 *   continuous function f on a closed interval [a,b] with f(a) = f(b),
 *   Rolle's Theorem asserts the existence of at least one point c in (a,b)
 *   where f'(c) = 0. The IVT asserts that for any continuous function on
 *   [a,b], any value between f(a) and f(b) is attained somewhere in (a,b).
 *   These theorems can be derived from each other through finitary logical
 *   steps without invoking any contingent assumptions. The equivalence is not
 *   discovered empirically—it is proved logically. No observer, agent,
 *   institution, or temporal context can change this relationship. The
 *   constraint is the logical structure itself: any valid system of real
 *   analysis that accepts one theorem must accept the other. This is the
 *   definition of a mountain in Deferential Realism: an unchangeable, fixed,
 *   natural law of the domain (in this case, the domain of logical
 *   relationships within mathematics).
 *
 * KEY AGENTS:
 *   - Analytical Observer: Views the constraint as pure logical equivalence (analytical/analytical) — sees the transparent derivation in both directions
 *   - Applied Mathematician: Uses theorems as functional tools (powerful/mobile) — constraint is notational convenience, not limitation
 *   - Theorem Student: Encounters the constraint as part of learning real analysis (moderate/constrained) — constraint binds regardless of understanding level
 *   - Mathematics Department: Institutional actor managing curriculum (institutional/arbitrage) — can choose pedagogical ordering but cannot change logical relationship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rolle_theorem_equivalence, 0.05).
domain_priors:suppression_score(rolle_theorem_equivalence, 0.02).
domain_priors:theater_ratio(rolle_theorem_equivalence, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rolle_theorem_equivalence, extractiveness, 0.05).
narrative_ontology:constraint_metric(rolle_theorem_equivalence, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(rolle_theorem_equivalence, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rolle_theorem_equivalence, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rolle_theorem_equivalence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rolle_theorem_equivalence, mountain).
narrative_ontology:human_readable(rolle_theorem_equivalence, "Rolle's Theorem and the Intermediate Value Theorem Equivalence").
narrative_ontology:topic_domain(rolle_theorem_equivalence, "mathematics/real_analysis").

domain_priors:emerges_naturally(rolle_theorem_equivalence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — Rolle's Theorem and the Intermediate Value Theorem are logically equivalent: either one can be derived from the other through finite steps of valid reasoning within real analysis. This equivalence is not contingent on any empirical observation, institutional arrangement, or temporal context. The logical relationship holds across all possible domains where real-valued continuous and differentiable functions can be defined. Zero degrees of freedom for any observer.
constraint_indexing:constraint_classification(rolle_theorem_equivalence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED MATHEMATICIAN (MOUNTAIN) — Even from the perspective of mathematical practice and application, Rolle's Theorem and IVT maintain their structural equivalence. Any computational problem solvable via one theorem can be reframed using the other. The choice between them is pedagogical or notational convenience, not structural necessity. Both theorems impose the same logical constraints on valid reasoning paths.
constraint_indexing:constraint_classification(rolle_theorem_equivalence, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THEOREM STUDENT (MOUNTAIN) — Even the learner encountering Rolle's Theorem for the first time encounters the same logical structure that an expert does. The theorem's logical form does not change based on the learner's prior knowledge, confidence, or exit options. The constraint is equally binding regardless of whether the student accepts, rejects, or remains uncertain about the proof.
constraint_indexing:constraint_classification(rolle_theorem_equivalence, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICS DEPARTMENT (MOUNTAIN) — From the institutional perspective of mathematics education and research, Rolle's Theorem and IVT equivalence remains invariant. Departments can teach either theorem first, can emphasize different applications, can allocate different textbook pages—but the underlying logical equivalence persists. No institutional decision or resource constraint can change the mathematical relationship.
constraint_indexing:constraint_classification(rolle_theorem_equivalence, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rolle_theorem_equivalence_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(rolle_theorem_equivalence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rolle_theorem_equivalence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rolle_theorem_equivalence, ExtMetricName, E),
    domain_priors:suppression_score(rolle_theorem_equivalence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rolle_theorem_equivalence),
    narrative_ontology:constraint_metric(rolle_theorem_equivalence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rolle_theorem_equivalence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rolle_theorem_equivalence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. The constraint imposes no extraction — it represents a pure logical relationship. There is no asymmetric cost or benefit to any agent from the equivalence holding. All perspectives benefit equally from having a reliable, unchangeable logical law. Suppression (0.02): Negligible. There are no barriers to understanding or utilizing this equivalence. Any agent with basic real analysis literacy can verify the derivation. The constraint does not suppress alternatives or access. Theater ratio (0.10): Very low. Mathematical proofs are straightforward; there is minimal performative content. The derivation of one theorem from the other is transparent and verifiable. Accessibility collapse (0.92): Very high. No external barriers exist to accessing this logical relationship. It is equally accessible to all literate observers. Resistance (0.08): Very low. There is no resistance to this constraint functioning as designed — no stakeholder attempts to circumvent, redefine, or weaken the equivalence. The constraint is purely structural, not institutional.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits NO perspectival gap. All four perspectives (analytical, powerful, moderate, institutional) classify it identically as mountain. This uniformity is the diagnostic signature of a natural law. In extractive or coordinative constraints, different observers perceive different types (snare vs rope vs tangled rope) based on their position in the extraction flow. Here, position is irrelevant. The equivalence is invariant across all (P,T,E,S) tuples. This invariance confirms the mountain classification and demonstrates why mathematical theorems are canonical exemplars of the mountain type.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a uniform-type (mountain-only) constraint with no beneficiary or victim groups. The logical equivalence creates no directional flow of extraction or benefit. All observers, regardless of power level, time horizon, or exit options, experience the same constraint: the logical laws of mathematics are binding. Directionality (d) is undefined for this constraint because there is no asymmetric power relationship. The equivalence holds independent of any agent's preferences, capabilities, or position. This is precisely why it is a mountain — it transcends the typical (P,T,E,S) differentiation that creates perspectival gaps in extractive or coordinative constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: This constraint demonstrates that the mandatrophy is definitionally resolved for mountain-type constraints. The mandatrophy asks: 'Is this constraint coordination (Rope/Tangled Rope/Scaffold) mislabeled as extraction (Snare), or extraction mislabeled as coordination?' For a mountain, this question does not apply. The constraint is neither coordinative nor extractive—it is a logical necessity that transcends such categories. The equivalence between Rolle's Theorem and IVT creates no coordination problem to be solved and extracts nothing from any agent. It is a ceiling that all parties bump against equally. The mandatrophy is vacuously resolved: there is no extractive overlay masquerading as coordination, and no extractive cost hidden in coordination function, because the constraint has neither function. It is pure logical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalization_framework_dependence,
    'Does the equivalence between Rolle''s Theorem and IVT hold across all formal frameworks (classical logic, constructive mathematics, non-standard analysis)?',
    'Examination of proofs in constructive mathematics and non-standard analysis; verification of theorem equivalence under different logical foundations',
    'If framework-dependent: the constraint is contingent on classical real analysis conventions rather than logically immutable. If framework-invariant: mountain classification is confirmed across all rigorous mathematical systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalization_framework_dependence, conceptual, 'Whether equivalence depends on classical logic framework').

omega_variable(
    implicit_finite_dimensionality,
    'Does the equivalence break down when attempting to generalize to infinite-dimensional function spaces or non-Euclidean topologies?',
    'Examination of Rolle-type theorems in Banach spaces, Hilbert spaces, and manifolds; identification of conditions under which the classical equivalence fails',
    'If equivalence fails in generalized spaces: the mountain is framework-local rather than universal. If equivalence persists under appropriate generalization: confirms mountain status across topological contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_finite_dimensionality, empirical, 'Whether equivalence generalizes to infinite-dimensional spaces').

omega_variable(
    pedagogical_asymmetry_illusion,
    'Does the asymmetry in textbook presentation (IVT taught before Rolle''s Theorem) create an illusion of logical dependency that masks the underlying equivalence?',
    'Meta-analysis of textbook orderings; explicit derivation of both directions (IVT→Rolle, Rolle→IVT) in multiple sources; student comprehension studies with reversed presentation order',
    'If pedagogical ordering creates false asymmetry: the perceived constraint on teaching sequence is institutional inertia (Piton), not logical necessity (Mountain). If equivalence remains psychologically obvious in reverse order: mountain classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_asymmetry_illusion, empirical, 'Whether pedagogical presentation creates false asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rolle_theorem_equivalence, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rolle_tr_t0, rolle_theorem_equivalence, theater_ratio, 0, 0.08).
narrative_ontology:measurement(rolle_tr_t100, rolle_theorem_equivalence, theater_ratio, 100, 0.1).
narrative_ontology:measurement(rolle_tr_t200, rolle_theorem_equivalence, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(rolle_be_t0, rolle_theorem_equivalence, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(rolle_be_t100, rolle_theorem_equivalence, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(rolle_be_t200, rolle_theorem_equivalence, base_extractiveness, 200, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rolle_theorem_equivalence, information_standard).
narrative_ontology:affects_constraint(rolle_theorem_equivalence, mean_value_theorem).
narrative_ontology:affects_constraint(rolle_theorem_equivalence, extreme_value_theorem).

% DUAL FORMULATION NOTE:
% Rolle's Theorem is downstream of the more general Mean Value Theorem (of which it is a special case). The family of theorems (EVT → IVT → Rolle ↔ MVT) forms a lattice of logical dependencies. Each theorem has its own extractiveness value reflecting how much contingent institutional arrangement surrounds its teaching and proof structure, but the core equivalence relationships are fixed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
