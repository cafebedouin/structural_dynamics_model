% ============================================================================
% CONSTRAINT STORY: russells_paradox_self_reference
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russells_paradox_self_reference, []).

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
 *   constraint_id: russells_paradox_self_reference
 *   human_readable: Russell's Paradox (Naive Set Theory Collapse)
 *   domain: mathematical_logic/foundational_mathematics
 *
 * SUMMARY:
 *   Russell's Paradox (1901) is the definitive mathematical mountain: a
 *   logical impossibility that no axiom choice, institutional arrangement, or
 *   resource allocation can circumvent. The paradox arises from a
 *   straightforward formal question—does the set R of all sets not containing
 *   themselves contain itself?—and delivers an irreducible contradiction: if
 *   R ∈ R, then R ∉ R (by definition of R); if R ∉ R, then R ∈ R (by
 *   definition of R). This is not an empirical surprise, a policy failure, or
 *   a coordination problem. It is a structural impossibility at the
 *   foundations of logical reasoning itself. The constraint exhibits zero
 *   extractiveness (ε=0.08), zero suppression (suppression=0.03), and
 *   near-zero theater (theater_ratio=0.15). The low theater ratio reflects
 *   that the paradox has no performative element—it is purely structural,
 *   with no ritualistic or proxy-goal content. No agent benefits from the
 *   paradox, and no agent can leverage it for extraction. All perspectives
 *   classify it identically as Mountain, confirming the uniform-type gate
 *   condition.
 *
 * KEY AGENTS:
 *   - Formal Logicians: Analytical perspective (analytical/analytical) — recognize the paradox as a theorem within any system claiming unrestricted comprehension
 *   - Mathematical Community: Institutional perspective (institutional/arbitrage) — must choose a resolution (ZFC, type theory, category theory) but cannot avoid the constraint itself
 *   - Platonist Philosophers: Conceptual perspective (analytical/civilizational) — see the paradox as a law of abstract existence, not artifact of our axioms
 *   - Model Theorists: Technical perspective (analytical/civilizational) — prove that no model can satisfy both unrestricted comprehension and extensionality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russells_paradox_self_reference, 0.08).
domain_priors:suppression_score(russells_paradox_self_reference, 0.03).
domain_priors:theater_ratio(russells_paradox_self_reference, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russells_paradox_self_reference, extractiveness, 0.08).
narrative_ontology:constraint_metric(russells_paradox_self_reference, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(russells_paradox_self_reference, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(russells_paradox_self_reference, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(russells_paradox_self_reference, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russells_paradox_self_reference, mountain).
narrative_ontology:human_readable(russells_paradox_self_reference, "Russell's Paradox (Naive Set Theory Collapse)").
narrative_ontology:topic_domain(russells_paradox_self_reference, "mathematical_logic/foundational_mathematics").

domain_priors:emerges_naturally(russells_paradox_self_reference).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORMAL LOGICIAN (MOUNTAIN) — From the universal perspective of logical structure, Russell's Paradox is an irreducible contradiction inherent to unrestricted set comprehension. The paradox emerges necessarily from the axiom 'for any property P, the set {x : P(x)} exists.' This is a logical limit, not an empirical or institutional constraint. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. The constraint cannot be circumvented by policy or resource allocation; it is resolved only by restricting the axiom itself.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL COMMUNITY (MOUNTAIN) — From the perspective of working mathematicians and logicians, Russell's Paradox is a fixed boundary of naive set theory. The community cannot extract value by ignoring the paradox; instead, it must choose among alternative axiomatizations (ZFC, type theory, category theory). Each choice solves the paradox but enforces different foundational commitments. The constraint is immutable: any claim to use unrestricted comprehension will be rejected. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Negative effective extraction indicates the paradox imposes a cost on all, equally.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATONIST PHILOSOPHER (MOUNTAIN) — From the Platonic view, Russell's Paradox is a structural impossibility: there is no possible world, abstract or concrete, in which the set R = {x : x ∉ x} can exist. The paradox reflects a fundamental constraint on what can be an object — not an artifact of our axiom choice but a law of abstract existence. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MODEL THEORIST (MOUNTAIN) — From the model-theoretic perspective, no model of naive set theory can satisfy both the axiom of unrestricted comprehension and the axiom of extensionality simultaneously. This is a theorem, not a negotiable property. Any attempted model either fails to include R or fails to satisfy the self-reference condition. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(russells_paradox_self_reference, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russells_paradox_self_reference_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(russells_paradox_self_reference, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russells_paradox_self_reference, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, ExtMetricName, E),
    domain_priors:suppression_score(russells_paradox_self_reference, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(russells_paradox_self_reference),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(russells_paradox_self_reference, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(russells_paradox_self_reference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Russell's Paradox extracts nothing and prevents nothing selectively—it applies equally to all who claim unrestricted set comprehension. No agent can hide behind the paradox, and no agent receives benefit from it. The constraint is a universal gate: it must be resolved, but the resolution distributes its costs symmetrically across the mathematical community. Suppression (0.03): Minimal. There are no hidden alternatives or coercive suppression of exit options. The mathematical community openly debated alternatives (ZFC, type theory, constructive approaches) and chose among them transparently. No agent is forced to use naive set theory; the paradox simply makes that choice untenable. Theater ratio (0.15): Near-zero. The paradox contains no performative or proxy-goal content. Its statement is purely structural: 'Let R = {x : x ∉ x}. Does R ∈ R?' The answer is deterministic and exhibits no gap between appearance and function. The small non-zero value reflects minor notational conventions (how we write 'x ∉ x') that do not affect the core contradiction.
 *
 * PERSPECTIVAL GAP:
 *   Remarkably, this constraint produces zero perspectival gap across all observation points. Every perspective—the formal logician, the working mathematician, the Platonist, the model theorist—arrives at the same classification (Mountain) and the same effective extraction (χ ≈ 0.09). This uniformity is the hallmark of a true natural law. The constraint is invariant across all indexical tuples (P, T, E, S). The absence of gap indicates that no observer has a structural position that would allow them to misread the paradox as coordination (Rope), extraction (Snare), or temporary (Scaffold). The paradox presents the same barrier to all inquirers, making it the canonical exemplar of a mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive d ≈ 0.72 (analytical observer canonical value), yielding f(d) ≈ 1.15 and χ ≈ ε × f(d) × σ ≈ 0.08 × 1.15 × 1.0 ≈ 0.09. The directionality is unambiguous because there is no beneficiary/victim structure: the constraint applies equally to all. No agent is the target of extraction; the paradox is not a mechanism for extracting from one group and benefiting another. The constraint is purely structural—a limitation on what can logically exist, not a social arrangement that redistributes resources or opportunities. Beneficiaries and victims are absent by definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comprehension_principle_necessity,
    'Is the axiom of unrestricted comprehension a necessary logical principle or a contingent choice?',
    'Investigate whether intuitive set formation — collecting objects by a property — can be reinterpreted without assuming comprehension. Examine whether type-theoretic or categorical reconstructions truly avoid the principle or merely hide it in different form.',
    'If comprehension is necessary: Russell''s Paradox is a law of logic (Mountain). If contingent: the paradox reflects a failed axiom choice, not a structural limit. Classification remains Mountain either way (ε and suppression stay stable), but the philosophical interpretation shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(comprehension_principle_necessity, conceptual, 'Whether unrestricted comprehension is logically necessary or contingent').

omega_variable(
    self_reference_elimination,
    'Can a formal system eliminate self-referential properties entirely while preserving expressive power?',
    'Compare expressiveness limits of stratified set theory, type theory, and category theory. Identify mathematical questions answerable in ZFC but not in type-theoretic frameworks. Assess whether ''fixed-point'' constructions (like recursive function definitions) reintroduce self-reference in disguised form.',
    'If self-reference is unavoidable: the paradox is truly structural. If it can be engineered out: the paradox might be a contingent feature of the axiom formulation, not a law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_reference_elimination, empirical, 'Whether self-reference can be fully eliminated from formal systems').

omega_variable(
    modal_status_of_paradox,
    'Is Russell''s Paradox necessary (true in all possible logical systems) or merely metaphysically necessary (true for any system that uses sets as we conceive them)?',
    'Examine whether paraconsistent logics or substructural logics that violate classical assumptions (law of excluded middle, contraction) can accommodate unrestricted comprehension without paradox. Assess whether such systems are genuine alternatives or merely artificial frameworks that dissolve the problem by changing the meaning of core terms.',
    'If truly necessary: Mountain (universal scope). If merely necessary under classical logic: the classification depends on scope (universal vs institutional). If resolvable in alternative logic: might downgrade to Piton (theater) in some institutional contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modal_status_of_paradox, conceptual, 'Modal status of paradox across different logical systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russells_paradox_self_reference, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(russ_tr_t0, russells_paradox_self_reference, theater_ratio, 0, 0.1).
narrative_ontology:measurement(russ_tr_t25, russells_paradox_self_reference, theater_ratio, 25, 0.15).
narrative_ontology:measurement(russ_tr_t50, russells_paradox_self_reference, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(russ_be_t0, russells_paradox_self_reference, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(russ_be_t25, russells_paradox_self_reference, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(russ_be_t50, russells_paradox_self_reference, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russells_paradox_self_reference, information_standard).
narrative_ontology:affects_constraint(russells_paradox_self_reference, cantors_diagonal_self_similarity).
narrative_ontology:affects_constraint(russells_paradox_self_reference, halting_problem_undecidability).
narrative_ontology:affects_constraint(russells_paradox_self_reference, godel_incompleteness_limit).

% DUAL FORMULATION NOTE:
% Russell's Paradox is the prototype of self-referential logical impossibilities. It influences (is upstream of) other mathematical mountains involving diagonalization and self-reference. Cantor's diagonal argument uses the same structural technique (define a set/object that contradicts its own definition); the Halting Problem and Gödel's Incompleteness rely on similar self-referential constructions. These are distinct constraints with their own ε values but share the family structure of self-referential limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
