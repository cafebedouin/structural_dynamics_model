% ============================================================================
% CONSTRAINT STORY: banach_tarski_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_banach_tarski_paradox, []).

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
 *   constraint_id: banach_tarski_paradox
 *   human_readable: Banach-Tarski Paradox
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Banach-Tarski Paradox is a theorem in set theory stating that a solid
 *   ball in three-dimensional Euclidean space can be decomposed into a finite
 *   number of non-measurable point sets, which can then be reassembled into
 *   two identical copies of the original ball. This result, proved in 1924 by
 *   Stefan Banach and Alfred Tarski, has become the canonical exemplar of a
 *   mathematical constraint that appears paradoxical only when abstract set
 *   theory is disconnected from physical geometry. The constraint is the
 *   irreducible gap between the power of set-theoretic decomposition (enabled
 *   by the Axiom of Choice) and the structure of measurable space (continuous
 *   geometry). Under the Deferential Realism framework, the Banach-Tarski
 *   Paradox classifies as Mountain from all perspectives: it is an immutable
 *   consequence of the axioms of mathematics, with no agent possessing
 *   meaningful degrees of freedom to escape, circumvent, or negotiate the
 *   constraint. No beneficiary extracts value from the paradox; no victim
 *   suffers from it. Rather, it is a natural law of formal mathematics
 *   itself.
 *
 * KEY AGENTS:
 *   - Mathematician: Observer within formal set theory (analytical/analytical) — understands the proof as logically valid given ZFC axioms
 *   - Physicist: Seeker of physical meaning (powerless/trapped) — cannot instantiate the decomposition physically; trapped by the gap between abstract mathematics and measurable reality
 *   - Foundationalist: Axiom system designer (institutional/arbitrage) — can choose alternative axiom systems but cannot escape the underlying constraint that some foundational incompleteness exists
 *   - Constructivist: Intuitionistic mathematics practitioner (organized/constrained) — rejects non-constructive axioms, eliminating the paradox within their system, but constrained by irreducible non-computability
 *   - Logician: Student of Gödel's incompleteness (analytical/analytical) — recognizes the paradox as a manifestation of inherent limitations in formal systems
 *   - Applied Mathematician: Engineer and pragmatist (moderate/mobile) — ignores the paradox as irrelevant but remains theoretically constrained by its existence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(banach_tarski_paradox, 0.08).
domain_priors:suppression_score(banach_tarski_paradox, 0.03).
domain_priors:theater_ratio(banach_tarski_paradox, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(banach_tarski_paradox, extractiveness, 0.08).
narrative_ontology:constraint_metric(banach_tarski_paradox, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(banach_tarski_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(banach_tarski_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(banach_tarski_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(banach_tarski_paradox, mountain).
narrative_ontology:human_readable(banach_tarski_paradox, "Banach-Tarski Paradox").
narrative_ontology:topic_domain(banach_tarski_paradox, "mathematical/logical").

domain_priors:emerges_naturally(banach_tarski_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICIAN / FORMAL SET THEORY (MOUNTAIN) — Within ZFC axioms and the assumption of the Axiom of Choice, the Banach-Tarski Paradox is a proven theorem. No escape: the decomposition is logically valid given the axioms. The constraint is that free decomposition of point sets into non-measurable subsets is irreducible to the structure of 3D space under standard axioms. d≈0.00, f(d)≈-0.20 (pure observer), χ≈-0.002. Zero degrees of freedom.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PHYSICIST / PHYSICAL REALIZABILITY (MOUNTAIN) — The paradox reveals an irreducible gap between mathematical set theory and physical reality: point sets lack physical measurability, making physical decomposition impossible. This is not a constraint the physicist can escape — it is an intrinsic property of the boundary between abstract mathematics and physical geometry. The Axiom of Choice itself generates non-measurable sets that cannot be physically instantiated. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.11. Trapped by logical necessity, but even maximum directionality yields minimal extraction because the underlying ε is so low.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: FOUNDATIONALIST / AXIOM SYSTEM (MOUNTAIN) — Different axiom systems (ZFC, ZF without Choice, constructive mathematics, type theory) yield different 'paradoxes'. In systems rejecting the Axiom of Choice or requiring constructibility, Banach-Tarski decomposition does not exist. Yet the Foundationalist cannot arbitrage this: the choice of axiom system is itself determined by deeper commitments about what mathematics *is*. Within any chosen system, the paradox is immutable. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.0008. Appears as beneficiary (arbitrage to other axiom systems) but the constraint (the paradox itself) is still mountain within each system.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: CONSTRUCTIVIST / INTUITIONISTIC MATHEMATICS (MOUNTAIN) — Constructivists reject the Axiom of Choice and non-constructive proofs, eliminating Banach-Tarski. But they cannot escape the underlying constraint: the incompleteness and irreducibility that *generates* the paradox remains. In constructive systems, it manifests as non-computability and undecidability. The Constructivist trades one irreducible constraint (AC-based paradox) for another (Gödel's limits on constructivity). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.06. Constrained by the foundational structure of formal mathematics itself.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 5: LOGICIAN / GÖDEL'S INCOMPLETENESS (MOUNTAIN) — The Banach-Tarski Paradox is a manifestation of Gödel's incompleteness: any axiom system powerful enough to do arithmetic admits unprovable truths and undecidable statements. The Axiom of Choice creates non-measurable sets, which create the decomposition. The logician sees this as an irreducible feature of formal systems themselves — not a problem to solve but a structural necessity. No exit. d≈0.00, f(d)≈-0.20, σ=1.0 → χ≈-0.002. Pure observer recognizing necessity.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: APPLIED MATHEMATICIAN (MOUNTAIN) — The Banach-Tarski Paradox is irrelevant to physical engineering: continuous functions, measurable sets, and constructive algorithms work fine for real applications. The Applied Mathematician is 'mobile' in the sense of ignoring abstract set theory entirely. Yet the constraint still applies: the theoretical gap between physical continuity and discontinuous non-measurable decomposition is irreducible. Escaping into pragmatism does not resolve the mathematical constraint. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.08. Mobile but constrained by mathematical reality anyway.
constraint_indexing:constraint_classification(banach_tarski_paradox, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(banach_tarski_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(banach_tarski_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(banach_tarski_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(banach_tarski_paradox, ExtMetricName, E),
    domain_priors:suppression_score(banach_tarski_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(banach_tarski_paradox),
    narrative_ontology:constraint_metric(banach_tarski_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(banach_tarski_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(banach_tarski_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. The Banach-Tarski Paradox is a pure logical constraint with no extraction mechanism. No agent benefits from the paradox; no agent bears costs from it in the economic or political sense. The low value reflects that this is a constraint on what can be proved, not a constraint on how value is distributed. The minimal non-zero value (0.08 rather than 0.00) accounts for the slight labor cost to mathematicians of learning the proof and the conceptual cost to physicists of integrating the result into their worldview. Suppression (0.03): Near-zero. The paradox is not suppressed by any coercive apparatus. It exists as a logical truth, accessible to anyone with enough mathematical sophistication. The minimal suppression reflects only the genuine difficulty (not coercion) of understanding the proof. Theater (0.15): Minimal. The proof is substantive — each step follows from axioms and logical rules. The slight non-zero value reflects that the paradox's emotional impact ('two balls from one') exceeds its technical content, making it somewhat theatrical in presentation. The stability over the 100-unit interval reflects that the paradox's logical status has not changed since 1924 — it remains a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   All six perspectives converge on Mountain classification, which is unusual for the Deferential Realism framework. This uniformity is correct: the Banach-Tarski Paradox is a uniform-type constraint (see Corpus Balance Guidance: Mountain-only, Natural Law section). However, the *experience* of the constraint differs radically across perspectives: (1) The Mathematician experiences relief — the paradox is *explained* by the Axiom of Choice, closing the mystery. (2) The Physicist experiences crisis — the paradox reveals a gulf between abstract and physical mathematics that cannot be bridged. (3) The Foundationalist experiences contingency — they can choose to avoid the paradox by adopting a weaker axiom system, but this choice is itself constrained by the impossibility of getting classical mathematics without the full power of AC. (4) The Constructivist experiences trade-off — they resolve the Banach-Tarski problem but face equal non-computability constraints. (5) The Logician experiences necessity — the paradox is an instance of Gödel's incompleteness, which no system can escape. (6) The Applied Mathematician experiences irrelevance — the paradox does not affect their work, yet they remain theoretically constrained by it. The perspectival gap is not in classification type (all Mountain) but in the *meaning* attributed to the constraint from different positions within mathematics and its applications.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is minimally meaningful for this constraint because there is no structural extraction. Every agent is roughly a pure observer: d≈0.0–0.5 across perspectives, reflecting that no agent benefits and no agent bears costs in the distributional sense. The Axiom of Choice creates the non-measurable sets, so in a strict sense the Foundation that adopts AC is a 'beneficiary' of having powerful axioms (d≈0.15, arbitrage), while the Physicist is a 'victim' of the gap between theory and reality (d≈0.95, trapped). But these are metaphorical: the Physicist is not harmed by the paradox; they are merely confronted with a limit to their explanatory reach. The low χ values across all perspectives (ranging from -0.002 to 0.11) confirm that extraction is negligible. This is mathematically healthy: a logical constraint should exert force through necessity, not through asymmetric benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN-ONLY EXEMPLAR: The Banach-Tarski Paradox resolves the mandatrophy by being a genuine Mountain, not a mislabeled Snare masquerading as a law. The test: does it exhibit the mountain gate metrics? Accessibility collapse (0.92 ≥ 0.85) ✓ — the paradox becomes less mysterious once you understand AC; it 'collapses' from paradoxical to provable. Resistance (0.08 ≤ 0.15) ✓ — no one resists the logic; the logic is irresistible. Emerges naturally (true) ✓ — the paradox emerges directly from the axioms without additional assumptions. Extractiveness (0.08 ≤ 0.25) ✓ — no rent-seeking or asymmetric benefit. Suppression (0.03 ≤ 0.05) ✓ — the proof is freely available, not hidden. The mandatrophy is resolved in the affirmative: this is a real Mountain, not a Snare pretending to be inevitable. The uniformity across perspectives (all Mountain, no perspectival gap in classification type) is the signature of a true natural law of mathematics. When Banach-Tarski is taught in graduate mathematics, it teaches students that some constraints are pure logical necessity — not institutions, not coordination failures, not extraction mechanisms, but irreducible facts about what can be proved within a formal system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    axiom_of_choice_necessity,
    'Is the Axiom of Choice a necessary truth or a contingent mathematical assumption? Does ZFC capture the ''correct'' foundation of mathematics?',
    'Philosophical and mathematical consensus on foundations; empirical applications testing whether AC-free mathematics suffices; interdependency analysis showing whether specific mathematical results require AC or can be proved constructively',
    'If AC is necessary: Banach-Tarski is a universal constraint within any complete mathematics. If AC is contingent: the paradox is an artifact of a particular axiom system choice, reducing its status from universal law to mathematical convention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(axiom_of_choice_necessity, conceptual, 'Whether the Axiom of Choice is necessary for mathematics').

omega_variable(
    physical_instantiation_boundary,
    'Is the boundary between non-measurable abstract sets and measurable physical geometry itself a law of physics, or merely a contingent feature of measurement apparatus?',
    'Quantum mechanics investigations of measurement at Planck scale; development of quantum set theory; experimental tests of whether Planck-scale decomposition manifests paradoxical properties',
    'If measurability gap is fundamental: Banach-Tarski marks an irreducible boundary in nature. If contingent: quantum systems might exhibit measurability properties violating classical assumptions, suggesting the paradox dissolves at sufficiently small scales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_instantiation_boundary, empirical, 'Whether the measurability boundary is a law of physics or contingent measurement fact').

omega_variable(
    model_theoretic_universality,
    'Does the Banach-Tarski Paradox hold across all mathematical models satisfying ZFC axioms, or does its force depend on specific model properties like cardinality or dimension?',
    'Model-theoretic analysis of paradox in different set-theoretic models; investigation of whether the paradox survives in hyperfinite or non-standard models; dimensional analysis showing whether paradox behavior changes in higher dimensions or transfinite spaces',
    'If universal across models: the paradox is a deep structural necessity of powerful axiom systems. If model-dependent: the paradox''s force is contingent, and alternative models might avoid it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(model_theoretic_universality, empirical, 'Whether the paradox holds universally across ZFC models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(banach_tarski_paradox, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btp_tr_t0, banach_tarski_paradox, theater_ratio, 0, 0.12).
narrative_ontology:measurement(btp_tr_t50, banach_tarski_paradox, theater_ratio, 50, 0.14).
narrative_ontology:measurement(btp_tr_t100, banach_tarski_paradox, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(btp_be_t0, banach_tarski_paradox, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(btp_be_t50, banach_tarski_paradox, base_extractiveness, 50, 0.075).
narrative_ontology:measurement(btp_be_t100, banach_tarski_paradox, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(banach_tarski_paradox, information_standard).
narrative_ontology:affects_constraint(banach_tarski_paradox, axiom_of_choice_necessity).
narrative_ontology:affects_constraint(banach_tarski_paradox, godel_incompleteness_first).
narrative_ontology:affects_constraint(banach_tarski_paradox, measurability_gap_quantum).

% DUAL FORMULATION NOTE:
% The Banach-Tarski Paradox exists in a constraint family alongside Gödel's Incompleteness Theorem and the Axiom of Choice. Gödel's incompleteness (ε≈0.05, Mountain) is upstream: it establishes that sufficiently powerful formal systems admit undecidable propositions. The Axiom of Choice (ε≈0.06, Mountain) is a sibling: it is the specific axiom that generates the non-measurable sets underlying the paradox. The Measurability Gap (ε≈0.25, Piton) is downstream and more contingent: whether the physical world can instantiate the decomposition depends on physics, not pure mathematics. All three are linked: AC depends on Gödel's framework (any proof of AC is within a formal system); Banach-Tarski depends on AC; the gap between mathematics and physics depends on whether physical reality instantiates measurability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
