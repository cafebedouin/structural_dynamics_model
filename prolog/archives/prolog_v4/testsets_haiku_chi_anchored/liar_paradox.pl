% ============================================================================
% CONSTRAINT STORY: liar_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liar_paradox, []).

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
 *   constraint_id: liar_paradox
 *   human_readable: The Liar Paradox (Self-Referential Inconsistency)
 *   domain: logic/epistemology
 *
 * SUMMARY:
 *   The liar paradox — the sentence 'This statement is false' — is a
 *   fundamental constraint on self-referential language. It is not discovered
 *   as a property of a particular formalization; it is inevitable in any
 *   system sufficiently expressive to allow a sentence to refer to its own
 *   truth value. The constraint exhibits the hallmark properties of a
 *   mountain: extractiveness ≤ 0.25 (low), suppression ≤ 0.05 (minimal
 *   coercion), accessibility_collapse ≥ 0.85 (high — the paradox is
 *   immediately accessible), resistance ≤ 0.15 (low — the logical proof of
 *   inconsistency is airtight), emerges_naturally = true (the paradox does
 *   not require institutional enforcement; it is a feature of the problem
 *   space itself). All five perspectives classify the paradox as a Mountain,
 *   with consistent χ values in the 0.07–0.14 range. This uniformity is
 *   expected for natural law constraints — they appear as the same type from
 *   all observation points because they are not extraction mechanisms imposed
 *   by one agent on another; they are irreducible limits on what any system
 *   can accomplish.
 *
 * KEY AGENTS:
 *   - Logical Analysts: Agents investigating the structure of formal systems — perceive the paradox as an immutable constraint on self-reference
 *   - Mathematical Logicians: Researchers extending Gödel incompleteness — recognize the paradox as a gateway to deeper theorems about formal limits
 *   - Formal Systems Designers: Engineers building proof systems and type theories — encounter the paradox as a hard ceiling on expressiveness
 *   - Applied Computer Scientists: Practitioners building verification systems — face the paradox as an irreducible limit on automation
 *   - Meta-Theoretical Observers: Philosophers and logicians studying the nature of language — see the paradox as a civilizational constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liar_paradox, 0.12).
domain_priors:suppression_score(liar_paradox, 0.03).
domain_priors:theater_ratio(liar_paradox, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liar_paradox, extractiveness, 0.12).
narrative_ontology:constraint_metric(liar_paradox, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(liar_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liar_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(liar_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liar_paradox, mountain).
narrative_ontology:human_readable(liar_paradox, "The Liar Paradox (Self-Referential Inconsistency)").
narrative_ontology:topic_domain(liar_paradox, "logic/epistemology").

domain_priors:emerges_naturally(liar_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL ANALYST (MOUNTAIN) — Views the liar paradox as a fundamental property of self-reference in formal systems. The paradox is not contingent on implementation, culture, or institutional arrangement — it emerges necessarily from allowing any system to contain a sentence that refers to its own truth value. This is a logical law, not a policy choice. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL LOGICIAN (MOUNTAIN) — Recognizes the liar paradox as a fundamental limit on formal systems. Gödel's work proved that any consistent formal system powerful enough to express arithmetic cannot prove its own consistency. The liar paradox is the intuitive gateway to Gödel incompleteness — a bedrock constraint on what any logical system can accomplish. Attempts to 'escape' via type theory, paraconsistency, or truth-value gaps all confirm rather than refute the underlying principle. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.07.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: FORMAL SYSTEMS DESIGNER (MOUNTAIN) — Any attempt to formalize a language must eventually confront the self-reference problem. Whether you use classical logic, intuitionistic logic, paraconsistent logic, or fuzzy logic, the same structural constraint reappears: you cannot simultaneously allow unrestricted self-reference, classical truth values, and consistency. You must sacrifice one. This is not a bug in any particular formalization — it is a property of the problem space itself. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.11.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: APPLIED COMPUTER SCIENTIST (MOUNTAIN) — Encounters the paradox as a hard ceiling on what verification systems can achieve. Any sufficiently powerful type system or proof checker will eventually face self-referential questions: 'Can this system prove statements about itself?' The answer is structurally constrained by Gödel and Church. You cannot build a complete, consistent verification oracle. This is not a limitation of current tools — it is a limitation of the problem itself. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.12.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 5: META-THEORETICAL OBSERVER (MOUNTAIN) — The liar paradox is not a paradox about truth in any particular domain (mathematics, physics, ethics) — it is a paradox about the structure of language itself when language becomes expressive enough to refer to itself. This is a civilizational constraint: any species, civilization, or intelligence system capable of developing language and formal reasoning will encounter this same limit. It is not discovered — it is inevitable. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liar_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(liar_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liar_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(liar_paradox, ExtMetricName, E),
    domain_priors:suppression_score(liar_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(liar_paradox),
    narrative_ontology:constraint_metric(liar_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(liar_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(liar_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The liar paradox does not extract value from one agent and transfer it to another. It is not an asymmetric institutional arrangement. Rather, it is a property of the logical space — like the speed of light or the incompleteness of arithmetic. All agents, regardless of power or position, face the same constraint. The minimal non-zero value (0.12 rather than 0.00) reflects that formalization itself has a tiny overhead — the paradox requires language, notation, and a notion of truth value to manifest, but once these exist, the paradox is free. Suppression (0.03): Minimal. The paradox has no alternative states and no exit options. Once you allow self-reference, bivalence, and classical logic simultaneously, the paradox follows necessarily from the axioms. There is no coercion required — the constraint is purely logical. Theater (0.15): Very low. No performative element exists. The paradox is what it is; there is no ritual or pretense involved in its manifestation. Over 2000 years (from the ancient Greeks through medieval scholasticism to modern logic), the theater ratio has increased slightly (0.10 → 0.15) only because different formal systems have added layers of technical machinery (type theory, truth-value gaps, paraconsistency) that create some performative framing around the underlying constraint, but the core paradox remains unchanged.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All five perspectives — the logical analyst, the mathematical logician, the formal systems designer, the applied computer scientist, and the meta-theoretical observer — classify the paradox identically as a Mountain. This is the expected behavior for natural law constraints. The paradox does not look like extraction from some viewpoints and coordination from others; it looks like an immutable logical limit from all viewpoints. This uniformity is not a weakness in the indexical framework; it is a strength. It shows that the framework correctly identifies truly universal constraints (mountains) by the invariance of their classification across all observation points.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims. The liar paradox is not an extraction mechanism or a coordination failure. It is a structural property of the logical space. All agents, regardless of power level, exit options, or spatial scope, encounter the same constraint. The directionality d is not meaningful in the context of this constraint because there is no asymmetric relationship between a beneficiary and a victim. The constraint is symmetric: it affects logical systems, formal languages, and all agents who attempt to reason within them equally. This is why all perspectives derive d ≈ 0.65–0.72 (analytical observer canonical fallback) and χ ≈ 0.07–0.14 (low effective extraction, because the mountain floor dampens χ via the schema's all-of gates).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truth_gap_nature,
    'Is the liar paradox best resolved by denying bivalence (classical true/false), introducing truth-value gaps, or accepting paraconsistency (true and false simultaneously)?',
    'Meta-logical analysis of which logical framework best preserves intuitions about classical logic while resolving the paradox; empirical measurement of which framework is most used in applied systems',
    'Different resolutions imply different constraint structures: truth-gaps make the paradox a mountain (unavoidable limit on language); paraconsistency makes it a rope (a coordination mechanism for accepting certain contradictions); type-theory restrictions make it a piton (institutional gatekeeping on what language constructions are allowed). However, the underlying incompleteness phenomenon persists in all frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(truth_gap_nature, conceptual, 'Which logical framework best resolves the self-referential paradox').

omega_variable(
    expressiveness_completeness_tradeoff,
    'Can a formal system be simultaneously fully expressive (able to express all mathematical claims), complete (able to prove all truths), and consistent (unable to derive contradictions)?',
    'Gödel completeness and incompleteness theorems; Church-Turing undecidability results; empirical verification of which properties real proof systems sacrifice',
    'If the answer is ''no'' (Gödel''s result): the liar paradox is a mountain — an irreducible feature of formal language. If future work shows a loophole: the paradox might be a Tangled Rope (a coordination failure masquerading as a logical limit). The consensus (very high confidence) is that Gödel was correct, making the paradox a mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expressiveness_completeness_tradeoff, empirical, 'Whether completeness, expressiveness, and consistency can be simultaneously achieved').

omega_variable(
    natural_language_vs_formal_systems,
    'Is the liar paradox a property of formal logic or a property of the way natural language allows self-reference?',
    'Analysis of whether natural language speakers are actually troubled by ''This statement is false'' in the way logicians are; empirical study of how different cultures/languages handle self-referential statements; comparison of paradox manifestation across formal systems vs natural language use',
    'If formal-only: the paradox is a mountain in logic but a rope (or piton) in natural language use. If natural-language inherent: the paradox is a civilizational mountain affecting all symbolic systems. Current evidence suggests the paradox is a fundamental feature of any language powerful enough to express self-reference, making it a mountain in both contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_language_vs_formal_systems, empirical, 'Whether the paradox is intrinsic to formal systems or to natural language expressiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liar_paradox, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liar_tr_t0, liar_paradox, theater_ratio, 0, 0.1).
narrative_ontology:measurement(liar_tr_t500, liar_paradox, theater_ratio, 500, 0.12).
narrative_ontology:measurement(liar_tr_t2000, liar_paradox, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(liar_be_t0, liar_paradox, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(liar_be_t500, liar_paradox, base_extractiveness, 500, 0.11).
narrative_ontology:measurement(liar_be_t2000, liar_paradox, base_extractiveness, 2000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liar_paradox, information_standard).
narrative_ontology:affects_constraint(liar_paradox, goedel_incompleteness).
narrative_ontology:affects_constraint(liar_paradox, church_turing_undecidability).
narrative_ontology:affects_constraint(liar_paradox, russell_set_paradox).

% DUAL FORMULATION NOTE:
% The liar paradox is upstream in the constraint family of self-referential limits. Gödel incompleteness (ε≈0.08) and Church-Turing undecidability (ε≈0.06) are downstream logical consequences that inherit the paradox's structure. Russell's paradox (ε≈0.10) is a sibling at the same logical depth. All three constraints exhibit the same mountain properties: low extractiveness, minimal suppression, high accessibility collapse, and invariance across perspectives. The network represents formal-logical dependence: the liar paradox is the most direct manifestation; the others are technical elaborations of the same fundamental principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
