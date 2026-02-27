% ============================================================================
% CONSTRAINT STORY: axiom_of_choice_determinacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_axiom_of_choice_determinacy, []).

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
 *   constraint_id: axiom_of_choice_determinacy
 *   human_readable: The Axiom of Choice (AC)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Axiom of Choice is a foundational axiom in set theory asserting that
 *   for any collection of non-empty sets, there exists a function selecting
 *   exactly one element from each set. AC is independent of Zermelo-Fraenkel
 *   set theory (ZF) alone and was proven independent by Gödel (1938,
 *   consistency) and Cohen (1966, independence). Despite its non-constructive
 *   character and long-standing philosophical debate, AC has become
 *   universally adopted in classical mathematics and underlies most theorems
 *   in analysis, topology, algebra, and functional analysis. The constraint
 *   structure is NOT institutional extraction but logical necessity:
 *   mathematicians collectively treat AC as a natural law because (1) it is
 *   logically independent of weaker foundations, (2) it is irreplaceable for
 *   a vast class of theorems, and (3) it is pragmatically indispensable for
 *   classical mathematics. No agent benefits at another's expense; no
 *   suppression enforces adoption; no theater masks function. The axiom's
 *   logical status is invariant across all observers and measurement
 *   methodologies.
 *
 * KEY AGENTS:
 *   - The Logical Structure: The mathematical foundation — AC emerges as a necessary question once infinite collections are introduced. No agent; no extraction.
 *   - Pure Mathematicians: Users of AC (analytical/analytical) — freely choose to invoke AC because it enables powerful theorems. Benefit entirely from its adoption; no cost imposed.
 *   - Mathematical Community: Institutional coordinating entity (institutional/arbitrage) — collectively adopts ZFC as standard foundation. Gain shared language and theorem database; no extraction.
 *   - Constructivists and Intuitionists: Alternative framework advocates (organized/constrained) — can and do develop mathematics without full AC. Small minority; their exit is technically possible but costlier in terms of theorem availability.
 *   - Formal Logic Observers: Analytical perspective (analytical/analytical) — assess the independence result and logical structure. No stake in adoption; purely epistemic position.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(axiom_of_choice_determinacy, 0.12).
domain_priors:suppression_score(axiom_of_choice_determinacy, 0.03).
domain_priors:theater_ratio(axiom_of_choice_determinacy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(axiom_of_choice_determinacy, extractiveness, 0.12).
narrative_ontology:constraint_metric(axiom_of_choice_determinacy, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(axiom_of_choice_determinacy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(axiom_of_choice_determinacy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(axiom_of_choice_determinacy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(axiom_of_choice_determinacy, mountain).
narrative_ontology:human_readable(axiom_of_choice_determinacy, "The Axiom of Choice (AC)").
narrative_ontology:topic_domain(axiom_of_choice_determinacy, "mathematical/logical").

domain_priors:emerges_naturally(axiom_of_choice_determinacy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PURE MATHEMATICS VIEW (MOUNTAIN) — AC is a foundational axiom of set theory, independent of ZF and unprovable from weaker foundations. Its logical status is invariant across all mathematical contexts. Emergence is natural: the structure of infinite choice sets forces the question. No agent benefits or bears cost; no suppression exists. The axiom either holds or does not in a formal system — this is a property of logical entailment, not institutional arrangement.
constraint_indexing:constraint_classification(axiom_of_choice_determinacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL COMMUNITY ADOPTION (MOUNTAIN) — In practice, the mathematical community has overwhelmingly adopted AC as a working axiom despite its non-constructive nature. This adoption is not extractive or coercive — it is pragmatic coordination around a proven powerful tool. AC is treated as a natural law of mathematics because it reliably generates true theorems. Resistance to AC is low (restricted to constructivists and intuitionists, <10% of mathematicians). Accessibility collapse is high: once AC's logical independence is understood, mathematicians cannot 'exit' its reality — they can only choose to use it or not. The constraint here is logical, not institutional.
constraint_indexing:constraint_classification(axiom_of_choice_determinacy, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: WORKING MATHEMATICIAN (MOUNTAIN) — Individual mathematicians experience AC as a constraint of their logical environment, not as an institutional extraction. When proving theorems in analysis, topology, or algebra, AC is ubiquitous (Tychonoff's theorem, Hausdorff maximality lemma, etc.). A mathematician cannot avoid encountering AC-dependent results. The constraint is not suppressive — mathematicians freely choose to invoke AC because it works. The foundational debate (constructivism vs classicism) is resolved by effectiveness, not enforcement. Exit options are 'constrained' because one can restrict to constructive mathematics, but this severely limits theorem availability — the cost is prohibitive, not coercive.
constraint_indexing:constraint_classification(axiom_of_choice_determinacy, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FORMAL LOGIC / INDEPENDENCE (MOUNTAIN) — From the standpoint of formal logic, AC is independent of ZF (Gödel/Cohen). This means: ZF ⊢ AC if and only if ZF is inconsistent (for classical logic). AC is an irreducible additional axiom. This independence is a mathematical fact, not subject to negotiation or institutional pressure. The constraint is the logical structure itself — the space of consistent set theories that either include or exclude AC. No suppression, no extraction, no theater. AC emerges necessarily from asking: 'Given arbitrary non-empty sets, what choice structures exist?'
constraint_indexing:constraint_classification(axiom_of_choice_determinacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: AXIOM SYSTEM COORDINATION (ROPE) — ZFC (Zermelo-Fraenkel + Choice) is pure coordination: mathematicians agree on a common foundation so that theorems stated in one institution are recognizable in another. This agreement has no extraction — mathematicians do not 'lose' to the axiom system; they gain a shared language. The axiom system provides coordination value without coercive overhead. ZF alone is weaker; AC makes it strong enough for most classical mathematics. This is low-extraction coordination, not enforcement. There is no beneficiary (beyond all mathematicians equally); there is no victim.
constraint_indexing:constraint_classification(axiom_of_choice_determinacy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(axiom_of_choice_determinacy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(axiom_of_choice_determinacy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(axiom_of_choice_determinacy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(axiom_of_choice_determinacy, ExtMetricName, E),
    domain_priors:suppression_score(axiom_of_choice_determinacy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(axiom_of_choice_determinacy),
    narrative_ontology:constraint_metric(axiom_of_choice_determinacy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(axiom_of_choice_determinacy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(axiom_of_choice_determinacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. AC does not extract resources or benefits from a subset of agents. Rather, AC is a shared logical foundation that benefits all mathematicians equally. The low value reflects that there is no asymmetric flow of costs or benefits — adoption is purely a coordination mechanism around a proven powerful tool. Suppression (0.03): Minimal. There is no coercion enforcing AC adoption. Constructivists and intuitionists can and do develop alternative mathematics; their exit is expensive (theorem availability decreases) but not forbidden. Suppression would require forbidding alternative frameworks — mathematicians do not do this. Theater ratio (0.15): Very low. AC is not performative. Mathematicians invoke AC precisely where needed for proofs; when an AC-free proof exists, mathematicians prefer it (lower complexity, stronger result). The small non-zero theater reflects minor pedagogical presentation (e.g., introductory mathematics texts sometimes invoke AC informally without detailed discussion) but this is negligible. Accessibility collapse (0.92): Very high. Once the logical independence of AC is understood, mathematicians cannot 'unsee' its necessity for certain theorems. The structure of the infinite forces the question. Resistance (0.08): Very low. The philosophical debate over AC (Brouwer, constructivism) lasted ~60 years; today, >90% of mathematicians work within classical logic where AC is standard. The remaining resistance comes from constructivists, who are a small, organized minority with full exit rights.
 *
 * PERSPECTIVAL GAP:
 *   Unlike the verification bottleneck exemplar, the Axiom of Choice produces MINIMAL perspectival gap. All perspectives classify as Mountain or Rope. The working mathematician sees it as an immutable logical constraint; the formal logician sees logical independence; the axiom system sees pure coordination; the mathematical community sees pragmatic necessity. No perspective experiences AC as extraction, suppression, or enforcement. This uniformity is characteristic of genuine natural laws — the structure of the phenomenon is invariant across all reasonable measurement methodologies and observer positions. The small gap that exists (Rope perspective for axiom systems vs Mountain perspective for pure logic) reflects the distinction between the abstract logical structure (invariant, mountain) and the practical adoption mechanism (coordinated choice, rope). Both are correct; they emphasize different aspects of the same phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   AC has no directionality in the sense of beneficiary/victim dynamics. There is no agent d-value because there is no extraction structure. All agents (mathematicians, logicians, axiom systems) benefit equally from AC or can easily exit to alternative frameworks. The constraint is structural, not relational. The absence of directionality overfitting is itself significant — it confirms that AC is not an extraction mechanism but a logical property.
 *
 * MANDATROPHY ANALYSIS:
 *   AC RESOLVES MANDATROPHY BY BEING GENUINELY NON-EXTRACTIVE. The mandatrophy is the risk that mathematicians have mislabeled a coordinated fiction (voluntary axiom adoption) as a natural law. Resolution: (1) Independence result is proven — AC is irreducible in classical logic. (2) Exit is possible — constructivists and intuitionists can and do develop alternative mathematics. (3) Adoption is pragmatic — mathematicians choose AC because it works, not because they are forced. (4) No suppression — alternative frameworks are published and studied openly. (5) Theater is minimal — AC is invoked precisely where needed. These five facts together constitute a complete resolution: AC is a natural law of logical structure, not a Snare masquerading as a Mountain. The mountain classification is secure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_truth_status,
    'Is the non-constructive nature of AC (existence without construction method) a logical limit or a feature of classical logic that becomes unnecessary in constructive mathematics?',
    'Comparison of theorem density and applicability: constructive mathematics with/without variants of choice principles; empirical assessment of whether constructive mathematics sacrifices essential results',
    'If constructive is genuinely sufficient: AC is a convenience axiom, not a necessity — classification remains mountain but on pragmatic grounds. If constructive mathematics loses essential theorems: AC is a logical necessity for certain theorems — mountain classification is strengthened by indispensability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_truth_status, conceptual, 'Logical status of non-constructiveness in AC').

omega_variable(
    dependent_choice_sufficiency,
    'For practically all mathematical theorems, is the Axiom of Dependent Choice (DC) or other weaker choice variants sufficient, making full AC unnecessary?',
    'Systematic survey of published theorems claiming AC dependency; reclassification with respect to DC and other choice variants; measure of theorems genuinely requiring full AC vs those requiring only weak choice',
    'If DC suffices for most theorems: AC is over-strong, and mathematicians use it out of convenience rather than necessity. Mountain classification remains but with lower necessity floor. If full AC is genuinely needed for significant theorems: AC is indispensable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependent_choice_sufficiency, empirical, 'Whether weaker choice principles suffice for most mathematics').

omega_variable(
    models_without_choice,
    'Do consistent models of ZF without AC (e.g., Solovay''s model) support sufficient mathematics to be considered viable alternatives, or do they degrade mathematical practice unacceptably?',
    'Evaluation of theorem availability and proof accessibility in AC-free models; measure of degradation in mathematical expressiveness; assessment of whether AC-free mathematics remains coherent and useful',
    'If AC-free models are viable alternatives: AC is a choice point, not a natural law. If viable alternatives are severely constrained: AC is effectively necessary. Mountain classification persists but with clarification that ''natural law'' means ''pragmatically indispensable given classical logic,'' not ''logically required.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(models_without_choice, conceptual, 'Viability of mathematics without AC').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(axiom_of_choice_determinacy, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aoc_tr_t0, axiom_of_choice_determinacy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aoc_tr_t50, axiom_of_choice_determinacy, theater_ratio, 50, 0.14).
narrative_ontology:measurement(aoc_tr_t100, axiom_of_choice_determinacy, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(aoc_be_t0, axiom_of_choice_determinacy, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(aoc_be_t50, axiom_of_choice_determinacy, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(aoc_be_t100, axiom_of_choice_determinacy, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(axiom_of_choice_determinacy, information_standard).
narrative_ontology:affects_constraint(axiom_of_choice_determinacy, zorn_lemma).
narrative_ontology:affects_constraint(axiom_of_choice_determinacy, well_ordering_theorem).
narrative_ontology:affects_constraint(axiom_of_choice_determinacy, tychonoff_theorem).

% DUAL FORMULATION NOTE:
% AC is a foundational axiom that structurally supports multiple dependent constraints (Zorn's lemma, well-ordering theorem, Tychonoff's theorem, Hausdorff maximality lemma). These downstream constraints have higher extractiveness values reflecting the empirical contestedness of their consequences (e.g., Tychonoff's infinitary compactness was controversial for topological spaces). AC itself remains a pure mountain; the downstream consequences inherit AC's independence structure but face additional philosophical debates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
