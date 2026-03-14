% ============================================================================
% CONSTRAINT STORY: axiom_of_choice_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_axiom_of_choice_dependency, []).

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
 *   constraint_id: axiom_of_choice_dependency
 *   human_readable: Axiom of Choice Dependency in Set-Theoretic Foundations
 *   domain: mathematical_logic/set_theory
 *
 * SUMMARY:
 *   The Axiom of Choice (AC) is a foundational principle in set theory
 *   asserting that for any collection of non-empty sets, there exists a
 *   function that selects one element from each set. Gödel (1938) and Cohen
 *   (1963) proved that AC is independent of the Zermelo-Fraenkel axioms (ZF)
 *   — it cannot be proven or disproven from ZF alone. This independence
 *   establishes that AC is a contingent foundational choice, not a logical
 *   necessity. Yet AC has become overwhelmingly dominant in mainstream
 *   mathematics (ZFC = ZF + AC), embedding itself as an assumed background
 *   rather than a consciously chosen axiom. The constraint story examines
 *   whether AC dependency represents a genuine logical immutability
 *   (Mountain) or an institutional convention with alternatives (Rope, Piton,
 *   or Tangled Rope depending on perspective and timescale). The key tension:
 *   formal logic says AC is contingent; institutional mathematics treats AC
 *   as inevitable; constructive mathematics operates without AC; practical
 *   mathematics often deploys AC more symbolically than functionally.
 *
 * KEY AGENTS:
 *   - Mathematical Foundations Community: Institutional actor (institutional/arbitrage) — maintains ZFC as default framework; benefits from AC's unifying power
 *   - Constructive Mathematicians: Organized agents (moderate/mobile) — work explicitly without AC; demonstrate viable alternatives
 *   - Applied Mathematicians: Powerful actors (powerful/mobile) — deploy AC in theorems but rely on algorithmic content that bypasses it
 *   - Logical Truth (formal standpoint): Analytical observer (analytical/analytical) — AC is undecidable within ZF; neither provable nor disprovable
 *   - Mathematical Practice (institutional standpoint): Institutional power (institutional/arbitrage) — AC is treated as foundational despite being axiomatic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(axiom_of_choice_dependency, 0.12).
domain_priors:suppression_score(axiom_of_choice_dependency, 0.03).
domain_priors:theater_ratio(axiom_of_choice_dependency, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(axiom_of_choice_dependency, extractiveness, 0.12).
narrative_ontology:constraint_metric(axiom_of_choice_dependency, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(axiom_of_choice_dependency, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(axiom_of_choice_dependency, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(axiom_of_choice_dependency, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(axiom_of_choice_dependency, mountain).
narrative_ontology:human_readable(axiom_of_choice_dependency, "Axiom of Choice Dependency in Set-Theoretic Foundations").
narrative_ontology:topic_domain(axiom_of_choice_dependency, "mathematical_logic/set_theory").

domain_priors:emerges_naturally(axiom_of_choice_dependency).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / ZFC PERSPECTIVE (MOUNTAIN) — From the formal logic standpoint, AC is an axiom: a foundational assumption. Gödel-Cohen independence results establish that AC is neither provable nor disprovable from ZF alone. This is a logical immutability, not contingent policy. Any mathematical system either includes AC (ZFC) or excludes it (ZF, constructive mathematics), and the choice is axiomatic. No agent can exit; the constraint is universally binding on whatever foundational framework is chosen. Zero degrees of freedom.
constraint_indexing:constraint_classification(axiom_of_choice_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICIAN IN ZFC (MOUNTAIN) — For those working within ZFC, AC is immutable. The theorems proven, the structures available, and the proof techniques possible all assume AC. At the civilizational scale, the constraint is unchangeable: one cannot selectively apply or revoke AC mid-proof. Even mathematicians with mobility (they could switch to constructive math) perceive this as a natural law within their chosen framework.
constraint_indexing:constraint_classification(axiom_of_choice_dependency, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL FOUNDATIONS COMMUNITY (MOUNTAIN) — Institutional mathematics (proof textbooks, graduate curricula, published theorems in mainstream journals) overwhelmingly assumes ZFC + AC. The institutional structure embeds AC as foundational. Even though logicians know alternatives exist (ZF without AC, constructive mathematics), the institutional inertia makes AC appear immutable at the generational timescale. Switching frameworks would require rewriting every dependent theorem — effectively impossible at scale.
constraint_indexing:constraint_classification(axiom_of_choice_dependency, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTRUCTIVE MATHEMATICIAN (ROPE) — Mathematicians working in intuitionistic or constructive logic can explicitly reject AC and build alternative frameworks. They see AC as a contingent choice, not a law. Their biographical timescale allows framework switching; their global connectivity means they can access the full constructive literature. They perceive the constraint as coordination (defining a shared foundation) rather than extraction. Suppression is low — the choice is fully articulated and transparent.
constraint_indexing:constraint_classification(axiom_of_choice_dependency, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PRAGMATIC APPLIED MATHEMATICIAN (PITON) — For working mathematicians in applied fields (optimization, numerical analysis, computational geometry), AC is largely invisible and vestigial. AC appears in existence proofs but rarely in actual algorithms or computational methods. The theorems citing AC persist through institutional inertia, but the functional dependence is often theatrical — the theorem could be strengthened to constructive or made algorithmic without losing practical utility. Theater ratio is high because AC is asserted more often than needed.
constraint_indexing:constraint_classification(axiom_of_choice_dependency, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(axiom_of_choice_dependency_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(axiom_of_choice_dependency, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(axiom_of_choice_dependency, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(axiom_of_choice_dependency, ExtMetricName, E),
    domain_priors:suppression_score(axiom_of_choice_dependency, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(axiom_of_choice_dependency),
    narrative_ontology:constraint_metric(axiom_of_choice_dependency, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(axiom_of_choice_dependency, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(piton_threshold) :-
    domain_priors:theater_ratio(axiom_of_choice_dependency, TR),
    TR >= 0.70.

:- end_tests(axiom_of_choice_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. AC is not extractive in the classical sense — it does not redistribute resources or generate asymmetric advantage. The base value reflects that AC does impose a foundational choice that may foreclose certain mathematical directions (e.g., constructive approaches). The low score reflects that the constraint is primarily logical immutability rather than extractive mechanism. Suppression (0.03): Minimal. AC is transparently stated as an axiom; there is no coercion or hidden mechanism. Alternatives are known and articulated; practitioners can choose frameworks. Theater ratio (0.15): Low. The performance of AC is modest — mathematicians state AC is assumed, and alternatives are available in principle. The slight increase over time reflects that AC invocations in published proofs may have become more ceremonial as AC becomes more backgrounded (newer generations learn ZFC without explicit AC awareness). Accessibility collapse (0.91): Very high. AC creates a sharp dichotomy: one either assumes AC (gaining access to powerful theorems, standard references, institutional support) or rejects it (gaining philosophical consistency but losing institutional resources). The collapse represents the binary institutional gate — no gradualism.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the formal-logical perspective and the institutional perspective. From formal logic, AC is contingent — Gödel-Cohen independence proves it is neither necessary nor derivable. From institutional mathematics, AC is necessary — every standard textbook, every graduate curriculum, every mainstream journal assumes ZFC. The Constructive Mathematician sees the gap explicitly and chooses to work on the ZF side. The Applied Mathematician is largely indifferent — AC appears in existence proofs but not in algorithms. The Pragmatic Mathematician sees AC as increasingly vestigial (piton). The Analytical Observer recognizes AC as a logical immutability (mountain) from the standpoint of civilizational mathematics — there is no escape velocity, because any consistent mathematical system must either include AC or exclude it, and this binary is itself unchangeable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is constrained because there is no obvious beneficiary or victim. AC does not extract value from one agent to another; it distributes choice uniformly across the mathematical community. The institutional beneficiary (mainstream mathematics) is stable; the constructive alternative exists but is a minority practice. Beneficiaries and victims are not present in base_properties because there is no structural extraction — AC is not enforced asymmetrically. Instead, each perspective experiences AC as either foundational (Mountain) or contingent (Rope/Piton) based on their temporal and spatial scope. The Pragmatic Mathematician's piton classification does indicate mild extraction — the ceremony of AC invocation in theorems that don't need it is a form of institutional theater that requires effort without functional return.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that AC is a natural law within mathematics as a formal system, not an institutional constraint. The Gödel-Cohen result is not ambiguous — AC is provably independent of ZF. This means AC is not a snare (extraction via information asymmetry) or tangled rope (mixed coordination and extraction). It is a foundational choice that appears immutable because any consistent system must make it. However, the piton perspective (Pragmatic Mathematician) hints at degradation: AC has become so backgrounded that it is invoked ceremonially in theorems that could be constructivized. This is not a sign that AC is immutable; it is a sign that AC has become institutionally embedded despite being logically contingent. The mountain classification is therefore a false summit if interpreted as 'AC is inherent to mathematics'; it is accurate only if interpreted as 'AC is an immutable foundational choice once you commit to ZFC.' The constructive mathematics perspective reveals this: AC is immutable in ZFC but contingent at the level of foundational choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    logical_vs_practical_dependence,
    'Is the observed dependence on AC a deep logical necessity or a pedagogical convenience?',
    'Systematic review of major theorems: reclassify each as (a) essentially requires AC for logical reasons, (b) can be constructivized with effort, or (c) has algorithmic content that bypasses AC entirely. Measure the fraction in each category.',
    'If category (a) is small: AC is more vestigial than foundational, and the mountain classification softens to piton. If category (a) is large: AC is genuinely foundational and the mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(logical_vs_practical_dependence, empirical, 'Whether AC dependency is logical or pedagogical').

omega_variable(
    alternative_foundation_viability,
    'Can mathematics of equivalent descriptive power be built without AC, and if so, at what complexity cost?',
    'Comparative analysis: prove key theorems in both ZFC and ZF-without-AC, measuring proof complexity, auxiliary lemmas, and notational overhead. Survey constructive mathematics literature for equivalent results.',
    'If equivalent power with low cost: AC is coordinate, and Rope classification is stronger. If high cost: AC captures deep asymmetry, and Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_foundation_viability, empirical, 'Whether equivalent mathematics can be built without AC').

omega_variable(
    institutional_ac_deployment,
    'What fraction of published proofs actually invoke AC versus merely assert frameworks that permit it?',
    'Corpus analysis of published proofs in analysis, topology, and algebra: flag invocations of AC, Zorn''s lemma, well-ordering, and equivalents; measure density per field and publication era.',
    'If fraction is low: AC is performative (theater_ratio rises toward Piton). If high: AC is genuinely deployed (Mountain classification holds). Trend over time indicates whether AC dependency is increasing (Snare trajectory) or decreasing (Piton trajectory).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_ac_deployment, empirical, 'Actual deployment rate of AC in published mathematics').

omega_variable(
    godel_cohen_independence_boundedness,
    'Does the Gödel-Cohen independence result establish that AC is immutable, or only that AC is undecidable within ZF? Is the constraint logical or epistemic?',
    'Philosophical analysis: distinguish between (a) AC is logically independent (true in some models, false in others), (b) AC cannot be decided by current axioms (epistemically undecidable), and (c) AC is immutable within all consistent models. The mathematical result is (a)/(b); the interpretation that makes it immutable is (c). Challenge this interpretation.',
    'If (a)/(b) interpretation holds: AC is contingent (not immutable), and classification shifts from Mountain toward Rope or even Tangled Rope. If (c) interpretation holds: AC is immutable and Mountain classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(godel_cohen_independence_boundedness, conceptual, 'Whether Gödel-Cohen independence implies immutability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(axiom_of_choice_dependency, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aoc_tr_t0, axiom_of_choice_dependency, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aoc_tr_t40, axiom_of_choice_dependency, theater_ratio, 40, 0.15).
narrative_ontology:measurement(aoc_tr_t80, axiom_of_choice_dependency, theater_ratio, 80, 0.18).

% Extraction over time
narrative_ontology:measurement(aoc_be_t0, axiom_of_choice_dependency, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(aoc_be_t40, axiom_of_choice_dependency, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(aoc_be_t80, axiom_of_choice_dependency, base_extractiveness, 80, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(axiom_of_choice_dependency, information_standard).
narrative_ontology:affects_constraint(axiom_of_choice_dependency, non_constructive_proof_inference).
narrative_ontology:affects_constraint(axiom_of_choice_dependency, well_ordering_principle_dependency).
narrative_ontology:affects_constraint(axiom_of_choice_dependency, banach_tarski_paradox_constructibility).

% DUAL FORMULATION NOTE:
% AC dependency decomposes into multiple structurally distinct constraints: (1) AC as a logical immutability within formal systems (Mountain), (2) AC as an institutional default in mathematics pedagogy (Piton/Rope), (3) AC as a functional requirement for specific theorems (varies by theorem). The base story addresses the logical level; downstream stories address the institutional and practical levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
