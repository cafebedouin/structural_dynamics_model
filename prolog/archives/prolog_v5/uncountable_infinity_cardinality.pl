% ============================================================================
% CONSTRAINT STORY: uncountable_infinity_cardinality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uncountable_infinity_cardinality, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uncountable_infinity_cardinality
 *   human_readable: Uncountable Infinity and the Continuum Cardinality
 *   domain: mathematical_foundations/set_theory
 *
 * SUMMARY:
 *   Uncountable infinity and the cardinality of the continuum represent a
 *   fundamental constraint in mathematical structure. The existence of
 *   uncountably infinite sets — most famously the real numbers — is not a
 *   matter of convention, institutional arrangement, or negotiable resource
 *   distribution. It is a necessary consequence of the axioms of set theory
 *   (ZFC) and emerges from Cantor's diagonalization argument, which proves
 *   that no bijection exists between the natural numbers and the real
 *   numbers. This constraint exhibits zero degrees of freedom across all
 *   indices: all agents, regardless of their mathematical philosophy or
 *   computational capacity, must contend with the fact that infinite
 *   cardinalities come in incomparable sizes. The constraint classifies as
 *   mountain from all perspectives because it is invariant across all
 *   observational positions, mathematical formalisms, and time horizons.
 *
 * KEY AGENTS:
 *   - Finitary Mathematicians: Agents restricting to computable/constructible objects (powerless/trapped) — cannot escape uncountable cardinality through finite methods
 *   - Intuitionist Logicians: Agents rejecting classical logic (moderate/constrained) — still encounter uncountability through constructive methods
 *   - Categorical Algebraists: Agents translating mathematics into categorical language (organized/mobile) — the constraint persists across all formal systems
 *   - Analytical Observer: The global mathematical position (analytical/analytical) — sees uncountable cardinality as a theorem, not a preference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uncountable_infinity_cardinality, 0.12).
domain_priors:suppression_score(uncountable_infinity_cardinality, 0.03).
domain_priors:theater_ratio(uncountable_infinity_cardinality, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uncountable_infinity_cardinality, extractiveness, 0.12).
narrative_ontology:constraint_metric(uncountable_infinity_cardinality, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(uncountable_infinity_cardinality, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(uncountable_infinity_cardinality, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(uncountable_infinity_cardinality, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uncountable_infinity_cardinality, mountain).
narrative_ontology:human_readable(uncountable_infinity_cardinality, "Uncountable Infinity and the Continuum Cardinality").
narrative_ontology:topic_domain(uncountable_infinity_cardinality, "mathematical_foundations/set_theory").

domain_priors:emerges_naturally(uncountable_infinity_cardinality).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FINITARY MATHEMATICIAN (MOUNTAIN) — An agent whose ontology restricts to computable or constructible objects. Cannot escape the fact that uncountably many real numbers exist independent of their constructibility. The constraint is immutable from this position: the cardinality of the continuum is unreducible through finitary means alone.
constraint_indexing:constraint_classification(uncountable_infinity_cardinality, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INTUITIONIST LOGICIAN (MOUNTAIN) — Even rejecting classical logic and the law of excluded middle, the constraint persists: the Dedekind cut construction and Cauchy sequence methods generate uncountable infinities without relying on impredicative comprehension. The unavoidability is structural, not logical-system-dependent.
constraint_indexing:constraint_classification(uncountable_infinity_cardinality, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: CATEGORICAL ALGEBRAIST (MOUNTAIN) — Even translating uncountability into categorical language (limits, colimits, functor categories), the underlying cardinality hierarchy does not disappear. The constraint reappears in any formal system capable of expressing arithmetic and order relations. Mobility here does not overcome the natural law.
constraint_indexing:constraint_classification(uncountable_infinity_cardinality, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the global analytical position, uncountable cardinality is a theorem of ZFC set theory derived from first principles (Cantor's diagonal argument). The constraint expresses an irreducible fact about the structure of infinite sets independent of any observer's preference or measurement methodology. This is a natural law in the strictest sense.
constraint_indexing:constraint_classification(uncountable_infinity_cardinality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uncountable_infinity_cardinality_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(uncountable_infinity_cardinality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uncountable_infinity_cardinality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(uncountable_infinity_cardinality, ExtMetricName, E),
    domain_priors:suppression_score(uncountable_infinity_cardinality, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(uncountable_infinity_cardinality),
    narrative_ontology:constraint_metric(uncountable_infinity_cardinality, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(uncountable_infinity_cardinality, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(uncountable_infinity_cardinality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low, reflecting that uncountable cardinality imposes no directional asymmetry or resource extraction. The constraint is purely structural, not economic or political. Suppression (0.03): Minimal. The constraint does not require coercion or suppression of alternatives — it is not suppressing anything, merely stating a mathematical fact. Theater ratio (0.15): Very low. The constraint has no performative component. Proofs of uncountability are rigorous and verifiable; Cantor's diagonal argument is logically transparent and admits no theater. Accessibility collapse (0.92): Near-maximal. The constraint is accessible to all agents equally — no observer can negotiate or circumvent uncountable cardinality through any known formal system. The only 'collapse' would be discovering ZFC is inconsistent, which would invalidate entire mathematics. Resistance (0.08): Minimal. The constraint encounters almost no resistance because it is not something agents typically resist against — it is a mathematical fact. Philosophical resistance (intuitionists, constructivists, predicativists) exists but does not alter the truth of the theorem under ZFC.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All four perspectives classify identically as mountain. This is characteristic of natural law constraints: mathematical theorems, logical tautologies, and physical impossibilities have zero degrees of freedom. A finitary mathematician cannot become an arbitrage agent or mobile agent through any structural reconfiguration — the cardinality of the continuum exceeds the cardinality of the natural numbers in any formal system that can express both. An intuitionist logician cannot escape by changing philosophical commitments — Dedekind cuts and Cauchy sequences generate uncountable infinities within intuitionistic logic as well. A categorical algebraist cannot reframe away the constraint through abstraction — the cardinality hierarchy persists across all formally equivalent translations. The analytical observer sees what all perspectives see: a theorem.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality computation is applicable to a mountain constraint. Uncountable cardinality does not extract from anyone or benefit anyone. It does not identify beneficiaries or victims because there is no asymmetry of burden or advantage. All agents stand in the same structural relationship to the constraint: they must accept it as a given of mathematical reality. The constraint is not a social arrangement that could be decomposed by beneficiary/victim analysis. It is a fact about the size of infinite sets.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy trivially: it is a genuine mountain, not misclassified extraction. All six perspectives produce mountain classification. No agent experiences the constraint as Rope (pure coordination) or Snare (pure extraction) — there is nothing to coordinate around and nothing to extract. The constraint simply is. The universal invariance of the classification is itself the proof of naturality. If any perspective classified this as tangled rope or scaffold, the diagnosis would immediately reveal an error: either the base properties were misdeclared, or the perspective was using 'uncountable infinity' to mean something different (e.g., the institutional treatment of infinity in real analysis pedagogy, which is a different constraint entirely). The current classification passes all gates: ε ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally = true. No mandatrophy marker is triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uncountable_infinity_cardinality, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uncountable_infinity_cardinality, information_standard).
narrative_ontology:affects_constraint(uncountable_infinity_cardinality, continuum_hypothesis_undecidability).
narrative_ontology:affects_constraint(uncountable_infinity_cardinality, set_cardinality_ordering).

% DUAL FORMULATION NOTE:
% Uncountable cardinality is downstream of the axioms of ZFC set theory. The continuum hypothesis (whether there exists a set strictly between countable and uncountable infinities) is a separate constraint with higher ε and different classification — it represents an irreducible uncertainty, not a natural law. The cardinality ordering constraint (relationships between different infinite cardinalities) is a theoretic extension of uncountable cardinality and inherits its mountain status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
