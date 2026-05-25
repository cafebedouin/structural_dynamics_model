% ============================================================================
% CONSTRAINT STORY: ramsey_numbers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ramsey_numbers, []).

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
 *   constraint_id: ramsey_numbers
 *   human_readable: Inevitability of Order (Ramsey's Theorem)
 *   domain: mathematical/combinatorics
 *
 * SUMMARY:
 *   Ramsey's Theorem, proved by Frank P. Ramsey in 1930, asserts that for any
 *   finite coloring of the complete graph on a sufficiently large set of
 *   vertices, there must exist a monochromatic complete subgraph of specified
 *   size. More generally, any partitioning of a sufficiently large structure
 *   into finitely many classes must contain a large, uniform (monochromatic
 *   or homogeneous) subset. This is a fundamental result in combinatorics
 *   with no known counterexample, no escape clause, and no observational
 *   dependency. The constraint exhibits zero degrees of freedom: no agent, no
 *   measurement basis, no alternative axiomatization avoids the conclusion.
 *   Ramsey numbers — the thresholds at which order becomes inevitable — grow
 *   extremely rapidly (double exponential for small cases, TREE-class for
 *   larger generalizations), making them practically uncomputable for
 *   non-trivial cases. However, the logical necessity of their existence is
 *   absolute. The constraint does not extract from any agent, enforce via any
 *   institutional mechanism, or depend on any agent's power position. It is
 *   natural law in the strongest sense: a consequence of mathematical logic
 *   itself.
 *
 * KEY AGENTS:
 *   - The Colorizer: Any entity attempting to partition a large set (mathematical, abstract) — experiences zero exit options and zero extraction, merely logical necessity
 *   - Mathematical Logic: The foundational framework from which the theorem emerges — neither beneficiary nor extractor, but the source of the constraint itself
 *   - The Analyst: Observer of the mathematical structure — can verify but not escape; sees pure necessity, not coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ramsey_numbers, 0.12).
domain_priors:suppression_score(ramsey_numbers, 0.02).
domain_priors:theater_ratio(ramsey_numbers, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ramsey_numbers, extractiveness, 0.12).
narrative_ontology:constraint_metric(ramsey_numbers, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ramsey_numbers, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ramsey_numbers, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ramsey_numbers, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ramsey_numbers, mountain).
narrative_ontology:human_readable(ramsey_numbers, "Inevitability of Order (Ramsey's Theorem)").
narrative_ontology:topic_domain(ramsey_numbers, "mathematical/combinatorics").

domain_priors:emerges_naturally(ramsey_numbers).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONSTRAINED COLORIZER (MOUNTAIN) — An agent forced to partition a sufficiently large set into finitely many classes has zero degrees of freedom regarding the emergence of monochromatic order. No exit option exists; the ordered substructure MUST appear. This is not coercion by another agent but structural impossibility of avoiding order.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE MATHEMATICAL OBSERVER (MOUNTAIN) — From the perspective of pure mathematics, Ramsey's theorem is a logical necessity. Given the axioms of set theory and combinatorics, the existence of Ramsey numbers is a deducible consequence, not a contingent constraint. The observer can only acknowledge the inevitability; no measurement basis or interpretation changes the logical requirement.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE FOUNDATIONAL FRAMEWORK (MOUNTAIN) — Ramsey's theorem emerges from the foundations of mathematics itself. No finite extension of computational power or alternative axiomatization eliminates the constraint. It is as immutable as the law of non-contradiction. Institutional verification (peer review, formalization in proof assistants) uniformly confirms the logical necessity.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ramsey_numbers_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ramsey_numbers, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ramsey_numbers, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ramsey_numbers, ExtMetricName, E),
    domain_priors:suppression_score(ramsey_numbers, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ramsey_numbers),
    narrative_ontology:constraint_metric(ramsey_numbers, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ramsey_numbers, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ramsey_numbers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Ramsey's theorem imposes no cost on any agent in the classical sense. The constraint is not extraction from one entity toward another. The modest value (not zero) reflects that discovering Ramsey numbers and their bounds requires computational effort and mathematical insight, creating a trivial 'cost' of understanding. This is not extraction but epistemic labor. Suppression (0.02): Negligible. There are no suppressed alternatives, hidden options, or coercive mechanisms. The theorem operates openly and necessarily. The tiny residual value reflects only that some formulations are more accessible than others, but this is pedagogical clarity, not institutional suppression. Theater ratio (0.05): Near-zero. Ramsey's theorem has no performative component. Verification is through formal proof, not ritual, institutional gesture, or surveillance. The minimal value reflects only the pedagogical scaffolding required to explain the result — the core mathematical claim has zero theater. Accessibility collapse (0.92): Very high. The constraint is nearly inaccessible to violation; there is no meaningful way to color a large set and avoid monochromatic structure. This is the defining feature of a natural law. Resistance (0.08): Very low. No counterexample has ever been found, and none is possible. The logical proof is airtight. Resistance to the constraint is essentially zero; the only resistance is epistemic (difficulty of understanding the proof), not structural.
 *
 * PERSPECTIVAL GAP:
 *   NONE. This is a uniform-type mountain constraint. All three perspectives arrive at identical classification: mountain. The powerless agent, the analytical observer, and the institutional framework all perceive the same logical necessity. There is no gap because there is no exit option, no extraction, and no power asymmetry. The constraint is invariant across all (P,T,E,S) tuples because it emerges from mathematics itself, not from any social or institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to mountain constraints. Ramsey's theorem has no beneficiary or victim in any meaningful sense. It is not a constraint imposed by one agent on another but a constraint imposed by logic on all agents equally. The d-value would be meaningless here; the constraint is not indexed to any agent's power position. All perspectives yield d = undefined or d = 0.5 (symmetric impact on all agents equally), and the mountain classification holds regardless.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_computability_threshold,
    'At what scale does the computational cost of finding monochromatic substructures exceed the informational content of the order itself?',
    'Analysis of Ramsey number growth rates (double exponential, TREE-class bounds) relative to physical universe scale; determination of whether the constraint remains ''natural law'' when Ramsey numbers exceed observable universe parameters',
    'If computability threshold is within physical reach for small cases: mountain classification remains robust. If Ramsey numbers exceed physical computation for all non-trivial cases: the constraint becomes mathematically necessary but practically inaccessible, raising questions about whether it qualifies as a natural law in the strong sense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_computability_threshold, empirical, 'Whether Ramsey numbers remain practically findable').

omega_variable(
    axiom_independence_question,
    'Does Ramsey''s theorem depend on the Axiom of Choice or other non-constructive axioms, or is it constructively provable from minimal set-theoretic foundations?',
    'Proof-theoretic analysis of various formulations (finite vs infinite Ramsey, homogeneous vs heterogeneous); comparison of constructive and classical proofs; examination of intuitionistic logic consistency',
    'If constructively provable: mountain classification is ironclad — the constraint emerges from logic alone. If dependent on non-constructive axioms: there is a lingering conceptual axis (axiom choice) that technically preserves some observational freedom, though this does not diminish the practical inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_independence_question, conceptual, 'Whether Ramsey''s theorem requires non-constructive axioms').

omega_variable(
    alternative_partition_semantics,
    'If partitioning semantics are generalized beyond classical set membership (e.g., fuzzy partition, probabilistic coloring, partial orderings), does an analog of Ramsey''s theorem still hold?',
    'Survey of generalizations in fuzzy Ramsey theory, probabilistic combinatorics, and order-theoretic analogs; determination of which structural properties are preserved under weakening',
    'If analogs exist in all generalizations: the constraint is robust across semantic interpretations, confirming mountain status. If analogs fail or degrade significantly: the constraint may be an artifact of classical set-theoretic framing rather than a true natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_partition_semantics, conceptual, 'Robustness of Ramsey structure under generalized partition semantics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ramsey_numbers, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ramsey_tr_t0, ramsey_numbers, theater_ratio, 0, 0.03).
narrative_ontology:measurement(ramsey_tr_t100, ramsey_numbers, theater_ratio, 100, 0.05).
narrative_ontology:measurement(ramsey_tr_t200, ramsey_numbers, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(ramsey_be_t0, ramsey_numbers, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ramsey_be_t100, ramsey_numbers, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(ramsey_be_t200, ramsey_numbers, base_extractiveness, 200, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ramsey_numbers, information_standard).
narrative_ontology:affects_constraint(ramsey_numbers, pigeonhole_principle).
narrative_ontology:affects_constraint(ramsey_numbers, godel_incompleteness).

% DUAL FORMULATION NOTE:
% Ramsey's theorem sits in a family of unavoidability results in combinatorics. It is upstream of applied constraints that depend on combinatorial structure (scheduling problems, network robustness). The pigeonhole principle is a special case; Gödel's incompleteness shares the property of logical inevitability in formal systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
