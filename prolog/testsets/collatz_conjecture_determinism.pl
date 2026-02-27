% ============================================================================
% CONSTRAINT STORY: collatz_conjecture_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collatz_conjecture_determinism, []).

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
 *   constraint_id: collatz_conjecture_determinism
 *   human_readable: The Collatz Conjecture (3n + 1) Determinism
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Collatz conjecture presents a mathematical claim about the
 *   deterministic behavior of a simple iterative function: starting from any
 *   positive integer, apply the rule (if even, divide by 2; if odd, multiply
 *   by 3 and add 1) repeatedly. The conjecture asserts that every starting
 *   value eventually reaches 1. Despite 80+ years of computational
 *   verification across billions of test cases and the engagement of
 *   thousands of mathematicians, no proof or counterexample has been found.
 *   This constraint exemplifies a mountain — a logical/mathematical limit
 *   that is truly immutable across all perspectives. The base extractiveness
 *   (0.08) reflects the absolute theoretical barrier: no agent can escape the
 *   logical structure of the problem through strategic action, resource
 *   allocation, or institutional choice. The constraint emerges from the
 *   axioms of arithmetic, not from enforcement or coordination, satisfying
 *   the natural law signature.
 *
 * KEY AGENTS:
 *   - Empiricist community: Computational verifiers (powerless/trapped) — can test but never exhaustively prove; trapped by the finiteness of verification against infinite cases
 *   - Mathematical research community: Organized effort (organized/constrained) — thousands of mathematicians seeking proof over 80+ years; constrained by the logical depth of the problem itself, not by resources
 *   - Formal systems (PA, ZFC, etc.): Axiom base (analytical/analytical) — the conjecture's truth-status depends on which axioms are accepted; analytical perspective reveals the axiom-dependency question
 *   - Individual mathematician: Single agent seeking proof (moderate/mobile) — can choose research direction but cannot escape the logical constraint once engaged
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collatz_conjecture_determinism, 0.08).
domain_priors:suppression_score(collatz_conjecture_determinism, 0.02).
domain_priors:theater_ratio(collatz_conjecture_determinism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collatz_conjecture_determinism, extractiveness, 0.08).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(collatz_conjecture_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collatz_conjecture_determinism, mountain).
narrative_ontology:human_readable(collatz_conjecture_determinism, "The Collatz Conjecture (3n + 1) Determinism").
narrative_ontology:topic_domain(collatz_conjecture_determinism, "mathematical/logical").

domain_priors:emerges_naturally(collatz_conjecture_determinism).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICIST — Has verified the conjecture for billions of starting values without finding a counterexample. Trapped within the computational horizon: cannot exhaustively verify all positive integers. No escape from the logical constraint — every integer either satisfies the property or provides a counterexample. The conjecture's logical necessity is inescapable.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER — The conjecture describes a structural property of the iteration function on positive integers. Logical necessity bounds the space of possibilities. The iteration either terminates at 1 or enters a cycle or diverges — no fourth option exists. The constraint emerges from the axioms of arithmetic, not from enforcement or coordination choice.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: RESEARCH COMMUNITY — Despite 80+ years of effort and the engagement of thousands of mathematicians, no proof exists. The research community is collectively trapped by the logical architecture of the problem itself. Even with unlimited resources and coordination, the underlying constraint (logical depth of the proof structure) cannot be bypassed. The constraint's immutability is the reason the problem remains open.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collatz_conjecture_determinism_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, ExtMetricName, E),
    domain_priors:suppression_score(collatz_conjecture_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(collatz_conjecture_determinism),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(collatz_conjecture_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract resources or asymmetrically redistribute power — it is a logical property. The small non-zero value reflects the epistemological cost: verifying the conjecture requires computational effort and human insight, but these are not 'extracted' from anyone; they are freely contributed by researchers choosing to engage. No agent bears a cost imposed by another agent via this constraint. Suppression (0.02): Negligible. There are no alternatives being suppressed — the logical structure of the iteration is what it is. Researchers are free to abandon the problem, pursue alternative approaches, or accept independent solutions. Theater ratio (0.15): Low. Mathematical proofs are not performative — either a proof is valid or it is not. The small theater value reflects only the academic presentation overhead (papers, conferences) necessary to communicate results, not any substantive falsity in the mathematical claim itself. The ratio's slight increase over 80 years (0.08 → 0.15) reflects growing computational complexity and the need for more sophisticated presentation methods, not increasing falsity.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint as mountain, revealing the true invariance of the logical property. The empiricist cannot escape the finiteness barrier; the analytical observer sees the logical necessity; the organized research community is constrained by the problem's inherent depth, not by external enforcement. Unlike the verification bottleneck exemplar, which showed perspectival divergence, the Collatz constraint demonstrates perspectival invariance — a gold-standard natural law property. No agent experiences the constraint as extraction, coordination, or temporary scaffolding. All experience it as an immutable logical boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to mountains. No agent is beneficiary or victim — the constraint is a property of the mathematical structure itself, not a relationship between agents. The base extraction value (0.08) is constraint-inherent, not derived from power differential, exit options, or structural positioning. All agents face the same logical wall regardless of their power level or exit capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   GOLD-STANDARD MOUNTAIN: The Collatz conjecture resolves mandatrophy trivially — there is no extraction, no coordination, and no institutional arrangement to mask as natural law. The constraint is a genuine mathematical property, emerging directly from the axioms of arithmetic and the definition of the iteration function. The accessibility collapse (0.92) reflects that the problem is logically inaccessible to current mathematics — the techniques available are structurally incapable of reaching the proof, not because verification is hidden behind institutional barriers, but because the logical depth of the problem exceeds current mathematical tools. The resistance (0.03) reflects negligible alternative framings — the conjecture is what it is, independent of how one phrases it. This constraint is a true mountain, not a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_existence_logical_depth,
    'Is the Collatz conjecture provable within standard Peano arithmetic, or is it (like Goodstein''s theorem) logically independent of PA even though computationally verifiable?',
    'Formal independence proof (via Gödel-class methods); demonstration of PA unprovability while maintaining computational decidability',
    'If independent: the conjecture is logically contingent on axiom choice (mountain property depends on formal system). If provable in PA: the conjecture is a true mountain — logically determined by arithmetic''s foundational structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_existence_logical_depth, conceptual, 'Whether Collatz is provable in standard arithmetic or logically independent').

omega_variable(
    computational_universality_barrier,
    'Does the Collatz sequence exhibit uncomputable properties (e.g., the halting behavior being non-primitive-recursive or Turing-hard) that create a fundamental epistemological ceiling even if the conjecture is true?',
    'Complexity-theoretic analysis of the iteration trajectory; proof of lower bounds on the stopping time function''s computational class',
    'If Turing-hard: even a valid proof may not provide constructive bounds on halting times for arbitrary starting values — the constraint is mountain-like in impossibility but with a computability flavor. If primitive-recursive: the stopping time is bounded and computable, reducing the logical depth barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_universality_barrier, empirical, 'Whether Collatz sequence dynamics exhibit uncomputable properties').

omega_variable(
    axiom_system_contingency,
    'If the conjecture is independent of ZFC or other standard axiom systems, is it still a ''mountain'' or does axiom-dependence downgrade it to a ''rope'' (coordination on which axioms to accept)?',
    'Independence proof in multiple formal systems (PA, ZFC, ZFC+GCH); mapping of which axioms are required for the proof',
    'If independent: the conjecture''s classification shifts — it is a mountain only within chosen axiom system, making it partially contingent on coordination (axiom choice) rather than universal logical necessity. If provable in all systems: mountain status is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_system_contingency, conceptual, 'Whether Collatz independence is system-relative or universal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collatz_conjecture_determinism, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(collatz_tr_t0, collatz_conjecture_determinism, theater_ratio, 0, 0.08).
narrative_ontology:measurement(collatz_tr_t40, collatz_conjecture_determinism, theater_ratio, 40, 0.12).
narrative_ontology:measurement(collatz_tr_t80, collatz_conjecture_determinism, theater_ratio, 80, 0.15).

% Extraction over time
narrative_ontology:measurement(collatz_be_t0, collatz_conjecture_determinism, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(collatz_be_t40, collatz_conjecture_determinism, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(collatz_be_t80, collatz_conjecture_determinism, base_extractiveness, 80, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collatz_conjecture_determinism, information_standard).
narrative_ontology:affects_constraint(collatz_conjecture_determinism, godel_incompleteness_first).
narrative_ontology:affects_constraint(collatz_conjecture_determinism, halting_problem_undecidability).

% DUAL FORMULATION NOTE:
% The Collatz conjecture is part of a constraint family with other mathematical undecidability and independence results. Gödel's first incompleteness theorem and the halting problem represent similar logical barriers. The Collatz conjecture is downstream of these more foundational results — its difficulty reflects deeper limitations on formal proof systems and computational decidability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
