% ============================================================================
% CONSTRAINT STORY: collatz_conjecture_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The Collatz conjecture (3n+1 problem) is a statement about the behavior
 *   of a simple iterative sequence: starting from any positive integer n,
 *   repeatedly apply the operation 'if n is even, divide by 2; if n is odd,
 *   multiply by 3 and add 1.' The conjecture asserts that all such sequences
 *   eventually reach 1, regardless of the starting value. This constraint
 *   exemplifies a pure mathematical mountain — a property of logical and
 *   numerical structure that exists prior to human agency, institutional
 *   power, or resource allocation. No agent can negotiate, extract rent from,
 *   or suppress the mathematical facts. The conjecture's truth value is
 *   determinate (either provably true, provably false, or independent of
 *   standard axioms), and this determinacy is the constraint itself. The
 *   constraint exhibits zero degrees of freedom across all indices: a formal
 *   logician, a novice, and a research institution all face the same
 *   deterministic mathematical reality. The theater_ratio is low (0.15)
 *   because mathematical verification requires genuine proof or exhaustive
 *   search, not performative ritual. The suppression is minimal (0.03)
 *   because mathematical truth is not enforced through coercion — agents
 *   either accept the proof or work toward their own. The extractiveness is
 *   minimal (0.12) because no agent benefits from the constraint through
 *   institutional leverage; discovery and proof benefit the discoverer's
 *   reputation, but the mathematical fact itself is not extracted rent.
 *
 * KEY AGENTS:
 *   - Formal Logician: Analytical observer (analytical/analytical) — perceives the constraint as pure logical structure, independent of human effort
 *   - Computational Mathematician: Institutional power (institutional/arbitrage) — pursues numerical verification and proof searches; benefits from priority of discovery but cannot alter the underlying truth
 *   - Novice Mathematician: Entry-level perspective (powerless/analytical) — encounters the conjecture as an immutable feature of the mathematical landscape, with no exit option
 *   - The Mathematical Structure Itself: The constraint as an agent — the Collatz iteration defines its own properties; this is the 'agent' that determines outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collatz_conjecture_determinism, 0.12).
domain_priors:suppression_score(collatz_conjecture_determinism, 0.03).
domain_priors:theater_ratio(collatz_conjecture_determinism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collatz_conjecture_determinism, extractiveness, 0.12).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(collatz_conjecture_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, resistance, 0.08).

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

% PERSPECTIVE 1: FORMAL LOGICIAN (MOUNTAIN) — From the vantage of pure logic and mathematical structure, the Collatz determinism is a statement about the iteration properties of a specific function on integers. Either the conjecture is true (all trajectories reach 1) or false (some trajectory escapes or cycles). This is a logical law: ε=0.12, suppression=0.03, no agent experiences extraction or coercion. The constraint is the formal structure itself — the recursive definition of the sequence.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: NOVICE MATHEMATICIAN (MOUNTAIN) — Even from the perspective of a learner with no special status in the mathematical community, the Collatz determinism presents as an immutable property of the number system. No amount of effort, funding, or institutional leverage can change whether the conjecture is true or false. The constraint is prior to any human agency. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08. The novice cannot 'exit' the mathematical facts; they are simply discovered or not.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: RESEARCH INSTITUTION (MOUNTAIN) — Even the largest research organization, with unlimited computational resources, cannot *change* the truth value of the Collatz conjecture through institutional leverage or strategic advantage. The constraint is orthogonal to institutional power. Institutions can allocate resources to verify it numerically or develop new proof techniques, but they cannot extract rent from the mathematical structure itself. The conjecture's truth or falsity is not negotiable.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
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
 *   Extractiveness (0.12): Minimal. The Collatz conjecture does not extract from any agent in the usual sense. If it is true, it is simply a property of integers. If it is false, the falsity reveals a counterexample, which is of mathematical interest but does not constitute extraction. The small positive value (0.12 rather than 0.0) reflects that the formulation of the conjecture itself emerged from human mathematical practice, and the search for a proof allocates computational resources — but this is coordination and discovery, not extraction. Suppression (0.03): Minimal. There is no coercive mechanism preventing agents from working on the conjecture, publishing results, or sharing insights. The mathematical community is relatively open to new proof attempts. The small value reflects only that some structural barriers to entry exist (needing mathematical training), but these are transparent and not suppressive in the sense of the framework. Theater_ratio (0.15): Low. Mathematical proof is the opposite of theater — it is a direct logical verification with no performative component. Numerical verification (testing trillions of cases) is more heuristic but still fundamentally about data, not ritual. The value reflects only the small amount of presentation/pedagogy involved in communicating proofs.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for this constraint. All perspectives converge on the mountain classification. A formal logician sees pure logical structure (mountain). A powerless novice sees the same immutable mathematical fact (mountain). A research institution with unlimited resources still faces the same deterministic constraints (mountain). This is the defining characteristic of a true natural law in the framework: it is invariant across all observables and all structural positions. The lack of perspectival gap is evidence that the mountain classification is correct and that the constraint is a genuine natural law rather than a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no directionality in the classical sense because it has no beneficiaries or victims in the structural framework. The Collatz determinism is not a relationship between agents but a property of mathematical structure. All agents — regardless of power, exit options, or scope — face the same constraint. The d value, if computed from the canonical fallback for 'analytical' power, would be d≈0.73, f(d)≈1.15, but this is misleading: the constraint is not extracting from the analytical observer. The framework's directionality machinery (which derives agent-relative benefit/cost) does not apply to pure logical constraints. The constraint is observer-independent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incompleteness_boundary,
    'Is the Collatz conjecture independent of ZFC (Zermelo-Fraenkel set theory with choice)?',
    'Formal proof of independence or discovery of a proof within ZFC; exhaustive search for constructive counterexamples or proofs of eventual non-convergence',
    'If independent: the conjecture is undecidable within standard mathematics, revealing that the ''determinism'' is not a determinate fact but a boundary between the decidable and undecidable. This would reframe the constraint from a simple mountain to a meta-logical mountain (the unprovability itself is the invariant). If decidable: the constraint remains a simple mountain — either true or false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompleteness_boundary, empirical, 'Whether Collatz is independent of ZFC').

omega_variable(
    computational_verification_sufficiency,
    'Does numerical verification (testing trillions of starting values) constitute evidence for a mathematical conjecture, or is it merely a high-confidence heuristic?',
    'Philosophical analysis of the epistemology of mathematical truth; comparison with other conjectures that were disproven after extensive verification (e.g., Pólya''s conjecture)',
    'If verification suffices as proof: the mountain classification is verified by the computational evidence accumulation. If verification does not suffice: the constraint remains a mountain (the truth is still prior to verification), but the epistemic status is epistemic uncertainty, not ontological certainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(computational_verification_sufficiency, conceptual, 'Whether numerical verification constitutes mathematical evidence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collatz_conjecture_determinism, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coll_tr_t0, collatz_conjecture_determinism, theater_ratio, 0, 0.08).
narrative_ontology:measurement(coll_tr_t50, collatz_conjecture_determinism, theater_ratio, 50, 0.15).
narrative_ontology:measurement(coll_tr_t100, collatz_conjecture_determinism, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(coll_be_t0, collatz_conjecture_determinism, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(coll_be_t50, collatz_conjecture_determinism, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(coll_be_t100, collatz_conjecture_determinism, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collatz_conjecture_determinism, information_standard).
narrative_ontology:affects_constraint(collatz_conjecture_determinism, halting_problem_undecidability).
narrative_ontology:affects_constraint(collatz_conjecture_determinism, godel_incompleteness_first).

% DUAL FORMULATION NOTE:
% The Collatz conjecture is part of a family of undecidable/unprovable mathematical statements. It is distinguished from the Halting Problem and Gödel's theorems by its elementary formulation (no reference to computation or set theory required), but all three share the property of being logical mountains that defy simple resolution. The network links reflect upstream/downstream dependencies: both the Halting Problem and Gödel's results apply constraints to the space of possible mathematical statements, within which Collatz is situated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
