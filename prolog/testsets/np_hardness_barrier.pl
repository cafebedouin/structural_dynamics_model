% ============================================================================
% CONSTRAINT STORY: np_hardness_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_np_hardness_barrier, []).

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
 *   constraint_id: np_hardness_barrier
 *   human_readable: NP Hardness Barrier in Computational Complexity
 *   domain: theoretical_computer_science/computational_complexity
 *
 * SUMMARY:
 *   The NP hardness barrier is a structural limit on computational
 *   tractability. Problems in the NP class can be verified in polynomial
 *   time, but no known polynomial-time algorithm solves them (with high
 *   probability). The barrier emerges from the mathematical definition of
 *   computational complexity classes and appears invariant across 50+ years
 *   of algorithmic research, attempted proofs, and theoretical frameworks.
 *   From all observable positions — the theoretical computer scientist, the
 *   practical algorithm designer, the cryptographic system architect — the
 *   barrier appears as an immutable physical and logical law. This constraint
 *   exemplifies a pure mountain: fixed ε (0.18 represents the fundamental gap
 *   between P and NP spaces), minimal suppression (the barrier is structural,
 *   not enforced), negligible theater (the mathematical structure is
 *   transparent; the verification is direct proof, not performative ritual).
 *
 * KEY AGENTS:
 *   - Theoretical Computer Scientist: Analytical observer (analytical/analytical) — studies the barrier directly; measures accessibility and resistance
 *   - Algorithm Designer: Practical victim (powerless/trapped) — faces insurmountable barriers for NP-complete instances at scale; no exit options
 *   - Cryptographic System: Institutional beneficiary (institutional/arbitrage) — derives security from the assumption that the barrier is real; would face catastrophic extraction if P = NP, but assumes arbitrage into other hardness assumptions
 *   - Quantum Computing Researcher: Analytical observer with alternative hypothesis — tests whether quantum paradigm bypasses the barrier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(np_hardness_barrier, 0.18).
domain_priors:suppression_score(np_hardness_barrier, 0.02).
domain_priors:theater_ratio(np_hardness_barrier, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(np_hardness_barrier, extractiveness, 0.18).
narrative_ontology:constraint_metric(np_hardness_barrier, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(np_hardness_barrier, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(np_hardness_barrier, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(np_hardness_barrier, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(np_hardness_barrier, mountain).
narrative_ontology:human_readable(np_hardness_barrier, "NP Hardness Barrier in Computational Complexity").
narrative_ontology:topic_domain(np_hardness_barrier, "theoretical_computer_science/computational_complexity").

domain_priors:emerges_naturally(np_hardness_barrier).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL THEORIST (MOUNTAIN) — No known polynomial-time algorithm for NP-complete problems exists despite 50+ years of intensive effort. The barrier appears as a fundamental structural limit of computational expressiveness. Accessibility collapse: the search space grows exponentially; no shortcut is known. Resistance to proof: the P vs NP problem remains open, but empirical evidence across all known algorithms and theoretical frameworks converges on immutability.
constraint_indexing:constraint_classification(np_hardness_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM DESIGNER (MOUNTAIN) — For any practical instance of an NP-complete problem at scale, the computational barrier is insurmountable within available time and resources. No exit path exists: approximation algorithms have their own hardness barriers, quantum speedups are unproven, and brute force remains exponential. The constraint appears as an immutable physical and logical law.
constraint_indexing:constraint_classification(np_hardness_barrier, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: CRYPTOGRAPHIC SYSTEM (MOUNTAIN) — RSA, discrete log, and lattice-based cryptography derive security guarantees from the assumption that factorization and related NP-hard problems have no polynomial-time solutions. Even institutional actors with substantial resources cannot bypass the barrier — the constraint is structural to the mathematics itself, not a policy that can be arbitraged.
constraint_indexing:constraint_classification(np_hardness_barrier, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(np_hardness_barrier_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(np_hardness_barrier, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(np_hardness_barrier, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(np_hardness_barrier, ExtMetricName, E),
    domain_priors:suppression_score(np_hardness_barrier, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(np_hardness_barrier),
    narrative_ontology:constraint_metric(np_hardness_barrier, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(np_hardness_barrier, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(np_hardness_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Represents the gap between the accessible space (verifiable in polynomial time) and the solvable space (no known polynomial algorithm). This gap is intrinsic to the computational model, not enforced by any external agent. The value reflects the fundamental asymmetry in computational complexity, not a policy-driven extraction. Suppression (0.02): Minimal. The barrier is not suppressed through coercion or alternative denial — it is transparent in the mathematical structure. Every algorithm designer knows why NP-complete problems are hard; there is no informational asymmetry. Theater ratio (0.15): Very low. The verification is direct mathematical proof, not performative ritual. The P vs NP problem is an open question, but the empirical evidence (the universal hardness across all attempted approaches) supports the barrier's existence without requiring performative validation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits perspective invariance, not perspectival gap. All perspectives classify as mountain because the barrier is genuinely universal and immutable from every computational position. This is the defining characteristic of a true natural law: no observer position produces a different classification. The cryptographer sees the barrier as enabling security (a beneficiary position), but still perceives it as an immutable natural law — they benefit because the law is stable, not because they have escaped it.
 *
 * DIRECTIONALITY LOGIC:
 *   The NP hardness barrier has no beneficiary/victim structure in the traditional sense. It is not an extraction mechanism maintained by an agent against others. Instead, it is a structural property of the computational universe itself. All agents — theorists, designers, cryptographers — face the same barrier. The barrier does not privilege one agent over another; it constrains all agents equally (up to constant factors). This is why the mountain classification holds across all perspectives: there is no asymmetric extraction, no enforcement, no agent group that could exit by paying a cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_vs_np_resolution,
    'Is P = NP, or does the hardness barrier represent a fundamental asymmetry in computational complexity?',
    'Proof or disproof of the P vs NP conjecture; discovery of a polynomial-time algorithm for an NP-complete problem or a proof that no such algorithm exists',
    'If P = NP: the mountain classification is falsified — the barrier collapses and becomes accessible (snare for those who exploit it, rope for coordination). If P ≠ NP: the mountain classification is confirmed. If undecidable within standard axioms: the barrier is contingent on foundational assumptions, not absolute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p_vs_np_resolution, empirical, 'Whether P equals NP').

omega_variable(
    quantum_speedup_sufficiency,
    'Can quantum algorithms (Grover''s algorithm, quantum annealing, hybrid approaches) achieve polynomial-time solutions for NP-complete problems?',
    'Theoretical proof of quantum polynomial-time algorithm for NP-complete problem; demonstration of quantum advantage on large instances; proof that quantum speedups are fundamentally limited by the same barriers',
    'If quantum achieves polynomial time: barrier remains structural (still a mountain, but for classical systems specifically). If quantum speedup is limited to quadratic: barrier persists. If quantum faces the same exponential wall: mountain classification is strengthened across computational paradigms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_speedup_sufficiency, empirical, 'Whether quantum algorithms can solve NP-complete problems in polynomial time').

omega_variable(
    average_case_hardness,
    'Is NP hardness a worst-case phenomenon, or does average-case hardness hold for typical instances?',
    'Construction of polynomial-time algorithms for average-case NP-complete problem instances; proof that worst-case hardness does not imply average-case hardness for natural distributions',
    'If average case is tractable: practical extraction of NP-hard problems becomes possible, reducing suppression. If average case is equally hard: mountain classification is strengthened for realistic scenarios. If hardness depends on instance distribution: barrier becomes conditional (rope-like under some distributions, mountain under others).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(average_case_hardness, empirical, 'Whether NP hardness applies to average-case instances').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(np_hardness_barrier, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(np_hard_tr_t0, np_hardness_barrier, theater_ratio, 0, 0.1).
narrative_ontology:measurement(np_hard_tr_t25, np_hardness_barrier, theater_ratio, 25, 0.14).
narrative_ontology:measurement(np_hard_tr_t50, np_hardness_barrier, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(np_hard_be_t0, np_hardness_barrier, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(np_hard_be_t25, np_hardness_barrier, base_extractiveness, 25, 0.17).
narrative_ontology:measurement(np_hard_be_t50, np_hardness_barrier, base_extractiveness, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(np_hardness_barrier, cryptographic_security_assumption).
narrative_ontology:affects_constraint(np_hardness_barrier, approximation_algorithm_barrier).
narrative_ontology:affects_constraint(np_hardness_barrier, quantum_advantage_limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
