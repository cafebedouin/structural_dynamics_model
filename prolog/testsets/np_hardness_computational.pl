% ============================================================================
% CONSTRAINT STORY: np_hardness_computational
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_np_hardness_computational, []).

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
 *   constraint_id: np_hardness_computational
 *   human_readable: NP-Hardness Computational Barrier
 *   domain: computational_complexity/theoretical_computer_science
 *
 * SUMMARY:
 *   NP-hardness represents a computational barrier that emerges from the
 *   logical structure of decision problems and polynomial-time verification.
 *   A problem is NP-hard if every other NP problem can be reduced to it in
 *   polynomial time; it is NP-complete if it is also in NP (verifiable in
 *   polynomial time). The asymmetry between the ease of verification
 *   (polynomial time) and the difficulty of discovery (no known polynomial
 *   algorithm) creates a structural constraint that applies universally
 *   across all computational systems operating under classical time
 *   complexity bounds. This constraint exhibits the signature of a natural
 *   law: it emerges from axioms (the definition of polynomial time, the
 *   Church-Turing thesis), applies universally (to all agents regardless of
 *   power or resources), admits no external bypass (the barrier is intrinsic
 *   to the problem structure), and is invariant across all observables and
 *   measurement methodologies. The constraint has persisted unchanged for
 *   over 50 years of computational complexity research, through exponential
 *   growth in computing power, algorithmic innovation, and theoretical
 *   advances. Theater ratio remains negligible (0.15) because the
 *   mathematical statement admits no performative content — either a
 *   polynomial algorithm exists or it does not.
 *
 * KEY AGENTS:
 *   - Polynomial-Time Algorithm: Victim/trapped agent (powerless/trapped) — cannot overcome the exponential barrier within its time bound
 *   - Heuristic/Approximation Methods: Constrained agent (moderate/constrained) — can find good-enough solutions but cannot guarantee optimality within polynomial time
 *   - Quantum Computer: Potentially mobile agent (powerful/mobile) — claims to bypass barrier through quantum superposition, but scalability and error correction remain unresolved
 *   - Computational Complexity Theory: Institutional analyst (institutional/analytical) — catalogs the barrier, proves reduction relationships, defines the constraint structure
 *   - Real-World Problem Solver: Pragmatic agent (organized/mobile) — works within restricted problem domains where structure provides speedup, but cannot escape worst-case NP-hardness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(np_hardness_computational, 0.08).
domain_priors:suppression_score(np_hardness_computational, 0.03).
domain_priors:theater_ratio(np_hardness_computational, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(np_hardness_computational, extractiveness, 0.08).
narrative_ontology:constraint_metric(np_hardness_computational, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(np_hardness_computational, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(np_hardness_computational, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(np_hardness_computational, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(np_hardness_computational, mountain).
narrative_ontology:human_readable(np_hardness_computational, "NP-Hardness Computational Barrier").
narrative_ontology:topic_domain(np_hardness_computational, "computational_complexity/theoretical_computer_science").

domain_priors:emerges_naturally(np_hardness_computational).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLYNOMIAL TIME AGENT (MOUNTAIN) — An algorithm bounded to polynomial runtime faces an absolute barrier when confronting NP-hard problems. No strategy, resource allocation, or optimization technique can overcome the structural constraint that the problem's decision space grows exponentially while execution time is bounded polynomially. The agent cannot negotiate, arbitrage, or evade this limit — it is built into the problem structure itself.
constraint_indexing:constraint_classification(np_hardness_computational, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: FORMAL ANALYST (MOUNTAIN) — From the perspective of computational complexity theory, NP-hardness is a structural property derived from logical reducibility and the definition of polynomial-time verifiability. The constraint emerges from the axioms of computation itself: the asymmetry between decision and verification, the exponential explosion of search space, and the Church-Turing thesis. This classification is invariant across all measurement methodologies, all algorithmic approaches, and all problem instances within the NP-hard class. It is a natural law of computation.
constraint_indexing:constraint_classification(np_hardness_computational, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: PRAGMATIC SOLVER (MOUNTAIN) — Even with organizational resources, heuristics, approximations, quantum computing claims, or problem-specific structure, the worst-case NP-hard barrier persists. Organizations can sidestep the constraint by accepting approximate solutions, restricted domains, or heuristic methods, but they cannot remove the fundamental barrier itself. The constraint remains immutable even when agents gain power and mobility — it is a property of the problem, not the solver.
constraint_indexing:constraint_classification(np_hardness_computational, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(np_hardness_computational_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(np_hardness_computational, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(np_hardness_computational, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(np_hardness_computational, ExtMetricName, E),
    domain_priors:suppression_score(np_hardness_computational, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(np_hardness_computational),
    narrative_ontology:constraint_metric(np_hardness_computational, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(np_hardness_computational, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(np_hardness_computational_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. NP-hardness does not extract resources from any agent for the beneficiary of another; it is a property of the computational problem itself. The 0.08 value reflects unavoidable overhead inherent to verification and decision-making, not extraction by an external agent. This is the canonical floor for a natural law. Suppression (0.03): Minimal. There are no coercive mechanisms suppressing alternatives — the barrier exists whether or not agents like it. The 0.03 reflects measurement noise and the observation that awareness of hardness might suppress exploration of certain solution approaches (a secondary effect). Theater ratio (0.15): Very low. Mathematical proofs of NP-completeness are logically airtight and admit no performative content. The 0.15 reflects minor theater in how NP-hardness is communicated, taught, and deployed rhetorically in industry, but the underlying constraint is entirely substantive. Accessibility collapse (0.92): Extreme. The constraint is completely inaccessible to bypass — there is no known path around it and the mathematical structure ensures none exists within classical polynomial-time computation. Resistance (0.08): Minimal. The barrier is not maintained through resistance or institutional defense; it is a structural property of the problem class itself. The 0.08 reflects that some problem instances have hidden tractable structure that reduces practical difficulty, not true resistance to the barrier.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the rare property of uniform classification: all three perspectives (powerless polynomial agent, organized pragmatist, analytical observer) classify it identically as Mountain. This uniformity is itself diagnostic — it confirms the natural law signature. There is no perspectival gap because the constraint is not contingent on observer position. The polynomial agent, the pragmatist, and the analyst all see the same immutable barrier, though they may respond to it differently (resignation, workaround-seeking, proof-exploration). This invariance across all indices is the defining signature of a true mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is inapplicable to mountain constraints — there is no beneficiary or victim group because there is no extraction flow. NP-hardness does not benefit one agent at the expense of another; it is a structural property that applies identically to all agents. The constraint operates at the level of the problem itself, not at the level of agents competing for resources. This absence of directionality is another confirmation of mountain classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_vs_np_resolution,
    'Is P=NP true or false? Does the barrier actually separate two distinct complexity classes or is it a mathematical artifact?',
    'Mathematical proof or disproof of P=NP; or empirical discovery of polynomial-time algorithm for an NP-complete problem',
    'If P=NP: the barrier is illusory — polynomial solutions exist for all NP problems. The mountain collapses to rope (pure coordination of known solutions). If P≠NP: the barrier is proven immutable, confirming mountain classification at maximum confidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p_vs_np_resolution, conceptual, 'Whether the P vs NP problem resolves the fundamental status of the computational barrier').

omega_variable(
    quantum_advantage_scope,
    'Can quantum computers with practical scalability solve NP-hard problems in better-than-polynomial classical time?',
    'Demonstration of quantum algorithm for NP-hard problem with proven quantum speedup and demonstration of scalable quantum hardware (1000+ logical qubits with low error rates)',
    'If quantum advantage is real and practical: for quantum agents the constraint becomes constrained (high-cost but surmountable exit). The mountain remains for classical agents but the agent''s technology matters. If quantum advantage is illusory or unscalable: the mountain persists across all known computational substrates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_advantage_scope, empirical, 'Whether quantum computing provides practical NP-hardness bypass').

omega_variable(
    problem_restriction_boundary,
    'What fraction of real-world problem instances in NP-hard problem classes admit polynomial-time exact solutions due to restrictive structure or unrecognized special cases?',
    'Systematic analysis of problem instance distributions in production systems; identification of hidden tractable subclasses; empirical hardness measurements on real instances vs worst-case theoretical bounds',
    'If majority of practical instances are easy: NP-hardness is a worst-case pathology, and the mountain classification applies only to worst-case analysis. The pragmatic agent sees rope (solvable problems) not mountain (unsolvable). If instances remain hard across distributions: the mountain extends to typical cases as well as worst cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(problem_restriction_boundary, empirical, 'Fraction of real-world NP-hard instances tractable via structure exploitation').

omega_variable(
    oracle_separation_reality,
    'Are oracle separation theorems (proving P≠NP relative to random oracles) evidence that P≠NP in standard computation, or do they reveal that oracle models are not faithful to actual complexity behavior?',
    'Development of new proof techniques for P vs NP; analysis of oracle separation limitations; examination of whether relativized hardness maps to absolute hardness',
    'If oracles faithfully model complexity: oracle separations strengthen mountain classification. If oracles are artifacts of the proof technique: oracle results may not entail P≠NP, weakening the mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracle_separation_reality, conceptual, 'Whether oracle separations constitute valid evidence for absolute complexity barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(np_hardness_computational, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nph_tr_t0, np_hardness_computational, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nph_tr_t15, np_hardness_computational, theater_ratio, 15, 0.15).
narrative_ontology:measurement(nph_tr_t30, np_hardness_computational, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(nph_be_t0, np_hardness_computational, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nph_be_t15, np_hardness_computational, base_extractiveness, 15, 0.08).
narrative_ontology:measurement(nph_be_t30, np_hardness_computational, base_extractiveness, 30, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(np_hardness_computational, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
