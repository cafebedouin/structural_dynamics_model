% ============================================================================
% CONSTRAINT STORY: prime_factorization_hardness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prime_factorization_hardness, []).

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
 *   constraint_id: prime_factorization_hardness
 *   human_readable: Computational Hardness of Prime Factorization
 *   domain: mathematics/computational_complexity/cryptography
 *
 * SUMMARY:
 *   Prime factorization hardness is a constraint on integer arithmetic: given
 *   a composite number that is the product of two large primes, determining
 *   the prime factors requires computational work that grows exponentially in
 *   the bit-length of the number. This constraint is a paradigmatic example
 *   of a mountain — it emerges necessarily from the structure of
 *   multiplication and primality, appears invariant across all computational
 *   contexts, and admits no agent perspective that can circumvent it. The
 *   constraint is the mathematical foundation of RSA cryptography and
 *   underlies most public-key infrastructure. It is not a policy, not an
 *   enforcement mechanism, and not subject to institutional variation. The
 *   only substantive uncertainty concerns whether the classical hardness
 *   remains a mountain when quantum computers are introduced, and whether the
 *   constraint's necessity depends on unproven conjectures (P ≠ NP) rather
 *   than mathematical axioms.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Analytical observers (analytical/analytical) — establishes the constraint as necessary consequence of integer arithmetic
 *   - Cryptographic Systems: Agents relying on factorization hardness (powerless/trapped) — cannot escape the computational barrier; security depends on the hardness assumption
 *   - Cryptographic Industry: Institutional actors (institutional/arbitrage) — cannot make factorization easier; can only substitute alternative cryptographic primitives
 *   - Algorithm Designers: Agents searching for faster factorization algorithms (powerless/trapped) — any success would prove the constraint weaker than assumed, but the constraint itself cannot be eliminated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prime_factorization_hardness, 0.12).
domain_priors:suppression_score(prime_factorization_hardness, 0.03).
domain_priors:theater_ratio(prime_factorization_hardness, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prime_factorization_hardness, extractiveness, 0.12).
narrative_ontology:constraint_metric(prime_factorization_hardness, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(prime_factorization_hardness, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prime_factorization_hardness, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(prime_factorization_hardness, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prime_factorization_hardness, mountain).
narrative_ontology:human_readable(prime_factorization_hardness, "Computational Hardness of Prime Factorization").
narrative_ontology:topic_domain(prime_factorization_hardness, "mathematics/computational_complexity/cryptography").

domain_priors:emerges_naturally(prime_factorization_hardness).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL OBSERVER (MOUNTAIN) — From the universal mathematical perspective, prime factorization hardness is a consequence of the multiplicative structure of integers. No agent can exit this constraint; it emerges necessarily from the definition of multiplication and primality. Zero degrees of freedom across all indices.
constraint_indexing:constraint_classification(prime_factorization_hardness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RSA PROTOCOL AGENT (MOUNTAIN) — From the perspective of any cryptographic system relying on factorization hardness, the constraint is immutable. The agent cannot escape the computational structure that makes large composite numbers difficult to factor. This is not a policy choice or enforcement mechanism — it is a structural property of arithmetic.
constraint_indexing:constraint_classification(prime_factorization_hardness, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: CRYPTOGRAPHIC INDUSTRY (MOUNTAIN) — Even institutions with maximal exit options cannot circumvent the hardness of factorization. They can adopt alternative cryptographic primitives (elliptic curves, lattice-based schemes), but they cannot make factorization easier — only discover faster algorithms that still leave the problem computationally hard. The constraint is invariant across all institutional framings.
constraint_indexing:constraint_classification(prime_factorization_hardness, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ALGORITHM DESIGNER (MOUNTAIN) — Even from the immediate time horizon, factorization hardness appears as a mathematical constant. Shor's algorithm proves that quantum computers could solve this in polynomial time, but that is an existence proof of a different constraint, not an escape from the classical hardness. The classical computational barrier remains invariant.
constraint_indexing:constraint_classification(prime_factorization_hardness, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_factorization_hardness_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(prime_factorization_hardness, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_factorization_hardness, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(prime_factorization_hardness, ExtMetricName, E),
    domain_priors:suppression_score(prime_factorization_hardness, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(prime_factorization_hardness),
    narrative_ontology:constraint_metric(prime_factorization_hardness, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(prime_factorization_hardness, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(prime_factorization_hardness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint does not extract value from agents — it is a structural property of arithmetic that all agents confront equally. No agent benefits at the expense of another; all agents face the same mathematical barrier. Suppression (0.03): Minimal. There are no alternatives suppressed; factorization hardness is not preventing agents from accessing a better option. The constraint simply defines the computational difficulty of a specific problem. Theater ratio (0.08): Minimal. The constraint has essentially no performative component. Its existence and properties are mathematically verifiable, not socially maintained. Accessibility collapse (0.92): Very high. There is no accessible alternative to factorization hardness within integer arithmetic — the constraint is physically and logically unavoidable. The only accessible alternatives require changing the computational substrate (quantum computing) or switching to entirely different cryptographic foundations (elliptic curves, lattice-based schemes), neither of which escape the constraint but rather sidestep it. Resistance (0.08): Very low. The constraint faces minimal resistance because it is not imposed by agents — it emerges from the definition of multiplication. Agents accept it as a given rather than resisting it.
 *
 * PERSPECTIVAL GAP:
 *   Remarkably, there is NO perspectival gap in this constraint. All perspectives — analytical/civilizational, powerless/trapped, institutional/arbitrage, algorithm designer/immediate — classify the constraint as mountain. This uniformity is diagnostically significant: it indicates a genuine natural law, not a contingent institutional arrangement naturalized as necessity. A constraint that appears mountain from all perspectives is either a true NL or has been stripped of its structural complexity through insufficient decomposition. In this case, decomposition confirms the mountain: factorization hardness is independent of cryptographic protocol choice, computational substrate (classically), time horizon, agent power, or exit options. The uniformity is not a flaw but a feature — it demonstrates that the constraint is genuinely invariant.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through uniformity: all perspectives produce the same classification (mountain) because the constraint is genuinely invariant across all observation contexts. There is no tension between 'is this coordination or extraction?' — the question does not apply. Factorization hardness is neither; it is a structural fact about arithmetic. The potential mandatrophy arises only at the meta-level: is the constraint truly necessary or is its necessity conditional on unproven assumptions (P ≠ NP, classical computation model)? The omega variables address this: if P = NP, the mountain dissolves to rope; if quantum computers are scalable, the classical mountain shifts to a different topography. But within the classical, deterministic Turing machine model with P ≠ NP, the constraint is resolved as a genuine mountain — not a false summit naturalizing contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_collapse_boundary,
    'Does Shor''s algorithm constitute a resolution of prime factorization hardness or merely a shift to a different computational substrate (classical → quantum)?',
    'Definitional: factorization hardness applies to the classical Turing machine model. Shor''s algorithm operates on quantum computers — a different computational substrate with different assumptions. If the constraint is redefined as ''hardness relative to a computational model,'' it becomes conditional (rope or snare from relevant perspectival indices), not a mountain. If it remains ''inherent difficulty of the mathematical problem,'' quantum algorithms are orthogonal.',
    'If Shor''s algorithm counts as ''solving'' the constraint: the mountain classification is conditional on the computational substrate. Reclassify as rope from quantum-capable institutional perspectives. If it does not count: factorization hardness remains a mountain, and quantum algorithms represent a parallel constraint in the network.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_collapse_boundary, conceptual, 'Whether Shor''s algorithm resolves or sidesteps the hardness constraint').

omega_variable(
    average_case_vs_worst_case,
    'Is the constraint ''worst-case hardness of factorization'' (mathematical mountain) or ''expected-case hardness for random composites used in RSA'' (empirical and potentially weaker)?',
    'Theoretical analysis of average-case hardness; empirical study of whether RSA-size composites (products of two large primes) exhibit the same hardness distribution as the mathematical worst case; historical records of algorithmic improvements (general number field sieve, elliptic curve factorization) and their impact on practical security.',
    'If average-case hardness is strictly weaker than worst-case: RSA security is an assumption, not a mathematical necessity — reclassify to rope or snare from cryptographic institutional perspective. If equivalent: factorization hardness remains a mountain for all practical RSA agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(average_case_vs_worst_case, empirical, 'Whether hardness holds for average-case or only worst-case factorization instances').

omega_variable(
    p_vs_np_undecidability,
    'Is the fundamental hardness of factorization dependent on the truth or falsity of P ≠ NP, or is it an autonomous mathematical fact?',
    'Meta-mathematical: if P = NP is true, factorization is in P and can be solved in polynomial time (mountain dissolves). If P ≠ NP is true, hardness remains but may still be conditional on the gap between NP and P. Definitional clarity: factorization is known to be in NP and widely believed to be NP-hard, but this remains unproven. The constraint''s necessity depends on an open conjecture.',
    'If P = NP: factorization hardness is false — reclassify to rope (problem becomes tractable). If P ≠ NP and factorization is NP-complete: hardness is a mountain. If P ≠ NP but factorization is not NP-hard: hardness exists but is weaker than the worst-case barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p_vs_np_undecidability, conceptual, 'Dependency of factorization hardness on the P vs NP conjecture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prime_factorization_hardness, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfh_tr_t0, prime_factorization_hardness, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pfh_tr_t50, prime_factorization_hardness, theater_ratio, 50, 0.08).
narrative_ontology:measurement(pfh_tr_t100, prime_factorization_hardness, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(pfh_be_t0, prime_factorization_hardness, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(pfh_be_t50, prime_factorization_hardness, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(pfh_be_t100, prime_factorization_hardness, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prime_factorization_hardness, information_standard).
narrative_ontology:affects_constraint(prime_factorization_hardness, rsa_security_assumption).
narrative_ontology:affects_constraint(prime_factorization_hardness, elliptic_curve_discrete_log).
narrative_ontology:affects_constraint(prime_factorization_hardness, quantum_shor_algorithm).

% DUAL FORMULATION NOTE:
% Prime factorization hardness is upstream of multiple cryptographic constraints. RSA security assumes factorization hardness; elliptic curve systems assume discrete logarithm hardness (a structurally similar but distinct constraint); Shor's algorithm proves that quantum computers would collapse this mountain to a different computational regime. The network links document the structural dependency: failure of any of these downstream constraints implies revisiting the hardness assumption itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
