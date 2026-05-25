% ============================================================================
% CONSTRAINT STORY: prime_number_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prime_number_theorem, []).

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
 *   constraint_id: prime_number_theorem
 *   human_readable: Prime Number Theorem (Asymptotic Density)
 *   domain: mathematics/number_theory
 *
 * SUMMARY:
 *   The Prime Number Theorem (PNT) states that the number of primes less than
 *   or equal to a given integer n — denoted π(n) — is asymptotically
 *   equivalent to n/ln(n). This constraint describes a fundamental structural
 *   property of the integers: as n grows large, the density of primes
 *   decreases logarithmically. The PNT was conjectured by Gauss and Legendre
 *   in the late 18th century, rigorously proven independently by Hadamard and
 *   de la Vallée Poussin in 1896, and later simplified by elementary methods
 *   (Erdős, Selberg, 1949). The theorem is a mountain constraint: it arises
 *   from the logical structure of the integers, admits no exceptions, permits
 *   no workarounds, and holds universally across all mathematical frameworks
 *   in which integer arithmetic is defined. No agent benefits from the PNT;
 *   no agent bears extraction costs. The constraint simply describes what
 *   must be true about prime distribution. Mathematicians work within this
 *   boundary, not because they are coerced, but because the boundary is
 *   intrinsic to the domain itself.
 *
 * KEY AGENTS:
 *   - Computational number theorists (analytical/trapped) — must work within the asymptotic constraint; cannot alter prime density
 *   - Research institutions (powerful/analytical) — possess resources to verify and extend PNT results, but cannot escape the constraint
 *   - Cryptography practitioners (powerful/constrained) — use prime distribution for RSA and other algorithms; constrained by but not exploited by PNT
 *   - Pure mathematicians (analytical/analytical) — study PNT as a fundamental truth; no beneficiary-victim relationship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prime_number_theorem, 0.08).
domain_priors:suppression_score(prime_number_theorem, 0.02).
domain_priors:theater_ratio(prime_number_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prime_number_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(prime_number_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(prime_number_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prime_number_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(prime_number_theorem, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prime_number_theorem, mountain).
narrative_ontology:human_readable(prime_number_theorem, "Prime Number Theorem (Asymptotic Density)").
narrative_ontology:topic_domain(prime_number_theorem, "mathematics/number_theory").

domain_priors:emerges_naturally(prime_number_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL NUMBER THEORIST (MOUNTAIN) — Computationally enumerable, empirically verified across billions of primes. The asymptotic density constraint π(n) ~ n/ln(n) is inviolable within formal arithmetic. No workaround, no escape, no substitution possible. The mathematician's work is constrained by the logical structure of the integers themselves.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RESEARCH COMMUNITY (MOUNTAIN) — Even well-resourced institutions with advanced computational capacity cannot alter the asymptotic behavior of prime density. The constraint is invariant across all computational resources, all algorithmic approaches, and all institutional positions. Mathematicians work within this boundary, not around it.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of formal number theory, the PNT is a fundamental structural property of the integers. It emerges from the logical necessity of prime distribution, proven rigorously by Hadamard and de la Vallée Poussin (1896) and simplified by later mathematicians. This is not a contingent institutional arrangement; it is a law of mathematical structure itself.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_number_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(prime_number_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_number_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(prime_number_theorem, ExtMetricName, E),
    domain_priors:suppression_score(prime_number_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(prime_number_theorem),
    narrative_ontology:constraint_metric(prime_number_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(prime_number_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(prime_number_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The PNT describes a structural feature of the integers, not an extraction mechanism. No agent extracts value from other agents through this constraint. The low value reflects that the constraint is purely descriptive of mathematical structure, with no coercive or asymmetric dimension. Suppression (0.02): Negligible. There are no alternatives to suppress; the asymptotic form is logically necessary given the definition of primes. The small non-zero value accounts for the finite complexity of understanding the proof and the historical resistance to accepting the asymptotic result before rigorous proof was established. Theater ratio (0.15): Very low. Modern mathematical exposition of PNT is direct and functional — Erdős's elementary proof (1949) stripped away unnecessary analytic machinery, reducing performative elements. The modest theater reflects only the pedagogical gap between stating the theorem and understanding why it holds.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The PNT classifies as mountain from all perspectives because it describes a mathematical truth independent of observer position. The computational number theorist, the research institution, and the analytical observer all perceive the same asymptotic constraint. This uniformity across perspectives is the defining feature of a mountain constraint. If any perspective produced a different classification, it would indicate either a misunderstanding of PNT or the presence of a secondary constraint (e.g., institutional barriers to cryptographic use) that should be decomposed into its own story.
 *
 * DIRECTIONALITY LOGIC:
 *   The PNT has no directionality in the traditional sense because it has no beneficiaries or victims. The constraint arises from the logical structure of the integers, not from the institutional arrangement or strategic interaction of agents. All perspectives — computational, institutional, analytical — see the same invariant mathematical truth. The d-values derived from these perspectives are meaningless in the traditional sense; instead, the constraint operates at the level of mathematical necessity. All agents, regardless of power or exit options, face the same asymptotic limit on prime density. This uniform experience across all perspectives is the hallmark of a true mountain: inescapable, invariant, and foundational.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this constraint. The PNT is a pure mathematical truth with no coordination function, no extraction mechanism, and no institutional component. It cannot be mislabeled as coordination (it is not coordination) or as extraction (it is not extraction). The constraint simply describes what must be true about the integers. The classification as mountain is not contingent on frame-dependent observables or institutional interpretation — it is invariant across all mathematical frameworks that define prime numbers and asymptotic density.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    riemann_hypothesis_status,
    'Does the Riemann Hypothesis hold? Would its resolution change the effective constraints on prime distribution?',
    'Mathematical proof or counterexample; analysis of how RH affects error terms in asymptotic expansions',
    'If RH true: error bounds tighten dramatically, but asymptotic form π(n) ~ n/ln(n) remains invariant. If RH false: growth behavior fundamentally altered, shifting classification from mountain to degraded constraint. However, current evidence overwhelmingly favors RH; probability of false status is < 0.0001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(riemann_hypothesis_status, empirical, 'Whether the Riemann Hypothesis holds and its impact on prime distribution constraints').

omega_variable(
    formalization_dependence,
    'Does the asymptotic form π(n) ~ n/ln(n) depend on the underlying axiom system (ZFC vs alternatives)?',
    'Formal analysis in constructive mathematics, intuitionistic logic, and alternative set theories; proof-theoretic examination of PNT derivation',
    'If PNT holds across all major axiom systems: the mountain classification is robust. If PNT fails in some formal systems: the constraint is framework-dependent rather than universal. Current status: PNT is proven in ZFC and holds in most standard systems, but some exotic frameworks may differ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalization_dependence, conceptual, 'Whether PNT depends on the axiom system used').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prime_number_theorem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pnt_tr_t0, prime_number_theorem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pnt_tr_t50, prime_number_theorem, theater_ratio, 50, 0.14).
narrative_ontology:measurement(pnt_tr_t100, prime_number_theorem, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(pnt_be_t0, prime_number_theorem, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(pnt_be_t50, prime_number_theorem, base_extractiveness, 50, 0.07).
narrative_ontology:measurement(pnt_be_t100, prime_number_theorem, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prime_number_theorem, information_standard).
narrative_ontology:affects_constraint(prime_number_theorem, riemann_hypothesis).
narrative_ontology:affects_constraint(prime_number_theorem, prime_gap_distribution).
narrative_ontology:affects_constraint(prime_number_theorem, prime_factorization_hardness).

% DUAL FORMULATION NOTE:
% The Prime Number Theorem is foundational to number-theoretic constraints. The Riemann Hypothesis provides sharper error bounds on π(n); prime gap distribution describes local density fluctuations; prime factorization hardness depends on the rarity of primes among large integers — all structurally downstream of PNT.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
