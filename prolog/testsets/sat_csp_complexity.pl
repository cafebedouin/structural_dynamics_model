% ============================================================================
% CONSTRAINT STORY: sat_csp_complexity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_sat_csp_complexity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sat_csp_complexity
 *   human_readable: SAT/CSP Computational Complexity
 *   domain: technological
 *
 * SUMMARY:
 *   Boolean Satisfiability (SAT) and Constraint Satisfaction Problems (CSP)
 *   represent the fundamental challenge of finding a set of values that satisfy
 *   a collection of constraints. While checking a given solution is easy (in P),
 *   finding one is often NP-complete, representing a universal "hardness" in
 *   automated reasoning. This creates a structural asymmetry: a coordination
 *   tool for designers but an extractive trap for resource-constrained systems.
 *
 * KEY AGENTS (by structural relationship):
 *   - Real-time Schedulers: Primary target (powerless/constrained) — bears the extraction of computational time, facing catastrophic failure if a solution is not found within a deadline.
 *   - Solver Developers & Verification Engineers: Primary beneficiary (institutional/arbitrage) — leverage the declarative power of SAT/CSP to solve complex design, verification, and optimization problems.
 *   - Complexity Theorists: Analytical observer — studies the structure of the problem space itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The constraint extracts computational time and opportunity. The
% asymmetry between verification (easy) and search (hard) imposes a cost that
% can be leveraged (e.g., in cryptography) or suffered (e.g., in real-time systems).
domain_priors:base_extractiveness(sat_csp_complexity, 0.40).

% Rationale: The hardness of the search suppresses "perfect optimization" and
% guaranteed solutions. The alternative—finding the absolute best solution
% quickly—is suppressed by the combinatorial explosion of the search space.
domain_priors:suppression_score(sat_csp_complexity, 0.50).
domain_priors:theater_ratio(sat_csp_complexity, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sat_csp_complexity, extractiveness, 0.40).
narrative_ontology:constraint_metric(sat_csp_complexity, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(sat_csp_complexity, theater_ratio, 0.03).

% --- Constraint claim (must match analytical perspective type) ---
% This is a Tangled Rope, not a Mountain. While it has features of a mathematical
% law, its high extraction and suppression metrics, combined with its dual role
% as a coordination tool and an extractive mechanism, fit the Tangled Rope signature.
narrative_ontology:constraint_claim(sat_csp_complexity, tangled_rope).
narrative_ontology:human_readable(sat_csp_complexity, "SAT/CSP Computational Complexity").

% --- Binary flags ---
% The "enforcement" is the computational work required to traverse the search
% space. Running a solver is the act of enforcement. This is required for the
% Tangled Rope classification.
domain_priors:requires_active_enforcement(sat_csp_complexity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sat_csp_complexity, solver_developers).
narrative_ontology:constraint_beneficiary(sat_csp_complexity, formal_verification_engineers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sat_csp_complexity, real_time_schedulers).
narrative_ontology:constraint_victim(sat_csp_complexity, embedded_systems_designers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE EMBEDDED SYSTEM (SNARE)
% Agent who bears the most extraction. For a real-time system, the exponential
% search time is a trap that can cause mission failure.
constraint_indexing:constraint_classification(sat_csp_complexity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE VERIFICATION ENGINEER (ROPE)
% Agent who benefits most. For an engineer using a solver, SAT/CSP is a pure
% coordination tool to declaratively find bugs or prove properties.
constraint_indexing:constraint_classification(sat_csp_complexity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes both the coordination function (Rope for
% engineers) and the asymmetric extraction (Snare for systems), classifying
% the overall structure as a Tangled Rope.
constraint_indexing:constraint_classification(sat_csp_complexity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sat_csp_complexity_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap: Snare for the powerless, Rope for the
    % institutional, and Tangled Rope for the analytical observer.
    constraint_indexing:constraint_classification(sat_csp_complexity, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sat_csp_complexity, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(sat_csp_complexity, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_metrics_validation) :-
    % Verify that the base metrics are consistent with a Tangled Rope classification.
    domain_priors:base_extractiveness(sat_csp_complexity, E), E >= 0.30,
    domain_priors:suppression_score(sat_csp_complexity, S), S >= 0.40.

test(tangled_rope_structural_gates) :-
    % Verify that all three structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(sat_csp_complexity, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(sat_csp_complexity, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(sat_csp_complexity).

:- end_tests(sat_csp_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original classification of this constraint as a Mountain was a "false
 *   natural law" error. While NP-completeness is a mathematical fact, its
 *   *implications* are not neutral. The base extractiveness (0.40) and
 *   suppression (0.50) are far too high for a Mountain (ε ≤ 0.25, supp ≤ 0.05).
 *   These scores reflect the real-world cost (in time, energy, and opportunity)
 *   imposed by the constraint.
 *
 * PERSPECTIVAL GAP:
 *   - The powerless embedded system sees a Snare: the search space is a trap
 *     that consumes its limited resources with no guarantee of escape (a solution)
 *     within its operational deadline.
 *   - The institutional verification engineer sees a Rope: modern solvers have
 *     made SAT/CSP a powerful coordination tool. They can declaratively specify
 *     a complex problem and have the solver find a solution, effectively
 *     "pulling" correctness from a complex system.
 *   - The analytical observer sees a Tangled Rope because it recognizes both
 *     the genuine coordination function (the Rope aspect) and the severe,
 *     asymmetric extraction (the Snare aspect).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `solver_developers` and `formal_verification_engineers`
 *     build careers and tools around navigating this complexity. They benefit
 *     from its existence as a structured problem.
 *   - Victims: `real_time_schedulers` and `embedded_systems_designers` bear
 *     the cost. For them, the complexity is not an interesting puzzle but a
 *     hard operational barrier that extracts performance and reliability.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly identifies the dual nature of
 *   the constraint. A pure Snare classification would miss its genuine utility
 *   in formal methods and optimization. A pure Rope classification would ignore
 *   the immense costs it imposes on resource-constrained systems. The original
 *   Mountain classification was a category error, mistaking a mathematical
 *   origin for a lack of structural extraction. The Tangled Rope classification
 *   resolves this by acknowledging the mathematical foundation while correctly
 *   modeling its socio-technical impact. The `requires_active_enforcement`
 *   predicate is justified because the combinatorial penalty is only "enforced"
 *   when one actively tries to find a solution by running a solver.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sat_csp_complexity,
    'Is P=NP? If P=NP, does the entire constraint structure collapse?',
    'A formal proof resolving the P vs. NP problem.',
    'If P=NP, the extractive element vanishes, and the constraint would likely collapse into a pure Rope (a simple coordination problem). If P!=NP, the Tangled Rope structure holds.',
    confidence_without_resolution(high) % High confidence that P!=NP, so the structure is stable.
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sat_csp_complexity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a fundamental mathematical constraint, so its properties are stable
% over time. The measurements reflect this stability. ε < 0.46, so this is not
% strictly required, but included for completeness.
narrative_ontology:measurement(sat_csp_complexity_tr_t0, sat_csp_complexity, theater_ratio, 0, 0.03).
narrative_ontology:measurement(sat_csp_complexity_tr_t5, sat_csp_complexity, theater_ratio, 5, 0.03).
narrative_ontology:measurement(sat_csp_complexity_tr_t10, sat_csp_complexity, theater_ratio, 10, 0.03).

narrative_ontology:measurement(sat_csp_complexity_ex_t0, sat_csp_complexity, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(sat_csp_complexity_ex_t5, sat_csp_complexity, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(sat_csp_complexity_ex_t10, sat_csp_complexity, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It provides a standard logical framework for expressing
% and solving problems.
narrative_ontology:coordination_type(sat_csp_complexity, information_standard).

% Network relationships: The hardness of SAT/CSP is a foundational constraint
% that affects the feasibility and performance of many other algorithmic systems.
narrative_ontology:affects_constraint(sat_csp_complexity, algorithm_performance_guarantees).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately captures the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */