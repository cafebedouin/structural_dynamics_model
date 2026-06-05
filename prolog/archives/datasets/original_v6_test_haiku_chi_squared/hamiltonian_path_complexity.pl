% ============================================================================
% CONSTRAINT STORY: hamiltonian_path_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hamiltonian_path_complexity, []).

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
 *   constraint_id: hamiltonian_path_complexity
 *   human_readable: Computational Complexity of the Hamiltonian Path Problem
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   The Hamiltonian Path problem represents a fundamental computational
 *   limit: given a finite graph, determining whether a path exists that
 *   visits each vertex exactly once is NP-complete. This means the problem is
 *   in NP (a proposed solution can be verified in polynomial time), and every
 *   NP problem reduces to it in polynomial time. By the Cook-Levin theorem,
 *   no polynomial-time algorithm can solve Hamiltonian Path in the worst case
 *   unless P = NP, which is widely believed (but unproven) to be false. This
 *   constraint is invariant across all known computational substrates and
 *   persists across all implementation strategies. It is a mathematical
 *   limit, not a contingent institutional arrangement, regulatory choice, or
 *   technological limitation that might be overcome with sufficient
 *   engineering effort. The constraint exhibits the signature of a Mountain:
 *   emerges naturally from the logical structure of NP-completeness, has high
 *   accessibility collapse (no meaningful workaround exists for the worst
 *   case), low resistance (the mathematical proof is robust and universally
 *   accepted), low extractiveness (no agent benefits at the expense of
 *   others), and minimal suppression (the constraint is transparent and
 *   non-coercive).
 *
 * KEY AGENTS:
 *   - Computational Theorist: Analyst (analytical/analytical) — observes the universal mathematical limit; establishes NP-completeness proof
 *   - Algorithm Designer: Powerful agent (powerful/mobile) — seeks optimal solutions; constrained by worst-case hardness but free to pursue heuristics and approximations
 *   - Application Domain: Institutional actor (institutional/arbitrage) — logistics, chip design, bioinformatics; benefits from practical workarounds but cannot escape the fundamental limit
 *   - Solver / Computational Agent: Powerless agent (powerless/trapped) — a computer executing the algorithm; has no exit from exponential state space in worst case
 *   - Research Community: Organized collective (organized/constrained) — decades of collective effort have established the hardness; perception is of an immutable ceiling, not unsolved problem
 *   - Practitioner: Moderate agent (moderate/mobile) — uses heuristics and approximations; understands the optimality gap as a real constraint, not a failure of current algorithms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hamiltonian_path_complexity, 0.12).
domain_priors:suppression_score(hamiltonian_path_complexity, 0.03).
domain_priors:theater_ratio(hamiltonian_path_complexity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hamiltonian_path_complexity, extractiveness, 0.12).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hamiltonian_path_complexity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hamiltonian_path_complexity, mountain).
narrative_ontology:human_readable(hamiltonian_path_complexity, "Computational Complexity of the Hamiltonian Path Problem").
narrative_ontology:topic_domain(hamiltonian_path_complexity, "technological/mathematical").

domain_priors:emerges_naturally(hamiltonian_path_complexity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL THEORIST (MOUNTAIN) — The Hamiltonian Path problem is NP-complete: deciding whether a path exists is in NP; SAT reduces to it in polynomial time; therefore no polynomial algorithm can solve it unless P=NP (widely believed false). This is a mathematical limit, not a contingent institutional arrangement. ε=0.12, suppression=0.03 — accessibility_collapse=0.92, resistance=0.08 establish the natural law signature.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM DESIGNER (MOUNTAIN) — Even with unlimited resources, optimal Hamiltonian Path solving requires exponential time in the worst case (by the Cook-Levin theorem). Heuristic approximations, branch-and-bound, and randomized algorithms improve practical performance but cannot escape the worst-case lower bound. The constraint persists across all implementation strategies. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: INDUSTRY APPLICATION (MOUNTAIN) — Routing, scheduling, and TSP-variant problems in logistics, chip design, and bioinformatics all map to Hamiltonian-type constraints. Practitioners cannot 'choose' a polynomial algorithm — the mathematical structure forces exponential search under worst-case inputs. Workarounds (greedy heuristics, constraint relaxation) work for practical instances but do not invalidate the underlying limit. d≈0.15, f(d)≈0.00, σ=1.2 → χ≈0.00.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOLVER / COMPUTATIONAL AGENT (MOUNTAIN) — A computer tasked with solving large Hamiltonian Path instances has no exit. It must explore exponentially many states in worst case. Cannot negotiate, delegate, or reframe the problem away. The complexity ceiling is absolute. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: RESEARCH COMMUNITY (MOUNTAIN) — Decades of collective effort (Karp, Impagliazzo, Arora, Barak) have proven the hardness is fundamental, not a sign of insufficient ingenuity. The community perceives this as an immutable ceiling — research effort flows toward approximation, parameterized complexity, and special cases, not toward 'solving' the NP-completeness. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRACTITIONER (MOUNTAIN) — A local routing company can find good (not optimal) tours quickly using nearest-neighbor, genetic algorithms, or simulated annealing. But practitioners understand this as a practical workaround, not a solution to the underlying constraint. The optimality gap is real, bounded by the NP-hardness ceiling. d≈0.65, f(d)≈0.95, σ=0.8 → χ≈0.09.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hamiltonian_path_complexity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, ExtMetricName, E),
    domain_priors:suppression_score(hamiltonian_path_complexity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hamiltonian_path_complexity),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hamiltonian_path_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. No agent extracts from others. The constraint is a natural law, not a power asymmetry. Suppression (0.03): Minimal. The constraint is transparent and universally understood. There is no hidden coercion or alternatives suppressed by institutional design. Theater ratio (0.15): Very low. The Hamiltonian Path problem has a clear mathematical definition and robust proof of hardness. There is no performative element — the constraint is what it claims to be. Accessibility collapse (0.92): Very high. The worst-case exponential bound is inaccessible to all agents (powerless through institutional). No amount of resources, cleverness, or coordination can solve arbitrary large instances in polynomial time. Resistance (0.08): Very low. The NP-completeness proof is robust across 50+ years of verification, peer review, and attempts at disproof. The mathematical structure is ironclad.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates near-perfect invariance across all perspectives: every agent, from powerless to analytical, from immediate to civilizational timescale, from local to universal scope, classifies it as Mountain. The perspectival gap is minimal because the constraint is fundamental, not institutional. Minor variation exists in how different agents experience the constraint (practitioners see workarounds; theorists see proofs), but all perspectives agree on the core classification. This is a mountain-only constraint by design — it is one of the canonical exemplars of a natural law in mathematics.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims. The standard directionality derivation (beneficiary + exit → d) does not apply because there is no power asymmetry. All agents face the same constraint. The canonical fallback values for agent_power atoms are used to populate perspectives, but they do not determine d or f(d) — instead, all perspectives converge on the same classification because the constraint is truly invariant. The constraint is not enforced; it emerges naturally from logical structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CONFIRMED: The Hamiltonian Path problem exhibits all five signature properties of a mountain: (1) emerges_naturally = true (flows from NP-completeness axioms, not institutional design), (2) accessibility_collapse = 0.92 (no workaround reaches worst-case polynomial time), (3) resistance = 0.08 (proof is robust, no credible alternative classification), (4) extractiveness = 0.12 (no asymmetric benefit), (5) suppression = 0.03 (constraint is transparent). The minimal theater_ratio (0.15) confirms the constraint is not performative — the mathematical definition and proof are what they appear to be. No mandatrophy exists because there is no mislabeling risk: the constraint is genuinely fundamental, not a coordination problem disguised as extraction or vice versa. The omega on P vs NP recognizes the single irreducible uncertainty: IF P = NP (vanishingly unlikely), the classification shifts to rope or piton; otherwise, the mountain is definitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_vs_np_status,
    'Is P = NP?',
    'Mathematical proof or counterexample establishing the relationship between polynomial and non-deterministic polynomial time classes',
    'If P ≠ NP: Hamiltonian Path hardness is definitively fundamental (mountain confirmed). If P = NP: polynomial algorithm exists; constraint becomes epistemic rather than computational (classification shifts to rope/piton depending on algorithm accessibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p_vs_np_status, empirical, 'Whether the P versus NP problem is resolved').

omega_variable(
    quantum_advantage_scope,
    'Can quantum computers (Grover''s algorithm, quantum annealing) achieve sub-exponential speedup for Hamiltonian Path on large instances?',
    'Empirical demonstration of quantum advantage on instances >100 vertices; theoretical analysis of quantum algorithm runtime bounds; scalability proof beyond current experimental capacities',
    'If strong quantum advantage demonstrated: mountain classification persists for classical agents, but quantum agents see a different constraint (coordinate two stories). If no advantage: classical NP-hardness is the fundamental limit across all known computational substrates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_advantage_scope, empirical, 'Whether quantum computers achieve sub-exponential speedup').

omega_variable(
    approximation_certification_completeness,
    'For Hamiltonian Path, is there a constant-factor approximation algorithm with polynomial runtime and certified bounds?',
    'Proof of existence/non-existence of c-approximation for fixed c > 1; complexity classification of approximation hardness',
    'If yes: practical workarounds have formal guarantees; practitioners see a rope (coordination around approximate solutions) rather than pure mountain. If no: even approximate solutions are hard; constraint is more severe than classical NP-completeness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(approximation_certification_completeness, empirical, 'Whether constant-factor approximation is feasible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hamiltonian_path_complexity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ham_tr_t0, hamiltonian_path_complexity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ham_tr_t25, hamiltonian_path_complexity, theater_ratio, 25, 0.15).
narrative_ontology:measurement(ham_tr_t50, hamiltonian_path_complexity, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(ham_be_t0, hamiltonian_path_complexity, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ham_be_t25, hamiltonian_path_complexity, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(ham_be_t50, hamiltonian_path_complexity, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hamiltonian_path_complexity, information_standard).
narrative_ontology:affects_constraint(hamiltonian_path_complexity, traveling_salesman_hardness).
narrative_ontology:affects_constraint(hamiltonian_path_complexity, graph_isomorphism_computational_limit).
narrative_ontology:affects_constraint(hamiltonian_path_complexity, vertex_cover_approximation_ceiling).

% DUAL FORMULATION NOTE:
% The Hamiltonian Path problem is NP-complete, making it equivalent (in hardness) to other NP-complete problems (SAT, TSP, vertex cover). These constraints form a family linked by polynomial-time reductions. A breakthrough on Hamiltonian Path would immediately resolve all NP-complete problems. The network relationships capture this dependency structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
