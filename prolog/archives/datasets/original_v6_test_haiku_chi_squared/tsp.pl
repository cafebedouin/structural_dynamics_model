% ============================================================================
% CONSTRAINT STORY: tsp
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsp, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tsp
 *   human_readable: Elimination of Duplicate Solutions in Traveling Salesperson Problem Solvers
 *   domain: technological/algorithmic_optimization
 *
 * SUMMARY:
 *   The elimination of duplicate solutions in Traveling Salesperson Problem
 *   solvers presents a structural constraint that bridges computational
 *   necessity and algorithmic choice. TSP solvers (genetic algorithms, ant
 *   colony optimization, simulated annealing, branch-and-bound) generate
 *   candidate solutions iteratively. The core structural problem: a TSP tour
 *   has multiple equivalent representations due to rotational and
 *   reflectional symmetry. A tour [1→2→3→4→1] is identical to [2→3→4→1→2],
 *   [3→4→1→2→3], etc. (rotations), and [4→3→2→1→4] (reflection). Without
 *   deduplication, solvers may evaluate the same solution multiple times,
 *   wasting computational resources and producing inflated diversity metrics.
 *   The constraint operates at three levels: (1) algorithmic (how to
 *   canonicalize tours to prevent redundant evaluation), (2) epistemic (how
 *   to measure solver diversity accurately), and (3) institutional (what
 *   deduplication standard to adopt for benchmarking). The constraint's
 *   extractiveness (0.32) reflects that deduplication imposes computational
 *   overhead and design complexity, but this is compensated by genuine
 *   coordination benefits: standardized benchmarking, faster convergence, and
 *   reproducible comparisons across solver implementations. The theater_ratio
 *   (0.35) indicates moderate performative content — some deduplication
 *   methods are maintained for benchmark conformance rather than actual
 *   efficiency gain.
 *
 * KEY AGENTS:
 *   - Algorithm Designer: Institutional beneficiary (arbitrage exit) — designs the deduplication scheme; benefits from standardized methods enabling publication and comparison
 *   - Solver Implementation: Powerful beneficiary (mobile exit) — computationally executes deduplication; benefits from correct fitness evaluation and prevention of convergence stalling
 *   - Domain Researcher: Moderate victim/beneficiary (constrained exit) — depends on solver toolkits and benchmark standards; benefits from reproducibility but bears computational overhead
 *   - Metaheuristic Framework: Powerful beneficiary (arbitrage exit) — can choose deduplication approach; benefits from enabling correct optimization
 *   - Computational Analyst: Observational (analytical exit) — risks naturalizing a contingent design choice as immutable law
 *   - Open Solver Ecosystem: Organized agent (constrained exit) — maintains TSPLIB benchmarks and standardized deduplication protocols; faces sunset as alternatives emerge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsp, 0.32).
domain_priors:suppression_score(tsp, 0.28).
domain_priors:theater_ratio(tsp, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsp, extractiveness, 0.32).
narrative_ontology:constraint_metric(tsp, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(tsp, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsp, rope).
narrative_ontology:human_readable(tsp, "Elimination of Duplicate Solutions in Traveling Salesperson Problem Solvers").
narrative_ontology:topic_domain(tsp, "technological/algorithmic_optimization").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsp, solver_developers).
narrative_ontology:constraint_beneficiary(tsp, computational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHM DESIGNER (ROPE) — Institutional actor with arbitrage exit (can publish, switch domains, or modify approach). The deduplication constraint solves a genuine coordination problem: representing the solution space efficiently. The designer benefits from standardized deduplication methods that enable comparison across solver implementations. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.04. Net coordination.
constraint_indexing:constraint_classification(tsp, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: SOLVER IMPLEMENTATION (ROPE) — Powerful (computationally capable) actor with mobile exit (can switch representation schemes, accept redundant solutions, or use alternative optimization frameworks). The deduplication constraint provides genuine coordination: it enables correct fitness evaluation and prevents algorithmic stalling on symmetric tours. d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.23. Pure coordination with mutual benefit.
constraint_indexing:constraint_classification(tsp, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: DOMAIN RESEARCHER (TANGLED ROPE) — Moderate power (depends on solver toolkits) with constrained exit (academic career incentives, publication requirements, tool lock-in). Benefits from deduplication via standardized benchmarks and reproducibility. But also bears cost: deduplication methods add computational overhead and obscure the raw solution diversity generated by metaheuristics. d≈0.65, f(d)≈0.98, σ=0.9 → χ≈0.28. Mixed coordination and extraction.
constraint_indexing:constraint_classification(tsp, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: METAHEURISTIC FRAMEWORK (ROPE) — Powerful (standardized, widely deployed) with arbitrage exit (can choose deduplication method, implement or omit). Deduplication is pure coordination: it prevents the framework from collapsing on symmetric solutions and enables benchmarking. d≈0.10, f(d)≈-0.02, σ=1.2 → χ≈-0.01. Near-zero extraction; genuine coordination.
constraint_indexing:constraint_classification(tsp, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPUTATIONAL ANALYST / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the deduplication of symmetric TSP solutions is a natural computational law: rotations and reflections of a tour represent the same solution under the Euclidean or metric space interpretation. This is structurally isomorphic to symmetry reduction in physics. The constraint appears immutable. However, the structural data (ε=0.32, suppression=0.28, theater=0.35) contradicts a mountain classification. The engine flags this as a false summit: deduplication is a legitimate coordination choice, not a law of nature.
constraint_indexing:constraint_classification(tsp, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: OPEN SOLVER ECOSYSTEM (SCAFFOLD) — Organized collective (TSPLIB, solver comparison initiatives, standardized benchmarks) sees deduplication as a temporary coordination mechanism with a sunset: as solvers become more efficient and hardware improves, the computational overhead of deduplication declines, and the need for strict standardization may relax. Deduplication methods are being replaced by more sophisticated dominance rules and hybrid approaches. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.15. Sunset logic evident in emergence of alternative canonicalization strategies.
constraint_indexing:constraint_classification(tsp, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsp_tests).
:- end_tests(tsp_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. Deduplication does extract a computational cost — hashing, tree maintenance, canonicalization routines consume cycles. However, this extraction is not severe because the deduplication benefit (faster convergence, correct fitness evaluation) typically exceeds the overhead. The analysis from the domain researcher perspective (Tangled Rope, d≈0.65) captures the mixed dynamic: researchers benefit from standardized benchmarks but bear the overhead cost. Suppression (0.28): Low-moderate. Solvers are not prevented from omitting deduplication — many metaheuristic frameworks allow deduplication to be toggled. However, institutional pressure (publication benchmarks, TSPLIB standards) suppresses the choice to omit it. Theater ratio (0.35): Moderate-low. Some deduplication overhead is pure theater: maintaining sorted lists or hash tables for conformance to benchmarking standards rather than actual algorithmic necessity. As computational hardware improves, the relative cost of this theater increases. The trajectory shows theater rising (0.15→0.35) as hardware acceleration makes deduplication relatively more expensive compared to raw solver iterations.
 *
 * PERSPECTIVAL GAP:
 *   Algorithm designers and metaheuristic frameworks see deduplication as pure coordination (Rope) — it solves the symmetry problem transparently. Domain researchers see mixed coordination and extraction (Tangled Rope) — they benefit from standardization but bear computational overhead. The open solver ecosystem sees a temporary mechanism with sunset logic (Scaffold) — as hybrid approaches (dominance-based filtering, constraint-based symmetry breaking) mature, traditional explicit deduplication will decline. The analytical observer risks seeing natural law (Mountain) — symmetry reduction appears immutable — but the structural data reveals contingent institutional design. The perspective gap reveals that deduplication is not a single constraint but a layered institutional choice: whether to deduplicate at all, how strictly (geometric vs problem-dependent), and at what granularity (global vs within-population).
 *
 * DIRECTIONALITY LOGIC:
 *   Algorithm designer: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Solver implementation: Beneficiary + mobile → d≈0.48, f(d)≈0.60. Domain researcher: Victim + constrained → d≈0.65, f(d)≈0.98. Metaheuristic framework: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.02. Open ecosystem: Organized + constrained → d≈0.35, f(d)≈0.35. The directionality spread (0.05 to 0.65) shows that institutional constraint (benchmark conformance, tool lock-in) drives domain researchers' higher d values despite the genuine coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that deduplication operates at distinct levels: (1) As pure coordination at the algorithmic level (Rope from designer and implementation perspectives), (2) As mixed coordination-extraction from the institutional level (Tangled Rope from researcher perspective constrained by benchmarking standards), (3) As a transient choice in the ecosystem (Scaffold as alternatives emerge), and (4) As a naturalized law from insufficiently reflexive analysis (false mountain). The constraint does not collapse into a single type because its extractiveness derives from institutional pressure (benchmarking lock-in) rather than from structural necessity. Domain researchers experience extraction partly because deduplication is *institutionally required*, not because it is algorithmically irreplaceable. As alternative canonicalization strategies mature and hardware efficiency improves, the institutional extraction component will decline, potentially reclassifying domain researcher experience from Tangled Rope toward Rope. This lifecycle is diagnostic of institutional rather than natural constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symmetry_definition_boundary,
    'What constitutes a duplicate solution: geometric equivalence only, or equivalence under problem-specific symmetries?',
    'Empirical comparison of solver performance under strict (geometric) vs relaxed (problem-dependent) deduplication rules; analysis of problem instances with asymmetric cost matrices',
    'If strict geometric equivalence: simpler deduplication, higher classification as coordination (Rope). If problem-dependent: more complex deduplication, shifts toward Tangled Rope for some solvers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symmetry_definition_boundary, empirical, 'Definition boundary for solution equivalence').

omega_variable(
    computational_overhead_threshold,
    'At what problem scale does deduplication overhead exceed fitness evaluation cost?',
    'Computational complexity analysis and empirical benchmarking of deduplication cost vs problem size; identification of crossover point where hashing/tree-based deduplication becomes net negative',
    'If crossover < 100 cities: deduplication is overhead burden (extraction). If crossover > 1000 cities: deduplication is enabler (coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_overhead_threshold, empirical, 'Threshold where deduplication overhead exceeds benefit').

omega_variable(
    alternative_canonicalization_sufficiency,
    'Do newer dominance-based and symmetry-breaking constraint approaches achieve duplicate elimination without explicit deduplication data structures?',
    'Comparison of solution diversity and optimality gap: dominance-only solvers vs traditional deduplication-based solvers; analysis of whether implicit filtering is adequate',
    'If sufficient: deduplication is contingent (Scaffold with sunset). If insufficient: deduplication is necessary coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_canonicalization_sufficiency, empirical, 'Whether implicit filtering can replace explicit deduplication').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsp, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsp_tr_t0, tsp, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tsp_tr_t3, tsp, theater_ratio, 3, 0.25).
narrative_ontology:measurement(tsp_tr_t6, tsp, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(tsp_be_t0, tsp, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tsp_be_t3, tsp, base_extractiveness, 3, 0.26).
narrative_ontology:measurement(tsp_be_t6, tsp, base_extractiveness, 6, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsp, information_standard).
narrative_ontology:affects_constraint(tsp, solution_space_exploration).
narrative_ontology:affects_constraint(tsp, metaheuristic_convergence_criteria).

% DUAL FORMULATION NOTE:
% Deduplication in TSP is downstream of the general symmetry reduction problem in combinatorial optimization but represents a distinct institutional constraint. Upstream constraints (the TSP itself, symmetry properties of Euclidean space) have ε near 0.0 (immutable); deduplication has ε=0.32 (contingent institutional choice). The network link shows that improving deduplication efficiency directly impacts convergence criteria and solution space characterization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsp, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
