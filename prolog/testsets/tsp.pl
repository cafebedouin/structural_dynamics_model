% ============================================================================
% CONSTRAINT STORY: tsp
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: computational_optimization/algorithmic_engineering
 *
 * SUMMARY:
 *   The elimination of duplicate solutions in Traveling Salesperson Problem
 *   solvers represents a constraint at the intersection of mathematics,
 *   software engineering, and benchmark standardization. The TSP has a
 *   fundamental symmetry: a circular tour and its reversal represent the same
 *   solution when the cost function is symmetric. This mathematical fact has
 *   been operationalized into a requirement for explicit duplicate
 *   elimination in solution evaluation. However, over four decades of TSP
 *   research and competitive benchmarking (TSPLIB standard since 1985), the
 *   constraint has evolved from a genuine optimization necessity into a mixed
 *   coordination mechanism and convention enforcement system. Modern hardware
 *   increasingly makes the deduplication logic itself more expensive than
 *   evaluating redundant solutions; alternative symmetry-handling methods can
 *   track equivalence without elimination; and open-source solvers are
 *   beginning to question whether benchmark compliance justifies
 *   implementation complexity. The constraint exhibits all six DR types from
 *   different perspectives: the solution space is trapped under suppression
 *   (snare); commercial vendors benefit from it as coordination (rope);
 *   open-source developers face mixed costs and benefits (tangled rope);
 *   benchmark standards maintain it through inertia (piton); hardware
 *   acceleration is creating an exit path with a sunset clause (scaffold);
 *   and from a pure combinatorial perspective, some form of symmetry handling
 *   is unavoidable (mountain).
 *
 * KEY AGENTS:
 *   - Solution Space / Problem Instance: Primary victim (powerless/trapped) — cannot exit the deduplication constraint; restricted to canonical orbit representatives
 *   - Academic Replication Groups: Secondary victim (powerless/trapped) — must implement proprietary deduplication heuristics to match published results; suppression of exploratory approaches
 *   - Commercial Optimization Vendors: Primary beneficiary (institutional/arbitrage) — capture efficiency gains and patent heuristic variants; arbitrage options allow licensing and proprietary implementation
 *   - Open-Source Algorithm Developer Community: Mixed victim/beneficiary (organized/constrained) — benefit from faster convergence but pay coordination cost; constrained exit due to benchmark comparability
 *   - Benchmark Standardization Bodies (TSPLIB, etc.): Institutional actor (institutional/arbitrage) — maintain duplicate-elimination rules through inertia; low functional necessity but high backward-compatibility lock
 *   - Hardware-Accelerated Solver Implementations: Organized agents (powerful/mobile) — see deduplication as temporary constraint bypassed by architectural change; expect sunset through hardware speedup
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsp, 0.38).
domain_priors:suppression_score(tsp, 0.48).
domain_priors:theater_ratio(tsp, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsp, extractiveness, 0.38).
narrative_ontology:constraint_metric(tsp, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(tsp, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsp, tangled_rope).
narrative_ontology:human_readable(tsp, "Elimination of Duplicate Solutions in Traveling Salesperson Problem Solvers").
narrative_ontology:topic_domain(tsp, "computational_optimization/algorithmic_engineering").

domain_priors:requires_active_enforcement(tsp).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsp, algorithm_developers).
narrative_ontology:constraint_beneficiary(tsp, commercial_optimization_vendors).
narrative_ontology:constraint_victim(tsp, solution_space_exploration).
narrative_ontology:constraint_victim(tsp, academic_replication_efforts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOLUTION SPACE EXPLORATION (SNARE) — The solution space as an epistemic domain cannot exit the duplicate elimination constraint. It bears the full cost: equivalent solutions are suppressed, solution diversity is artificially constrained, and exploratory search algorithms cannot freely enumerate the orbits they are designed to traverse. Maximum extraction with no alternatives available.
constraint_indexing:constraint_classification(tsp, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC REPLICATION GROUPS (SNARE) — Small research teams attempting to reproduce published TSP solver results face suppression: they must implement the same duplicate-elimination heuristics to match published benchmarks, but those heuristics are often proprietary or poorly documented. They cannot exit without losing comparability. High suppression, high extraction of effort toward reimplementation rather than innovation.
constraint_indexing:constraint_classification(tsp, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMERCIAL OPTIMIZATION VENDORS (ROPE) — Benefit from the duplicate-elimination constraint as a coordination mechanism: it enables them to claim solution quality improvements through faster convergence (fewer redundant evaluations), to patent heuristic variants, and to maintain proprietary algorithm libraries. They experience the constraint as coordination: 'our deduplication method finds better solutions faster.' Net beneficiaries with arbitrage options (can license, implement alternatives, or license out to competitors).
constraint_indexing:constraint_classification(tsp, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALGORITHM DEVELOPER COMMUNITY (TANGLED ROPE) — Organized open-source developers face a hybrid constraint. Benefit: duplicate elimination accelerates their solvers and reduces runtime overhead, enabling them to compete with commercial implementations. Extraction: they must implement complex deduplication logic, face literature fragmentation (different authors use different deduplication strategies), and cannot freely explore alternative solution enumeration schemes without breaking benchmark comparability. Constrained exit — they need duplicate elimination to remain competitive but pay a significant coordination cost.
constraint_indexing:constraint_classification(tsp, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TSP BENCHMARK STANDARDIZATION BODIES (PITON) — TSPLIB and similar benchmark repositories maintain duplicate-elimination rules largely through institutional inertia. The original rationale (faster testing, clearer comparison) is still functionally present but increasingly performative: modern hardware can enumerate and evaluate redundant solutions faster than the logic to suppress them. The benchmark standards persist because changing them would invalidate 40 years of published results and comparison tables. Theater ratio reflects that the suppression mechanism is mostly about maintaining canonical benchmark status rather than genuine efficiency gains.
constraint_indexing:constraint_classification(tsp, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HARDWARE-ACCELERATED SOLVERS (SCAFFOLD) — GPU and quantum-inspired solvers can evaluate solutions faster than CPU-based deduplication logic can suppress them. These solvers see duplicate elimination as a temporary constraint being bypassed by architectural change: as quantum annealing and tensor-core hardware mature, the cost of redundant evaluation drops below the cost of tracking duplicates. This perspective expects a sunset: in 5-15 years, hardware acceleration will make duplicate suppression unnecessary through brute-force speedup rather than clever algorithms.
constraint_indexing:constraint_classification(tsp, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COMBINATORIAL INVARIANT VIEW (MOUNTAIN) — From a universal/civilizational perspective, duplicate elimination in TSP is mathematically inevitable: a circular tour and its reversal represent the same solution in the symmetry group of the problem. This is not a contingent institutional choice but a structural property of the solution space. Any TSP solver must account for this symmetry or be mathematically incomplete. However, the claim requires scrutiny: the symmetry is real, but whether 'elimination' vs 'equivalence-class tracking' vs 'solution-weighting by symmetry' are forced or contingent remains open.
constraint_indexing:constraint_classification(tsp, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsp_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tsp, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tsp, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(tsp, TR),
    TR >= 0.70.

:- end_tests(tsp_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts complexity cost from developers — they must implement deduplication logic, maintain compatibility with benchmark standards, and restrict solution exploration strategies. However, the extraction is not severe because: (1) deduplication is mathematically justified for some TSP variants, (2) commercial vendors can arbitrage alternative implementations, and (3) hardware acceleration is starting to bypass the constraint entirely. The trajectory shows increasing extractiveness over time (0.22 → 0.38) as problem instances grow and deduplication logic becomes a larger fraction of solver runtime. Suppression (0.48): Moderate. Significant barriers include benchmark standardization (changing rules would invalidate comparisons), literature lock-in (papers describe specific deduplication strategies), and competitive pressure (solvers must match benchmarks to be taken seriously). But suppression is not total — open-source alternatives exist, academic freedom to experiment remains, and some solvers bypass deduplication through alternative symmetry-handling. Theater ratio (0.58): Moderate-high. The justification for duplicate elimination increasingly involves benchmark compliance and historical precedent rather than algorithmic necessity. Hardware speedup is reducing the genuine efficiency argument; the performative aspect (maintaining canonical comparison tables) is rising. Theater increased over the interval as hardware made redundant evaluation cheaper relative to deduplication logic.
 *
 * PERSPECTIVAL GAP:
 *   Commercial vendors see duplicate elimination as coordination (Rope perspective): it enables them to publish faster benchmarks, claim solution quality improvements, and differentiate on deduplication algorithms. Academic replicators see it as extraction (Snare perspective): they must invest effort in implementation to match published results but gain no efficiency benefit from deduplication itself — the cost is pure overhead. Open-source developers occupy the middle ground (Tangled Rope): they benefit from faster solvers through deduplication but pay the cost of complex logic maintenance and lose the freedom to experiment with alternative enumeration schemes. Benchmark bodies see it as inertial maintenance (Piton): the original rationale (efficiency) is weakening, but the institutional lock-in (40 years of published results) forces continuation. Hardware accelerators see it as temporary (Scaffold): as GPU and quantum solvers mature, they will make deduplication unnecessary through brute-force speedup. The analytical observer risks seeing it as mathematical inevitability (Mountain): TSP solutions have rotational symmetry, so some form of symmetry handling is necessary. But the structural data reveals that explicit elimination is contingent — other symmetry-handling approaches (orbit weighting, normalization) could replace it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural relationships: (1) Commercial vendors benefit from deduplication (low d, derived from beneficiary status + arbitrage exit); (2) Academic replicators bear costs without equivalent benefit (high d, derived from victim status + trapped exit due to benchmark lock-in); (3) Open-source developers experience mixed extraction and benefit (moderate d, from both beneficiary and victim status + constrained exit); (4) Hardware accelerators have high exit capacity (low d, derived from mobile exit + powerful agent status, though they face constrained exit to competitive markets — refined as 'mobile' for true hardware differentiation). Beneficiary status (algorithm developers, vendors) generates low d because these agents derive measurable benefit from the constraint and retain options. Victim status (solution space exploration, replication efforts) generates high d because these agents bear costs with limited alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that duplicate elimination exists as both a mathematical necessity (symmetry is real) and a software/institutional contingency (elimination is one choice among alternatives). The constraint is legitimately tangled_rope because it serves both coordination (faster evaluation through deduplication) and extraction (imposed complexity through benchmark compliance). The mountain perspective (analytical observer) is a false summit — it mistakes the inevitability of symmetry handling with the contingency of elimination. Alternative methods (equivalence-class tracking, orbit weighting, rotational-invariant normalization) are mathematically equivalent but would shift the institutional landscape (benchmark incomparability, literature fragmentation). The tangled_rope classification persists because the extraction component — the suppression of alternative symmetry-handling methods through benchmark standardization — is real and asymmetric: vendors benefit from the current scheme, developers pay the complexity cost. Scaffold classification for hardware-accelerated solvers is justified because the sunset mechanism is empirically observable: as hardware speedup exceeds deduplication overhead, the constraint decays naturally without policy change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symmetry_classification_necessity,
    'Is explicit duplicate elimination a mathematical necessity or a software engineering choice driven by benchmark conventions?',
    'Comparison of solver performance with and without duplicate-elimination logic across diverse instance sizes and types; analysis of whether equivalence-class weighting produces results identical to elimination',
    'If necessary: mountain classification confirmed — the constraint reflects a true combinatorial invariant. If contingent: tangled_rope classification confirmed — duplicate elimination is a coordination convention that extracts complexity cost from solvers that could track equivalence classes instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symmetry_classification_necessity, empirical, 'Whether duplicate elimination is mathematically forced or contingently chosen').

omega_variable(
    benchmark_backward_compatibility_lock,
    'How much of the duplicate-elimination requirement is driven by the need to maintain backward compatibility with 40 years of published TSP benchmarks?',
    'Survey of solver implementations to identify which deduplication strategies are used for mathematical correctness vs benchmark compliance; cost-benefit analysis of re-running benchmark corpus with relaxed deduplication rules',
    'If high backward-compatibility lock (>70%): piton classification confirmed — the constraint is maintained through institutional inertia. If low (<30%): rope or tangled_rope confirmed — the constraint reflects genuine algorithmic necessity or coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_backward_compatibility_lock, empirical, 'Extent of backward-compatibility constraints on deduplication requirements').

omega_variable(
    hardware_acceleration_sunset_timeline,
    'At what hardware speedup threshold does duplicate elimination become computationally negligible (cost of deduplication > cost of redundant evaluation)?',
    'Benchmark suite testing with variable hardware acceleration levels; cost modeling for future GPU and quantum annealing systems; comparison of trajectory curves for deduplication logic vs raw evaluation speedup',
    'If threshold reached in <5 years: scaffold sunset is imminent, constraint will decay rapidly. If >15 years: scaffold is aspirational, duplicate elimination will remain enforced. If never (deduplication complexity scales with problem size): mountain classification may be correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_acceleration_sunset_timeline, empirical, 'Timeline for hardware acceleration to make deduplication computationally negligible').

omega_variable(
    alternative_symmetry_handling_equivalence,
    'Do alternative approaches (solution weighting by symmetry orbit size, equivalence-class tracking, rotational-invariance normalization) produce identical optimization results to explicit elimination?',
    'Implementation of alternative symmetry-handling methods; comparison of solution quality, convergence rate, and computational cost across benchmark suite; analysis of whether they are mathematically equivalent or produce subtly different solver behaviors',
    'If equivalent: deduplication is a choice, not a requirement — the constraint is tangled_rope (coordination choice with extraction cost). If non-equivalent: deduplication is necessary — the constraint approaches mountain (combinatorial invariant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_symmetry_handling_equivalence, empirical, 'Whether alternative symmetry-handling methods are equivalent to explicit deduplication').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsp, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsp_tr_t0, tsp, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tsp_tr_t10, tsp, theater_ratio, 10, 0.48).
narrative_ontology:measurement(tsp_tr_t20, tsp, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(tsp_be_t0, tsp, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(tsp_be_t10, tsp, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(tsp_be_t20, tsp, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsp, enforcement_mechanism).
narrative_ontology:affects_constraint(tsp, vehicle_routing_problem_distance_symmetry).
narrative_ontology:affects_constraint(tsp, quadratic_assignment_symmetry_handling).

% DUAL FORMULATION NOTE:
% Duplicate elimination in TSP is upstream of related combinatorial optimization constraints (VRP, QAP, facility location) that face similar symmetry-handling requirements. The TSP constraint has ε=0.38 (tangled rope with institutional inertia) while downstream constraints often face steeper extraction if they adopt TSPLIB standards without questioning necessity. TSP serves as proof-of-concept for whether benchmark-driven constraints can be relaxed when hardware makes their original justification obsolete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsp, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
