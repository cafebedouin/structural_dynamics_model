% ============================================================================
% CONSTRAINT STORY: traveling_salesman_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_traveling_salesman_problem, []).

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
 *   constraint_id: traveling_salesman_problem
 *   human_readable: Computational Complexity of the Traveling Salesman Problem
 *   domain: technological/computer_science/combinatorial_optimization
 *
 * SUMMARY:
 *   The Traveling Salesman Problem represents a constraint that emerges from
 *   mathematical structure itself, not from institutional design or strategic
 *   choice. Discovered formally in the 1950s (though the problem is much
 *   older), TSP has become the canonical exemplar of NP-completeness: the
 *   class of problems whose solutions can be verified efficiently but
 *   (conjecturally) cannot be found efficiently by any deterministic
 *   algorithm. The constraint is invariant across all observers: a
 *   theoretical computer scientist, a resource-rich algorithm designer, a
 *   logistics practitioner, and an organized research community all face the
 *   same exponential scaling law. This universality makes TSP a paradigm case
 *   of a mountain constraint. The structural data confirms this:
 *   extractiveness ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.92,
 *   resistance ≤ 0.08, emerges_naturally = true. No institutional actor can
 *   negotiate with the law of computational complexity. Approximation
 *   algorithms and heuristics provide practical escape valves, but they do
 *   not escape the constraint itself — they change the problem statement
 *   (from exact to approximate), not the constraint. The theater_ratio
 *   remains low (0.15) because there is no performative component: either an
 *   algorithm solves the problem correctly or it does not.
 *
 * KEY AGENTS:
 *   - Theoretical Computer Scientists: Analytical observers (analytical/analytical) — study TSP as a pure mathematical object; see the constraint as a formal truth independent of application
 *   - Algorithm Designers and Software Engineers: Powerful actors (powerful/mobile) — develop heuristics and approximation algorithms; can solve larger instances but cannot escape exponential bound without approximation
 *   - Logistics and Route Optimization Practitioners: Powerless actors (powerless/trapped) — operate under TSP constraints in real-world applications (delivery, manufacturing, scheduling); must use practical approximations or metaheuristics
 *   - Operations Research Community: Organized actors (organized/constrained) — 70+ years of collective effort to develop heuristics, genetic algorithms, simulated annealing, and other problem-specific optimizations; community consensus that exact solutions remain hard
 *   - Quantum Computing Researchers: Emerging actors exploring alternative computational models — currently no practical quantum advantage for TSP, but this is an omega variable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(traveling_salesman_problem, 0.12).
domain_priors:suppression_score(traveling_salesman_problem, 0.02).
domain_priors:theater_ratio(traveling_salesman_problem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(traveling_salesman_problem, extractiveness, 0.12).
narrative_ontology:constraint_metric(traveling_salesman_problem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(traveling_salesman_problem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(traveling_salesman_problem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(traveling_salesman_problem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(traveling_salesman_problem, mountain).
narrative_ontology:human_readable(traveling_salesman_problem, "Computational Complexity of the Traveling Salesman Problem").
narrative_ontology:topic_domain(traveling_salesman_problem, "technological/computer_science/combinatorial_optimization").

domain_priors:emerges_naturally(traveling_salesman_problem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THEORETICAL COMPUTER SCIENTIST (MOUNTAIN) — The NP-completeness of TSP is a mathematical fact independent of any observer's position. From the civilizational/universal view, TSP's hardness is an immutable property of the combinatorial landscape: there is no escape from exponential search space for exact solutions without additional structure. This is a constraint of formal logic, not of institutional design.
constraint_indexing:constraint_classification(traveling_salesman_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM DESIGNER (MOUNTAIN) — Even for the most resourced actor (powerful, mobile), the computational floor is non-negotiable. A designer with unlimited compute can solve larger instances, but cannot escape the exponential bound without approximation or heuristics. The constraint is natural law, not institutional arrangement. d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.09.
constraint_indexing:constraint_classification(traveling_salesman_problem, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: ROUTE OPTIMIZATION PRACTITIONER (MOUNTAIN) — A logistics operator with immediate needs and local scope faces the same fundamental constraint as the theoretical scientist: TSP is hard by nature, not by institutional suppression. No resources can change the mathematical fact that exact solutions require exponential computation. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.14. Even for the most constrained actor, the constraint is natural law.
constraint_indexing:constraint_classification(traveling_salesman_problem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: OPERATIONS RESEARCH COMMUNITY (MOUNTAIN) — Organized across institutions (universities, corporations, government labs), the OR community has spent 70+ years developing heuristics, approximation algorithms, and problem-specific optimizations. Despite this collective effort, the community has not escaped the fundamental constraint: exact TSP solution remains NP-complete. The hardness is immutable; what varies is the approximation quality achievable in polynomial time.
constraint_indexing:constraint_classification(traveling_salesman_problem, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(traveling_salesman_problem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(traveling_salesman_problem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(traveling_salesman_problem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(traveling_salesman_problem, ExtMetricName, E),
    domain_priors:suppression_score(traveling_salesman_problem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(traveling_salesman_problem),
    narrative_ontology:constraint_metric(traveling_salesman_problem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(traveling_salesman_problem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(traveling_salesman_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. TSP is not an extraction mechanism — it does not redistribute resources or enforce asymmetric costs. The computational cost is universal and intrinsic to the problem structure, not imposed by any actor on others. The small non-zero value reflects that approximation algorithms do introduce tradeoffs (solution quality vs computation time), but these are technical choices, not extraction. Suppression (0.02): Negligible. There are no alternative formulations that suppress escape routes — the NP-completeness is invariant under polynomial-time reduction. Theater ratio (0.15): Very low. The constraint is purely functional: either an algorithm terminates with a solution or it does not. There is no performative ritual, no theater of verification, no symbolic maintenance required. The small non-zero value reflects pedagogical and social elements (teaching TSP as a canonical problem, rituals of presenting approximation algorithms as 'solutions'), but these are not structural to the constraint itself. Accessibility collapse (0.92): Very high. The constraint is completely inaccessible to human intuition or brute-force search for large n. A person cannot mentally enumerate millions of permutations; a computer cannot do so without exponential time. The accessibility floor is determined by the search space size, not by any institutional barrier. Resistance (0.08): Very low. No actor has successfully resisted the constraint or circumvented it. All proposed solutions either (a) find exact answers in exponential time, (b) find approximate answers in polynomial time, or (c) solve restricted versions (metric TSP, Euclidean TSP, small n). These are adaptations, not resistances. The constraint persists universally.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints in the corpus, TSP shows NO perspectival gap. All four perspectives — the theoretical scientist, the powerful designer, the powerless practitioner, and the organized community — classify TSP identically as a mountain. This invariance is the signature of a true natural law constraint. The theoretical scientist sees it as immutable mathematics. The designer sees it as immutable computation. The practitioner sees it as immutable reality. The community sees it as immutable collectively. There is no power asymmetry, no exit option that escapes the constraint, no institutional actor benefiting from suppression. The constraint is pure structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is non-standard for TSP because there is no beneficiary or victim — the constraint is symmetrical across all actors. In the standard framework: all agents experience the constraint equally regardless of power, time horizon, or exit options. The d values across perspectives vary (0.48 for powerful/mobile, 0.95 for powerless/trapped) but they do NOT result in different classifications. All perspectives still compute to mountain because the core metrics (ε=0.12, suppression=0.02) satisfy the mountain gates universally. This is the defining property of a mountain: directionality derives different f(d) values, but ε is so low that no amount of f(d) amplification changes the classification. The constraint is invariant across all observation points.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_advantage_feasibility,
    'Can quantum computers (NISQ or future) provide polynomial-time solutions to TSP, converting a mountain into a coordinate system artifact?',
    'Demonstration of quantum algorithm achieving provably better than classical exponential scaling; fault-tolerant quantum computer with sufficient qubits and coherence time',
    'If yes: TSP transitions from mountain to constraint artifact — the problem''s hardness becomes device-dependent rather than universal. If no: quantum advantage is insufficient; TSP remains a mountain even in the quantum regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_advantage_feasibility, empirical, 'Whether quantum computers can achieve polynomial-time TSP solutions').

omega_variable(
    p_vs_np_resolution,
    'Is P = NP? Does a polynomial-time deterministic algorithm for TSP exist?',
    'Proof of P = NP or P ≠ NP via complexity theory; independent verification by peer review',
    'If P = NP: TSP is a false mountain — efficient solutions exist. If P ≠ NP: TSP is confirmed as an immutable constraint; no efficient exact algorithm exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p_vs_np_resolution, conceptual, 'Resolution of the P vs NP conjecture').

omega_variable(
    approximation_sufficiency,
    'Is practical ''good enough'' approximation (e.g., 1.5-approximation within polynomial time) functionally sufficient to transform TSP from a mountain into a Rope coordination problem in most applications?',
    'Empirical analysis of approximation quality requirements across logistics, scheduling, circuit design applications; cost-benefit analysis of exact vs approximate solutions',
    'If approximation suffices: from an organizational perspective, TSP becomes a manageable coordination problem (Rope) rather than a hard constraint (Mountain). If not: the hardness remains binding even with approximations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(approximation_sufficiency, preference, 'Whether approximation algorithms functionally resolve TSP in practical applications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(traveling_salesman_problem, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsp_tr_t0, traveling_salesman_problem, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tsp_tr_t35, traveling_salesman_problem, theater_ratio, 35, 0.12).
narrative_ontology:measurement(tsp_tr_t70, traveling_salesman_problem, theater_ratio, 70, 0.15).

% Extraction over time
narrative_ontology:measurement(tsp_be_t0, traveling_salesman_problem, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tsp_be_t35, traveling_salesman_problem, base_extractiveness, 35, 0.12).
narrative_ontology:measurement(tsp_be_t70, traveling_salesman_problem, base_extractiveness, 70, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(traveling_salesman_problem, information_standard).
narrative_ontology:affects_constraint(traveling_salesman_problem, np_hardness_gate).
narrative_ontology:affects_constraint(traveling_salesman_problem, combinatorial_optimization_floor).

% DUAL FORMULATION NOTE:
% TSP is upstream of broader NP-hardness and computational complexity constraints. It is not an observable-dependent variant of a single constraint — TSP itself is a single, well-defined problem with a single ε value (0.12). Related problems like Hamiltonian Cycle and Vehicle Routing are structurally distinct constraints with different ε values and different perspectives, linked to TSP via the network dependency graph.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
