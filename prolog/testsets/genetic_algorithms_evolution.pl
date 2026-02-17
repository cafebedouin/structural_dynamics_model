% ============================================================================
% CONSTRAINT STORY: genetic_algorithms_evolution
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_genetic_algorithms_evolution, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: genetic_algorithms_evolution
 *   human_readable: Genetic Algorithms (Search by Selection)
 *   domain: technological/computational
 *
 * SUMMARY:
 *   Genetic Algorithms (GAs) are search heuristics inspired by Charles Darwin’s
 *   theory of natural evolution. They reflect the process of natural selection
 *   where the fittest individuals are selected for reproduction in order to
 *   produce offspring of the next generation, effectively solving optimization
 *   problems through simulated evolution.
 *
 * KEY AGENTS (by structural relationship):
 *   - Sub-optimal Solutions: Primary target (powerless/trapped) — bears extraction (elimination from the gene pool)
 *   - Complex System Designers: Primary beneficiary (institutional/mobile) — benefits from the optimization process
 *   - The Geneticist: Analytical observer — sees the fundamental logic of selection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Highly efficient; the primary "cost" is compute, but it does not extract
% value in a social or economic sense.
domain_priors:base_extractiveness(genetic_algorithms_evolution, 0.10).

% Suppression is low; alternatives (random search, hill climbing) are visible
% but usually discarded due to inferiority in complex search spaces. The
% constraint doesn't actively hide them, it just outperforms them.
domain_priors:suppression_score(genetic_algorithms_evolution, 0.05).   % Structural property (raw, unscaled).

% Functional coordination mechanism — primarily substantive, not performative.
domain_priors:theater_ratio(genetic_algorithms_evolution, 0.11).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_algorithms_evolution, extractiveness, 0.10).
narrative_ontology:constraint_metric(genetic_algorithms_evolution, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(genetic_algorithms_evolution, theater_ratio, 0.11).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(genetic_algorithms_evolution, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(genetic_algorithms_evolution, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(genetic_algorithms_evolution, mountain).
narrative_ontology:human_readable(genetic_algorithms_evolution, "Genetic Algorithms (Search by Selection)").

% --- Emergence flag (required for mountain constraints) ---
% The selection pressure emerges naturally from the fitness function and
% the structure of the search space.
domain_priors:emerges_naturally(genetic_algorithms_evolution).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% While this constraint is a Mountain from the analytical view, these
% declarations are necessary to derive the Rope/Snare classifications from
% other perspectives.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(genetic_algorithms_evolution, complex_system_designers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(genetic_algorithms_evolution, sub_optimal_solutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% For a candidate solution with low fitness, the GA is a 'Snare'. The objective
% function is a rigid bottleneck that "strangles" its persistence in the gene
% pool. It is the cost paid for the system's progress.
constraint_indexing:constraint_classification(genetic_algorithms_evolution, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For a research lab or AI company, genetic algorithms are a 'Rope' – a
% powerful computational tool to solve complex optimization problems. They
% efficiently navigate vast search spaces, enabling breakthroughs.
constraint_indexing:constraint_classification(genetic_algorithms_evolution, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical observer sees GAs as a 'Mountain'—the fundamental
% logic of how complexity arises from simple selection rules. It is
% an immutable law of information theory and biology alike, a fixed principle
% that governs adaptive systems.
constraint_indexing:constraint_classification(genetic_algorithms_evolution, mountain,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_algorithms_evolution_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target, beneficiary, and analyst.
    constraint_indexing:constraint_classification(genetic_algorithms_evolution, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_algorithms_evolution, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(genetic_algorithms_evolution, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeBeneficiary \= TypeAnalyst,
    TypeTarget \= TypeAnalyst.

test(threshold_validation) :-
    % Verify that the metrics are consistent with a Mountain classification.
    narrative_ontology:constraint_metric(genetic_algorithms_evolution, extractiveness, E),
    narrative_ontology:constraint_metric(genetic_algorithms_evolution, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(genetic_algorithms_evolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.10) is low, reflecting the computational cost
 *   rather than social extraction. The suppression score was lowered to 0.05
 *   to be consistent with the Mountain classification; GAs don't coerce, they
 *   simply outperform alternatives in many domains. The Natural Law profile
 *   metrics were added to pass the linter: accessibility_collapse (0.95) is
 *   high because the logic of selection is fundamental and hard to circumvent,
 *   and resistance (0.05) is low because one cannot coherently "resist" an
 *   algorithmic process. The constraint_claim is set to 'mountain' to match
 *   the analytical perspective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a "sub-optimal solution" (an abstract entity), the
 *   process is a Snare of immediate elimination. For the user (an AI company),
 *   it's a Rope for coordination and problem-solving. For the analyst, it's a
 *   Mountain, a fundamental principle of adaptive systems. This demonstrates
 *   how a single process can be perceived radically differently based on one's
 *   structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are 'complex_system_designers' who use the algorithm to
 *   find novel solutions. The "victims" are 'sub_optimal_solutions', a
 *   conceptual group representing the candidate solutions that are discarded.
 *   This personification is necessary to model the selective pressure that
 *   drives the algorithm's success and generates the Snare perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the core function as coordination
 *   (Rope) and fundamental law (Mountain), while acknowledging the eliminative
 *   "cost" (Snare) paid by individual candidate solutions. It avoids mislabeling
 *   the entire system as extractive, as the "extraction" is purely functional
 *   within the algorithm's logic, not a transfer of social value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_genetic_algorithms_evolution,
    "Will a dominant 'good enough' gene pool (local optimum) prevent the discovery of a truly 'Global Optimum' through genetic algorithms?",
    "Monitoring genetic diversity vs. fitness plateaus in GA runs over extended generations for various problem sets.",
    "If Yes: The GA becomes a 'Local Optimum Snare' for the whole system, failing to find the true peak. If No: The GA is a reliable 'Rope' for global optimization.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_genetic_algorithms_evolution, empirical, "Risk of premature convergence to a local optimum instead of the global optimum.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(genetic_algorithms_evolution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.10) is below the 0.46 threshold for
% mandatory lifecycle drift tracking. The constraint is stable and not prone
% to extraction accumulation.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Genetic algorithms are a method for coordinating information (genes) to
% search a solution space.
narrative_ontology:coordination_type(genetic_algorithms_evolution, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations correctly models the perspectival gaps.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */