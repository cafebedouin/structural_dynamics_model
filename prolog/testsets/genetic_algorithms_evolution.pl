% ============================================================================
% CONSTRAINT STORY: genetic_algorithms_evolution
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Holland, J. H. (1992). Adaptation in Natural and Artificial Systems.
% ============================================================================

:- module(constraint_genetic_algorithms, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: genetic_algorithms_evolution
 * human_readable: Genetic Algorithms (Search by Selection)
 * domain: technological/biological
 * temporal_scope: Biographical to Civilizational
 * spatial_scope: Global
 * * SUMMARY:
 * Genetic Algorithms (GAs) are search heuristics inspired by Charles Darwin’s 
 * theory of natural evolution. They reflect the process of natural selection 
 * where the fittest individuals are selected for reproduction in order to 
 * produce offspring of the next generation.
 * * KEY AGENTS:
 * - The Chromosome: A candidate solution encoded as a string of parameters.
 * - The Fitness Function: The objective evaluator that determines "survival."
 * - The Breeder: The algorithm designer who sets the mutation and crossover rates.
 * * NARRATIVE ARC:
 * GAs function as a "Massively Parallel Rope." Instead of a single climber 
 * attacking a Mountain, a population of agents explores the landscape. 
 * Through "Mutation," they jump across valleys to avoid the "Local Optimum Noose."
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(genetic_algorithms_evolution, 0, 10).
narrative_ontology:constraint_claim(genetic_algorithms_evolution, rope).

% Base extractiveness score (0.1)
% Highly efficient; the primary "cost" is the compute required for generations.
domain_priors:base_extractiveness(genetic_algorithms_evolution, 0.1).

% Suppression score (0.2)
% Alternatives (random search, hill climbing) are visible but usually 
% discarded due to inferiority in complex spaces.
domain_priors:suppression_score(genetic_algorithms_evolution, 0.2).

% Enforcement requirements
domain_priors:emerges_naturally(genetic_algorithms_evolution).

% Metrics 
narrative_ontology:constraint_metric(genetic_algorithms_evolution, extractiveness, 0.1).
narrative_ontology:constraint_metric(genetic_algorithms_evolution, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(genetic_algorithms_evolution, complex_system_designers).
constraint_victim(genetic_algorithms_evolution, weak_candidate_solutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SUCCESSFUL OFFSPRING - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile (high mutation potential)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For a solution that has been "selected," the GA is a "Rope." It has 
   provided the path to a high-fitness state. The constraints of the 
   environment served as the coordination mechanism that allowed it to 
   outperform its peers.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    genetic_algorithms_evolution,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(genetic_algorithms_evolution, E),
    E < 0.2,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UNFIT INDIVIDUAL - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate (single generation)
   WHERE: trapped (cannot improve fast enough to survive)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a solution with low fitness, the GA is a "Noose." The objective 
   function is a rigid bottleneck that "strangles" their persistence 
   in the gene pool. They are the cost paid for the system's progress.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    genetic_algorithms_evolution,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GENETICIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The analytical observer sees GAs as a "Mountain"—the fundamental 
   logic of how complexity arises from simple selection rules. It is 
   an immutable law of information theory and biology alike.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    genetic_algorithms_evolution,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(genetic_algorithms_evolution),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(genetic_algorithm_tests).

test(fitness_bottleneck) :-
    % Low fitness/powerless agents experience the Noose of selection.
    constraint_indexing:constraint_classification(genetic_algorithms_evolution, noose, context(individual_powerless, _, trapped, _)).

test(evolutionary_mountain) :-
    % The meta-logic of evolution is seen as a Mountain by observers.
    constraint_indexing:constraint_classification(genetic_algorithms_evolution, mountain, context(analytical, _, _, _)).

:- end_tests(genetic_algorithm_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * GAs are the "Multi-Agent" version of Heuristic Optimization. 
 * I set extractiveness to 0.1 because while individuals "die" (Noose), 
 * the system's value increases without coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    premature_convergence,
    "Will a dominant 'good enough' gene pool prevent the discovery of a 'Global Optimum'?",
    resolution_mechanism("Monitoring genetic diversity vs. fitness plateaus over 10^6 generations"),
    impact("If Yes: The GA becomes a 'Local Optimum Noose' for the whole system."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
