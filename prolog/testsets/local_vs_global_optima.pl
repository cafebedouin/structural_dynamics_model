% ============================================================================
% CONSTRAINT STORY: local_vs_global_optima
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Optimization Theory / Genetic Algorithms / Calculus
% ============================================================================

:- module(constraint_optimization_landscapes, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: local_vs_global_optima
 * human_readable: Local vs. Global Optima (The Peak Trap)
 * domain: mathematical/technological/biological
 * temporal_scope: Immediate to Civilizational
 * spatial_scope: Global
 * * SUMMARY:
 * A "Local Optimum" is a solution that is better than all nearby solutions 
 * but inferior to the "Global Optimum"—the best possible solution across 
 * the entire system.
 * * KEY AGENTS:
 * - The Hill-Climber: An agent that always moves toward immediate improvement.
 * - The Explorer: An agent that occasionally moves "downhill" to find 
 * a different, potentially higher mountain.
 * - The Landscape: The mathematical reality defining the peaks and valleys.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(local_vs_global_optima, 0, 10).
narrative_ontology:constraint_claim(local_vs_global_optima, mathematical_limit).

% Base extractiveness score (0.3)
% Sub-optimality extracts potential value from the system; being stuck at 
% a local peak is a "tax" on efficiency.
domain_priors:base_extractiveness(local_vs_global_optima, 0.3).

% Suppression score (0.5)
% Local peaks often suppress the visibility of the global peak; 
% if you are on a "high enough" hill, you may not even look for the mountain.
domain_priors:suppression_score(local_vs_global_optima, 0.5).

domain_priors:emerges_naturally(local_vs_global_optima).

% Metrics
narrative_ontology:constraint_metric(local_vs_global_optima, extractiveness, 0.3).
narrative_ontology:constraint_metric(local_vs_global_optima, suppression_requirement, 0.5).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE HILL-CLIMBER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (limited by local perception)
   WHEN: immediate
   WHERE: trapped (at the local peak)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For an agent that can only see immediate improvements (like a simple 
   algorithm or a short-sighted manager), the local optimum is a "Noose." 
   They have reached a point where any single step in any direction makes 
   things worse, so they stop. They are "strangled" by their own success.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    local_vs_global_optima,
    noose,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ANNEALER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile (willing to accept short-term loss)
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For an agent that understands the landscape (using heuristics like 
   Simulated Annealing), the local optimum is just a "Rope" to be tested. 
   They are willing to climb down into the valley to find a higher peak, 
   using the local data to coordinate their next jump.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    local_vs_global_optima,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(regional))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MATHEMATICIAN - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the mathematician, the existence of local vs global optima is a 
   "Mountain"—an immutable law of non-convex optimization. It is 
   simply how the universe is shaped.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    local_vs_global_optima,
    mountain,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
) :- !.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
