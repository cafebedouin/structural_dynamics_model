% ============================================================================
% CONSTRAINT STORY: local_vs_global_optima
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Optimization Theory / Genetic Algorithms / Calculus
% ============================================================================

:- module(constraint_local_vs_global_optima, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: local_vs_global_optima
 * human_readable: Local vs. Global Optima (The Peak Trap)
 * domain: mathematical/technological/biological
 * temporal_scope: Immediate to Civilizational
 * spatial_scope: Global
 * 
 * SUMMARY:
 * A "Local Optimum" is a solution that is better than all nearby solutions 
 * but inferior to the "Global Optimum"—the best possible solution across 
 * the entire system.
 * 
 * KEY AGENTS:
 * - The Hill-Climber: An agent that always moves toward immediate improvement.
 * - The Explorer: An agent that occasionally moves "downhill" to find 
 * a different, potentially higher mountain.
 * - The Landscape: The mathematical reality defining the peaks and valleys.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(local_vs_global_optima, 0, 10).
narrative_ontology:constraint_claim(local_vs_global_optima, tangled_rope).
domain_priors:requires_active_enforcement(local_vs_global_optima).

% Base extractiveness score (0.3)
% Sub-optimality extracts potential value from the system; being stuck at 
% a local peak is a "tax" on efficiency.
domain_priors:base_extractiveness(local_vs_global_optima, 0.3).

% Suppression score (0.5)
% Local peaks often suppress the visibility of the global peak; 
% if you are on a "high enough" hill, you may not even look for the mountain.
domain_priors:suppression_score(local_vs_global_optima, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(local_vs_global_optima, extractiveness, 0.3).
narrative_ontology:constraint_metric(local_vs_global_optima, suppression_requirement, 0.5).

domain_priors:emerges_naturally(local_vs_global_optima).

% Asymmetry Metrics
narrative_ontology:constraint_beneficiary(local_vs_global_optima, none).
narrative_ontology:constraint_victim(local_vs_global_optima, none).


/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE HILL-CLIMBER - Snare
   --------------------------------------------------------------------------
   WHO: powerless (limited by local perception)
   WHEN: immediate
   WHERE: trapped (at the local peak)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For an agent that can only see immediate improvements (like a simple 
   algorithm or a short-sighted manager), the local optimum is a "Snare." 
   They have reached a point where any single step in any direction makes 
   things worse, so they stop. They are "strangled" by their own success.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    local_vs_global_optima,
    snare,
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
).

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
).

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
    tangled_rope,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
).


/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE OPTIMIZATION ENGINEER - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (designs systems around the problem)
   WHEN: biographical
   WHERE: mobile (can choose algorithms/strategies)
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For an engineer building or applying optimization algorithms, the presence
   of local and global optima is a fundamental, immutable characteristic of
   the problem space. It's a "Mountain" they acknowledge and design their
   systems to navigate, rather than something they can eliminate. They accept
   it as a given and work within its constraints.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    local_vs_global_optima,
    tangled_rope,
    context(agent_power(institutional), time_horizon(biographical), exit_options(mobile), spatial_scope(regional))
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(local_vs_global_optima_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that same constraint = different types from different perspectives
 */
test(multi_perspective_variance) :-
    % Perspective 1
    constraint_indexing:constraint_classification(
        local_vs_global_optima,
        Type1,
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
    ),
    % Perspective 2
    constraint_indexing:constraint_classification(
        local_vs_global_optima,
        Type2,
        context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(regional))
    ),
    % Perspective 3
    constraint_indexing:constraint_classification(
        local_vs_global_optima,
        Type3,
        context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
    ),
    % Verify they differ (Snare, Rope, Mountain)
    Type1 \= Type2,
    Type2 \= Type3.

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that extraction experienced varies with agent power
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(biographical), exit_options(mobile), spatial_scope(regional)),
    constraint_indexing:extractiveness_for_agent(local_vs_global_optima, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(local_vs_global_optima, ContextPowerful, Score2),
    Score1 > Score2.  % Powerless experience more extraction

/**
 * TEST 3: Local Peak Trap Insight
 * Tests the core insight that a hill-climber gets trapped.
 */
test(local_peak_trap_insight) :-
    % A hill-climber is defined by being powerless and trapped.
    constraint_indexing:constraint_classification(
        local_vs_global_optima,
        snare,
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
    ).

:- end_tests(local_vs_global_optima_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * 
 * KEY DECISIONS MADE BY MODEL:
 * 
 * 1. BASE EXTRACTIVENESS (0.3):
 *    Reasoning: The nature of local vs. global optima inherently "extracts" potential value by trapping agents in sub-optimal solutions.
 *    Evidence: Optimization theory, real-world scenarios where local maxima impede global progress.
 *    Uncertainty: The precise quantification of "extracted potential" is subjective and context-dependent.
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Model chose to analyze from 4 perspectives (Hill-Climber, Annealer, Mathematician, Optimization Engineer) because they represent distinct interactions with the optimization landscape.
 *    These four cover key operational, strategic, and analytical viewpoints.
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    Hill-Climber as Snare: Trapped by limited perception and immediate gains, unable to escape the local peak.
 *    Annealer as Rope: Possesses strategy to escape local peaks by accepting temporary setbacks for greater gains.
 *    Mathematician as Mountain: Views the concept as an immutable mathematical truth.
 *    Optimization Engineer as Mountain: Designs systems with the inherent understanding of this immutable constraint.
 * 
 * 4. AMBIGUITIES IN SOURCE MATERIAL:
 *    - Quantification of "optimality": How "optimal" a local solution is relative to global can vary greatly, influencing the perceived "trap" or "rope." Resolved by focusing on the qualitative experience of the agent.
 *
 * 5. CONFIDENCE ASSESSMENT:
 *    High confidence: Classification of Mathematician (Mountain) and Engineer (Mountain), as these roles deal with the inherent mathematical/systemic nature.
 *    Medium confidence: Classification of Hill-Climber (Snare) and Annealer (Rope), as these depend on agent's perception and strategy, which can vary.
 *    Low confidence: Precise quantitative scoring of extractiveness and suppression, as these are conceptual for this abstract constraint.
 *    Would benefit from: Specific real-world case studies demonstrating these behaviors in different agent types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * omega_variable(local_vs_global_optima_trapped_perception,
 *      "Does the agent truly perceive itself as 'trapped' or merely content with 'good enough'?",
 *      resolution_mechanism("In-depth psychological/behavioral study of agents operating within sub-optimal solutions."),
 *      impact("If merely content: shifts Snare classification to a weaker Rope or even Mountain, as the constraint is self-imposed or accepted. If truly trapped: reinforces Snare."),
 *      confidence_without_resolution(medium)
 * ).
 */

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * For the fundamental mathematical concept of local vs. global optima, there are no "suppressed alternatives"
 * in the sense of an alternative mathematical reality. However, there are alternative *approaches* to
 * navigating such landscapes.
 * 
 * ALTERNATIVE 1: Meta-heuristic Algorithms (e.g., Genetic Algorithms)
 *    Viability: These algorithms are designed to escape local optima by exploring broader solution spaces. They are viable and widely used.
 *    Suppression: Not suppressed; rather, they represent an *alternative strategy* to overcome the constraint, demonstrating that the "trap" is not absolute for all agents.
 *    Evidence: The field of global optimization.
 * 
 * ALTERNATIVE 2: Problem Transformation
 *    Viability: In some cases, a problem can be re-formulated (e.g., convexified) such that local optima are eliminated.
 *    Suppression: Not suppressed; this is a valid engineering/mathematical technique.
 *    Evidence: Convex optimization literature.
 * 
 * CONCLUSION:
 * The existence of alternative *strategies* (like meta-heuristics or problem transformation) means that for agents with the knowledge and tools, the local optima are not an inescapable "Snare," but a navigable "Rope" or even a "Mountain" to design around. This reinforces the perspectival nature of the constraint.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/local_vs_global_optima].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(local_vs_global_optima).
 * 
 * 3. Run tests:
 *    ?- run_tests(local_vs_global_optima_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(local_vs_global_optima).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(local_vs_global_optima, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Formal truth — substantive with near-zero performative component
domain_priors:theater_ratio(local_vs_global_optima, 0.0).
narrative_ontology:constraint_metric(local_vs_global_optima, theater_ratio, 0.0).
