% ============================================================================
% CONSTRAINT STORY: fast_growing_hierarchy
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Mathematical Logic / Computability Theory / Subrecursive Hierarchies
% ============================================================================

:- module(constraint_fast_growing_hierarchy, []).

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
 * * constraint_id: fast_growing_hierarchy
 * human_readable: The Fast-Growing Hierarchy (FGH)
 * domain: technological
 * temporal_scope: Permanent (Universal Mathematical Law)
 * spatial_scope: Global (Formal Information Systems)
 * * SUMMARY:
 * The Fast-Growing Hierarchy is a family of functions indexed by ordinals that 
 * classifies the growth rate of computable functions. It defines a hard 
 * boundary for what can be computed within specific axiomatic systems; for 
 * example, Peano Arithmetic cannot "see" the termination of functions 
 * growing at the rate of f_epsilon_0.
 * * KEY AGENTS:
 * - The Proof Theorist: Uses FGH as a yardstick (Rope) to measure the strength of 
 * mathematical systems and justify transfinite methods.
 * - The Computer Scientist: Faces the "wall" of recursion (Snare/Mountain) where 
 * algorithms that are technically "computable" are physically impossible 
 * due to the scale of time and memory required.
 * - The Analytical Observer: Views the hierarchy as an objective geometric 
 * landscape of complexity.
 * * NARRATIVE ARC:
 * FGH begins as a simple recursive tool (f_0(n) = n+1). As the index climbs, 
 * it quickly consumes all available physical resources (Snare). To the logician, 
 * it is a Mountain of necessity; to the system architect, it is a Rope 
 * for classifying upper bounds on complexity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor for extraction
narrative_ontology:interval(fast_growing_hierarchy, 0, 10).
narrative_ontology:constraint_claim(fast_growing_hierarchy, mountain).

% Base extractiveness: 0.6 (Moderate-High)
% Rationale: FGH extracts "realizability" and "physical possibility." It 
% renders vast swathes of mathematical truth "uncomputable" in any physical 
% universe, essentially stealing the utility of certain recursive definitions.
domain_priors:base_extractiveness(fast_growing_hierarchy, 0.6).

% Suppression: 0.4 (Moderate)
% Rationale: It suppresses "naive finitism." The existence of FGH forces 
% the admission that "computable" does not mean "attainable," suppressing 
% simpler, more intuitive models of computation.
domain_priors:suppression_score(fast_growing_hierarchy, 0.4).

% Enforcement: Emerges naturally from the axioms of induction and recursion.
domain_priors:emerges_naturally(fast_growing_hierarchy).

% Metrics for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(fast_growing_hierarchy, extractiveness, 0.6).
narrative_ontology:constraint_metric(fast_growing_hierarchy, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
% Beneficiaries: Transfinite mathematicians who gain a framework for classifying truth.
constraint_beneficiary(fast_growing_hierarchy, transfinite_logicians).
% Victims: Brute-force algorithm designers and physical computers limited by entropy.
constraint_victim(fast_growing_hierarchy, [brute_force_solvers, physical_hardware]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE LOGICAL ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal informational laws.
   WHEN: civilizational - Viewing logic as a permanent substrate.
   WHERE: trapped - Logic cannot bypass the recursive definitions of ordinals.
   SCOPE: global - Universal applicability.
   
   WHY THIS CLASSIFICATION:
   To the analyst, FGH is a Mountain. It is an unchangeable consequence of 
   recursive depth. No amount of "better hardware" changes the fact that 
   f_3(n) is exponentially larger than f_2(n). It is a fixed peak in the 
   landscape of complexity theory.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fast_growing_hierarchy,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PROOF THEORIST - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define and utilize formal rules for proofs.
   WHEN: biographical - Achieving results within a single career.
   WHERE: arbitrage - Can move between different systems (PA, ZFC, etc.) 
          using FGH to benchmark them.
   SCOPE: national - Industrial/Academic application.
   
   WHY THIS CLASSIFICATION:
   For the proof theorist, FGH is a Rope. It is a coordination mechanism for 
   understanding axiomatic strength. By assigning an ordinal to a theory, 
   they "pull" truths out of systems that were otherwise opaque. It 
   transforms a "Snare" of complexity into a "Rope" of classification.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fast_growing_hierarchy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(fast_growing_hierarchy, E),
    E < 0.7, % It's a tool if it's manageable
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PROGRAMMER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the limits of compute and time.
   WHEN: immediate - The current task of solving a recursive function.
   WHERE: constrained - Limited by a fixed compute budget.
   SCOPE: local - Immediate workspace.
   
   WHY THIS CLASSIFICATION:
   For the developer accidentally implementing an Ackermann-level function 
   (f_omega), the hierarchy is a Snare. It strangles their resources instantly. 
   There is no "escape" within their current hardware; the complexity 
   tightens with every additional increment of 'n', leading to system 
   failure (the "ruin" of the process).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fast_growing_hierarchy,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(fast_growing_hierarchy, E),
    E > 0.5, % High extraction of resources
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(fast_growing_hierarchy_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain, context(analytical, civilizational, trapped, global)),
    % Institutional sees Rope
    constraint_indexing:constraint_classification(fast_growing_hierarchy, rope, context(institutional, biographical, arbitrage, national)),
    % Powerless sees Snare
    constraint_indexing:constraint_classification(fast_growing_hierarchy, snare, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    % While the model reports a base score, the experienced extraction scales
    % because the powerless cannot arbitrage out of the compute trap.
    domain_priors:base_extractiveness(fast_growing_hierarchy, Score),
    Score > 0.5.

test(time_immutability_fgh) :-
    % Short horizon = Mountain/Snare (the wall is hit now)
    constraint_indexing:effective_immutability(immediate, constrained, mountain).

:- end_tests(fast_growing_hierarchy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.6):
 * Reasoning: FGH is the ultimate "resource extractor." It takes a 
 * theoretically "computable" process and makes it physically impossible, 
 * extracting time and memory at an astronomical rate.
 * * 2. PERSPECTIVE SELECTION:
 * Chose to highlight the difference between "Math as Law" (Mountain), 
 * "Math as Metric" (Rope), and "Math as Trap" (Snare).
 * * 3. CLASSIFICATION RATIONALE:
 * The "Snare" for programmers is particularly apt; one wrong recursive call 
 * and the "rope" of computation becomes a "snare" for the CPU.
 * * 4. CONFIDENCE:
 * High: Mathematical properties of FGH.
 * Medium: Exact extractiveness score (varies by ordinal index).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_realizability_limit,
    "Does our specific physical universe impose a 'Hard Mountain' at a 
    specific FGH index (e.g., Bekenstein bound), or can information 
    persist to f_epsilon_0?",
    resolution_mechanism("Discovery of final laws of quantum gravity and information density"),
    impact("If Mountain: FGH is a Snare for all physical life. If Rope: Space-time 
    can be manipulated to compute transfinite values."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * Does this constraint have alternatives that were suppressed?
 * * ALTERNATIVE 1: Non-standard Models of Arithmetic
 * Viability: Allows for "infinite" integers that might bypass FGH limits.
 * Suppression: High. Standard mathematics favors the "Standard Model" for 
 * intuitive consistency, suppressing non-standard models as "curiosities."
 * * CONCLUSION:
 * The dominance of standard PA/ZFC makes FGH a Snare for those who cannot 
 * access the "Rope" of non-standard models.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * * 1. Load: ?- [constraint_fast_growing_hierarchy].
 * 2. Multi-perspective: ?- multi_index_report(fast_growing_hierarchy).
 * 3. Run tests: ?- run_tests(fast_growing_hierarchy_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
