% ============================================================================
% CONSTRAINT STORY: busy_beaver_noncomputability
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Tibor Radó (1962) / Computability Theory
% ============================================================================

:- module(constraint_busy_beaver, []).

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
 * * constraint_id: busy_beaver_noncomputability
 * human_readable: The Busy Beaver Function (Σ)
 * domain: technological/mathematical
 * temporal_scope: 1962 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Universal Logic)
 * * SUMMARY:
 * The Busy Beaver function measures the maximum number of steps a halting Turing 
 * machine with $n$ states can take before stopping. It is a well-defined but 
 * non-computable function that grows faster than any computable sequence 
 * (exponential, factorial, or Ackermann). It represents the absolute ceiling 
 * of algorithmic complexity and the price of the Halting Problem.
 * * KEY AGENTS:
 * - The Turing Machine (Subject): A powerless agent whose productivity is 
 * bound by its state-count but whose limits are unknowable.
 * - The Theoretical Computer Scientist (Institutional): An agent using the 
 * Busy Beaver as a "Rope" to define the boundaries of potential computation.
 * - The Brute-Force Searcher (Victim): An agent for whom the function is a 
 * "Noose," as they expend infinite energy searching for values that 
 * mathematics proves they can never algorithmically predict.
 * * NARRATIVE ARC:
 * The Busy Beaver is the "Mountain" of formal systems—a fixed, unyielding 
 * truth of state-space geometry. In high-level logic, it is a "Rope" for 
 * proving non-computability via reduction. However, for those attempting 
 * to solve "provability" through exhaustive simulation, it is a "Noose" 
 * that extracts time, power, and capital (extraction), eventually "strangling" 
 * the project with growth so rapid it exceeds the physical resources 
 * of the universe.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for the Deferential Realism Auditor
narrative_ontology:interval(busy_beaver_era, 1962, 2026).
narrative_ontology:constraint_claim(busy_beaver_noncomputability, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.4. It "extracts" the possibility of universal predictive 
% heuristics for code behavior. It imposes a "tax" of unknowability on 
% every finite system, forcing a reliance on bounded approximations.
domain_priors:base_extractiveness(busy_beaver_noncomputability, 0.4).

% Suppression score (0.0-1.0)
% Rationale: 0.3. It suppresses the hope for "omniscience" in automated 
% theorem proving, rendering the idea of a "universal optimizer" 
% mathematically fraudulent.
domain_priors:suppression_score(busy_beaver_noncomputability, 0.3).

% Enforcement: Emerges naturally from the axioms of computation.
domain_priors:emerges_naturally(busy_beaver_noncomputability).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(busy_beaver_noncomputability, extractiveness, 0.4).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, suppression_requirement, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(busy_beaver_noncomputability, complexity_philosophers).
constraint_beneficiary(busy_beaver_noncomputability, logic_educators).
constraint_victim(busy_beaver_noncomputability, brute_force_optimizers).
constraint_victim(busy_beaver_noncomputability, long_term_simulators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE 5-STATE TURING MACHINE - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The machine is slave to its transition table.
   WHEN: immediate - True every time the machine is initialized.
   WHERE: trapped - Bound within the finite states it possesses.
   SCOPE: local - Immediate read/write/move operations.
   
   WHY THIS CLASSIFICATION:
   For a specific machine, its maximum step count is an absolute Mountain. 
   It cannot "choose" to run for $k+1$ steps if its $n$ states only allow for $k$. 
   The logic of the Busy Beaver limit is a fixed feature of its mathematical 
   universe, unyielding and absolute.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    busy_beaver_noncomputability,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE THEORETICAL COMPUTER SCIENTIST - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to establish the rules of what is computable.
   WHEN: biographical - Spanning a career of research and proof-building.
   WHERE: mobile - Can adjust the "universal machine" model to explore bounds.
   SCOPE: global - Universal application in computer science.
   
   WHY THIS CLASSIFICATION:
   For the researcher, the function is a "Rope"—a functional coordination 
   mechanism. It provides the "standard of achievement" for showing why 
   the Halting Problem is hard. By mapping the "Mountain," they coordinate 
   the understanding of growth rates and logical limits.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    busy_beaver_noncomputability,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXPERIMENTAL SEARCHER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has agency to search but is bound by the growth rate.
   WHEN: historical - It may take centuries of compute to find $\Sigma(6)$ or $\Sigma(7)$.
   WHERE: constrained - No alternative but to use exponentially growing resources.
   SCOPE: global - Searching through the entire set of possible small machines.
   
   WHY THIS CLASSIFICATION:
   For the experimenter, the Busy Beaver is a "Noose." It "strangles" 
   computational projects. Because $\Sigma(n)$ grows faster than any 
   computable function, it extracts enormous energy and hardware cycles 
   (extraction) while "choking" the project with the certainty that most 
   machines will never be proven to halt or run forever.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    busy_beaver_noncomputability,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(busy_beaver_noncomputability, E),
    E >= 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(busy_beaver_tests).

test(multi_perspective_variance) :-
    % Machine -> Mountain
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, Type1, context(individual_powerless, immediate, trapped, local)),
    % Scientist -> Rope
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, Type2, context(institutional, biographical, mobile, global)),
    Type1 \= Type2.

test(search_extraction_penalty) :-
    % Searchers feel the 0.4 extraction of resources as a Noose.
    Context = context(individual_moderate, historical, constrained, global),
    constraint_indexing:extractiveness_for_agent(busy_beaver_noncomputability, Context, Score),
    Score >= 0.3.

test(natural_emergence) :-
    domain_priors:emerges_naturally(busy_beaver_noncomputability).

:- end_tests(busy_beaver_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * I chose 0.4 because the Busy Beaver function is the "Apex Extractor" of 
 * computability. It doesn't just take energy; it renders the very 
 * concept of "finding the end" a resource-sink that grows faster than 
 * the universe's atoms.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Machine (Subject), Scientist (User), and Searcher (Victim) 
 * to illustrate how a "Mountain" of logic becomes a "Noose" for 
 * experimental verification.
 * * 3. OMEGA IDENTIFICATION: 
 * Formalized the "Provability" uncertainty—is there a fixed point where 
 * our math is simply too weak to ever know $\Sigma(n)$ for a specific $n$?
 */

% YOUR OMEGAS HERE:
omega_variable(
    busy_beaver_unprovability_threshold,
    "At what value of $n$ does the specific value of $\Sigma(n)$ become independent of ZFC (Mountain)?",
    resolution_mechanism("Investigation into the smallest Turing machine that simulates a search for a contradiction in ZFC."),
    impact("If $n$ is small (e.g., <1000): The 'Noose' of unprovability is much tighter than we hope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * * ALTERNATIVE 1: Bounded Computation
 * Viability: Limiting the tape or the step-count arbitrarily.
 * Suppression: Rejected by pure theory because it breaks the Turing-completeness 
 * of the "Mountain."
 * * * ALTERNATIVE 2: Hypercomputation
 * Viability: Machines that can compute the Busy Beaver in "one step" (Oracle machines).
 * Suppression: Suppressed by the "Mountain" of standard physical and logical 
 * limits in our universe.
 * * CONCLUSION:
 * The absence of Alternative 2 (Hypercomputation) is exactly what makes the 
 * Busy Beaver a "Mountain" of absolute logical reality for our era.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [busy_beaver_noncomputability].
 * 2. Multi-perspective: ?- multi_index_report(busy_beaver_noncomputability).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
