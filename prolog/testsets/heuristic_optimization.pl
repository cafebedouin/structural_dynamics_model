% ============================================================================
% CONSTRAINT STORY: heuristic_optimization
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Metaheuristics / Evolutionary Computing / Optimization Theory
% ============================================================================

:- module(constraint_heuristics, []).

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
 * * constraint_id: heuristic_optimization
 * human_readable: Heuristic Optimization (The "Good Enough" Rope)
 * domain: technological/mathematical/economic
 * temporal_scope: Immediate to Biographical
 * spatial_scope: Global
 * * SUMMARY:
 * Heuristics are strategies derived from experience or "rules of thumb" 
 * to solve problems more quickly than classic methods. While they do not 
 * guarantee a perfect solution, they provide a functional one where 
 * exhaustive search (Mountain) would fail due to time or resource limits.
 * * KEY AGENTS:
 * - The Solver: The agent utilizing the heuristic to navigate a complex system.
 * - The Perfectionist: The agent trapped by the Noose of absolute optimality.
 * - The System: The reality that rewards speed and "functionality" over 
 * theoretical purity.
 * * NARRATIVE ARC:
 * Heuristics function as "Approximate Ropes." They acknowledge the 
 * impossibility of climbing the absolute peak of the Mountain and instead 
 * find a stable plateau. This creates a matching market where the "Price" 
 * is a small margin of error, paid for in exchange for a massive 
 * gain in speed and agency.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(heuristic_optimization, 0, 10).
narrative_ontology:constraint_claim(heuristic_optimization, rope).

% Base extractiveness score (0.2)
% Low extraction; users "pay" in sub-optimality, but gain the ability 
% to function in complex environments.
domain_priors:base_extractiveness(heuristic_optimization, 0.2).

% Suppression score (0.1)
% Heuristics are inherently transparent "shortcuts"; they don't hide 
% alternatives so much as they simplify the search for them.
domain_priors:suppression_score(heuristic_optimization, 0.1).

% Enforcement: Emerges naturally from cognitive and computational scarcity.
domain_priors:emerges_naturally(heuristic_optimization).

% Metrics
narrative_ontology:constraint_metric(heuristic_optimization, extractiveness, 0.2).
narrative_ontology:constraint_metric(heuristic_optimization, suppression_requirement, 0.1).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(heuristic_optimization, real_time_systems).
constraint_beneficiary(heuristic_optimization, adaptive_agents).
constraint_victim(heuristic_optimization, none). % Positive-sum utility

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRACTITIONER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: immediate
   WHERE: mobile
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For someone making real-world decisions (like a delivery driver or a 
   trading algorithm), a heuristic is a "Rope." It allows them to act 
   instantly. The trade-off (95% optimal in 1 second vs 100% optimal 
   in 100 years) is a coordination win that empowers their mobility.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    heuristic_optimization,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(heuristic_optimization, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE THEORIST - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerful (intellectual authority)
   WHEN: generational
   WHERE: trapped (locked in the search for the 'Absolute Truth')
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To a perfectionist seeking the absolute global minimum, a heuristic 
   is a "Noose." It "strangles" the truth by settling for a local 
   optimum. They see the heuristic as a shortcut that prevents the 
   system from ever reaching its theoretical potential.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    heuristic_optimization,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BIOLOGICAL ORGANISM - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   Biological evolution is a "Mountain" of heuristic optimization. 
   Natural selection doesn't find the "perfect" organism; it finds 
   the one that is "good enough" to survive and reproduce. To the 
   observer, heuristics are the immutable law of life in a resource-constrained universe.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    heuristic_optimization,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(heuristic_optimization),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(heuristic_tests).

test(functional_mobility) :-
    % Testing that heuristics act as a Rope for those needing immediate action.
    constraint_indexing:constraint_classification(heuristic_optimization, rope, context(_, _, mobile, _)).

test(optimal_frustration) :-
    % Testing that those 'trapped' by the need for perfection see a Noose.
    constraint_indexing:constraint_classification(heuristic_optimization, noose, context(_, _, trapped, _)).

:- end_tests(heuristic_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Heuristics are the "Exit Option" for NP-hard problems. I set 
 * extractiveness to 0.2 because the cost is inaccuracy, not coercion. 
 * I chose "Mountain" for biology because life *is* a heuristic process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    heuristic_drift,
    "Will heuristics eventually become so accurate that the 'theoretical optimal' becomes irrelevant to human civilization?",
    resolution_mechanism("Comparative analysis of economic gains from heuristics vs. potential gains from theoretical optima"),
    impact("If Yes: The Noose of NP-completeness is effectively cut; the Mountain is conquered."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
