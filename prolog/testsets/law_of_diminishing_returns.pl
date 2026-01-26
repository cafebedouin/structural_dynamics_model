% ============================================================================
% CONSTRAINT STORY: law_of_diminishing_returns
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Classical Economics / David Ricardo / T.R. Malthus
% ============================================================================

:- module(constraint_law_of_diminishing_returns, []).

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
 * * constraint_id: law_of_diminishing_returns
 * human_readable: The Law of Diminishing Returns
 * domain: economic/technological
 * temporal_scope: Permanent (Physical and Economic Reality)
 * spatial_scope: Global (Production Systems)
 * * SUMMARY:
 * The law of diminishing returns states that in all productive processes, 
 * adding more of one factor of production while holding others constant 
 * will at some point yield lower per-unit returns. It represents the 
 * fundamental limit of scaling within a fixed infrastructure.
 * * KEY AGENTS:
 * - The Economist: Analytical observer mapping the marginal utility curves.
 * - The Factory Manager: Institutional agent seeking the "sweet spot" of 
 * efficiency to maximize profit.
 * - The Overworked Laborer: Individual powerless agent added to an already 
 * crowded process, experiencing the "squeeze" of inefficiency.
 * * NARRATIVE ARC:
 * Initially, adding resources feels like a "Rope" for growth. However, 
 * as the system saturates, the law emerges as a "Mountain"—a hard 
 * physical limit of the environment. For the last unit added, it becomes 
 * a "Snare," as their presence extracts value from the group while 
 * providing negligible or negative individual utility.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(diminishing_returns_interval, 0, 10).
narrative_ontology:constraint_claim(law_of_diminishing_returns, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). The law extracts "marginal utility." As 
% production scales, the system "taxes" each additional unit of input 
% with lower output, extracting potential efficiency.
domain_priors:base_extractiveness(law_of_diminishing_returns, 0.4).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.3). It suppresses the "Infinite Scaling" 
% narrative. While growth is encouraged, the physical reality eventually 
% punishes those who ignore the curve.
domain_priors:suppression_score(law_of_diminishing_returns, 0.3).

% Enforcement: Emerges naturally from physical space and resource scarcity.
domain_priors:emerges_naturally(law_of_diminishing_returns).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(law_of_diminishing_returns, extractiveness, 0.4).
narrative_ontology:constraint_metric(law_of_diminishing_returns, suppression_requirement, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(law_of_diminishing_returns, [efficiency_consultants, equilibrium_stable_states]).
constraint_victim(law_of_diminishing_returns, [over_extending_firms, marginal_laborers]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ECONOMIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of systemic limits and curves.
   WHEN: civilizational - Viewing production as a permanent feature of humanity.
   WHERE: trapped - Logic and thermodynamics cannot bypass the law.
   SCOPE: global - Universal to all resource-constrained systems.
   
   WHY THIS CLASSIFICATION:
   To the analyst, the law is a Mountain. It is an unchangeable feature of 
   the universe's "hardware." You cannot "innovate" past the point where 
   a fixed field produces less per extra bag of seed; it is a fixed peak 
   in the topography of reality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    law_of_diminishing_returns,
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
   PERSPECTIVE 2: THE BUSINESS OWNER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to allocate capital and labor.
   WHEN: biographical - Managing a firm's profitability over a career.
   WHERE: arbitrage - Can shift resources between different production lines.
   SCOPE: national - Industrial/Corporate level.
   
   WHY THIS CLASSIFICATION:
   For the owner, the law is a Rope. It is a coordination mechanism. By 
   identifying the point of diminishing returns, they can "tether" their 
   investments to the optimal point, pulling the business toward 
   maximum profit and preventing waste.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    law_of_diminishing_returns,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(law_of_diminishing_returns, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ADDITIONAL WORKER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the crowded workspace.
   WHEN: immediate - Today's shift and productivity pressure.
   WHERE: constrained - Limited by available tools and space.
   SCOPE: local - Immediate workstation.
   
   WHY THIS CLASSIFICATION:
   For the worker added to a saturated team, the law is a Snare. Their 
   presence makes the work harder for everyone (crowding), yet they are 
   judged by the falling marginal output. The system "strangles" their 
   effectiveness, extracting their labor while providing a trap of 
   decreasing returns and potentially lower wages.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    law_of_diminishing_returns,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(law_of_diminishing_returns, E),
    E > 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(law_of_diminishing_returns_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(law_of_diminishing_returns, mountain, context(analytical, civilizational, trapped, global)),
    % Institutional sees Rope
    constraint_indexing:constraint_classification(law_of_diminishing_returns, rope, context(institutional, biographical, arbitrage, national)),
    % Powerless sees Snare
    constraint_indexing:constraint_classification(law_of_diminishing_returns, snare, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_marginal_loss) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(law_of_diminishing_returns, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(law_of_diminishing_returns, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(law_of_diminishing_returns_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.4):
 * Reasoning: The law extracts potential "growth" and "utility." It forces 
 * a systemic cost where each new input provides less benefit.
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Economist (Law), the Manager (Tool), and the Worker (Victim) 
 * to demonstrate how a physical fact becomes an organizational trap.
 * * 3. NOOSE LOGIC:
 * Specifically focuses on the "crowding effect" in production, where the 
 * additional agent feels the pressure of the limit more than the 
 * institutional coordinator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tech_shift_plateau,
    "Can a technological breakthrough permanently 'untie' the Snare (Rope), 
    or does it just move the Mountain further back (Mountain)?",
    resolution_mechanism("Long-term analysis of agricultural/industrial 
    output after major automation shifts"),
    impact("If Mountain: The law is absolute. If Rope: Humanity can 
    escape scarcity indefinitely."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Economies of Scale
 * Viability: High. In some phases, increasing all inputs can lead to 
 * *increased* per-unit returns (the opposite of this law).
 * Suppression: None. It is viewed as a complementary phase that 
 * eventually leads back to the diminishing returns mountain.
 * * CONCLUSION:
 * The existence of Economies of Scale as a "Rope" for growth suggests 
 * that the Law of Diminishing Returns is not a Snare until a system 
 * reaches its physical or organizational saturation point.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_law_of_diminishing_returns].
 * 2. Multi-perspective: ?- multi_index_report(law_of_diminishing_returns).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
