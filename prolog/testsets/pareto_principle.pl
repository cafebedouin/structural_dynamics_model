% ============================================================================
% CONSTRAINT STORY: pareto_principle
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Statistical Observation / Vilfredo Pareto (1896)
% ============================================================================

:- module(constraint_pareto_principle, []).

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
 * 
 * constraint_id: pareto_principle
 * human_readable: The Pareto Principle (80/20 Rule)
 * domain: economic/social/statistical
 * temporal_scope: Permanent (Universal Law of Power Distributions)
 * spatial_scope: Global (Systems with Cumulative Advantage)
 * 
 * SUMMARY:
 * The Pareto Principle states that for many outcomes, roughly 80% of consequences 
 * come from 20% of causes. This power-law distribution manifests in wealth, 
 * software bugs, city sizes, and biological populations. It represents a 
 * fundamental asymmetry in how value or effort is distributed within a system.
 * 
 * KEY AGENTS:
 * - The Bottom-Tier Competitor (Individual Powerless): Struggles for survival in a winner-take-all market.
 * - The Business Manager (Individual Moderate): Seeks to identify the high-leverage 20% to maximize ROI.
 * - The Government Policy Maker (Institutional): Designs policies that interact with wealth distribution.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(pareto_principle, 0, 10).
narrative_ontology:constraint_claim(pareto_principle, mountain).

% Base extractiveness: 0.4 (Moderate)
% It describes an inherent asymmetry where a small group of inputs 
% "extracts" or captures the majority of the outputs.
domain_priors:base_extractiveness(pareto_principle, 0.4).

% Suppression score: 0.3 (Low)
% Alternatives (Normal distributions) are visible and frequent in 
% nature, but in complex networks, the Pareto distribution 
% is rarely successfully "suppressed" for long.
domain_priors:suppression_score(pareto_principle, 0.3).

% Enforcement: Emerges naturally from preferential attachment and power laws.
domain_priors:emerges_naturally(pareto_principle).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(pareto_principle, vital_few).
constraint_victim(pareto_principle, trivial_many).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE BOTTOM-TIER COMPETITOR - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (A small creator or worker in the tail end).
   WHEN: immediate (Fighting for survival in a winner-take-all market).
   WHERE: constrained (Locked into a system where rewards are centralized).
   
   WHY THIS CLASSIFICATION:
   For someone in the "long tail," the principle is a 'Snare'. The more the system 
   grows, the more value concentrates at the top (The 20%). This makes it 
   increasingly difficult for those at the bottom to gain traction, as the 
   distribution naturally "strangles" those without initial leverage.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    pareto_principle,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BUSINESS MANAGER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (A professional optimizing a system).
   WHEN: biographical (Achieving goals within a career).
   WHERE: mobile (Can shift focus to different inputs).
   
   WHY THIS CLASSIFICATION:
   For the manager, the principle is a 'Rope'. It is a coordination mechanism for 
   productivity. By focusing on the "vital few" customers or tasks, they can 
   achieve disproportionate results with minimal effort. It is a tool for 
   leveraging reality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    pareto_principle,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GOVERNMENT POLICY MAKER - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (Designs policies affecting wealth distribution).
   WHEN: generational (Planning for long-term societal well-being).
   WHERE: analytical (Observing economic trends as immutable facts).
   
   WHY THIS CLASSIFICATION:
   For a government policy maker, the Pareto principle is a 'Mountain' – an
   immutable demographic reality they must plan around for long-term policy
   (e.g., taxation, social welfare). It is an unchangeable input to their models.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    pareto_principle,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(analytical),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(pareto_principle_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(pareto_principle, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(pareto_principle_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'Government Policy Maker' as the
 *    institutional agent. For such a body, the Pareto principle is a 'Mountain'
 *    of statistical reality that must be acknowledged when designing policies.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Bottom-Tier (Snare): Trapped by wealth concentration.
 *    - Business Manager (Rope): A tool for efficient resource allocation.
 *    - Policy Maker (Mountain): An immutable statistical reality.
 * 
 * 3. CORE INSIGHT: The Pareto Principle is a 'Mountain' of statistical reality
 *    that has profound implications for how resources are distributed. What is a
 *    tool ('Rope') for some can be a trap ('Snare') for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether the Pareto distribution is an immutable law
 * or a manipulable consequence of system design.
 */

omega_variable(
    preferential_attachment_origin,
    "Is the 80/20 distribution a result of inherent initial skill/quality (Mountain) or pure stochastic luck exacerbated by preferential attachment (Snare)?",
    resolution_mechanism("Simulating high-fidelity agents with varying initial conditions vs. luck ratios in a preferential attachment model."),
    impact("If Skill: The distribution is a 'Rope' for meritocracy. If Luck: It is a 'Snare' of unfair advantage."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Normal Distribution (Gaussian)
 *    Viability: Occurs naturally in physical traits (e.g., height).
 *    Suppression: Rarely observed in information or wealth systems with cumulative advantage. It is not actively "suppressed" so much as it simply doesn't emerge.
 *
 * ALTERNATIVE 2: Radical Egalitarian Redistribution
 *    Viability: Mathematically suppresses the Pareto tail (e.g., progressive taxation, universal basic income).
 *    Suppression: High. Often viewed as economically inefficient or coercive by those who benefit from the Pareto distribution.
 *
 * CONCLUSION:
 * While the Pareto Principle is a natural 'Mountain' for observers, the
 * suppression of egalitarian alternatives makes it function as a 'Snare'
 * for the bottom 80%.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/pareto_principle].
 * 2. Multi-perspective: ?- multi_index_report(pareto_principle).
 * 3. Run tests: ?- run_tests(pareto_principle_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */