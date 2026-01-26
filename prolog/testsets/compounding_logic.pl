% ============================================================================
% CONSTRAINT STORY: compounding_logic
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: General Economic Theory & Berkshire Hathaway 2024 Letter
% ============================================================================

:- module(constraint_compounding_logic, []).

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
 * constraint_id: compounding_logic
 * human_readable: The Law of Compounding Returns
 * domain: economic/mathematical
 * temporal_scope: Universal / Long-term
 * spatial_scope: Global
 * 
 * SUMMARY:
 * Compounding is the mathematical constraint where the value of a system increases 
 * exponentially because earnings are reinvested to generate further earnings. 
 * It is constrained by three primary variables: time, rate of return, and the 
 * stability of the underlying medium (currency or asset).
 * 
 * KEY AGENTS:
 * - The Debtor (Individual Powerless): Experiences compounding as an extractive force.
 * - The Capital Allocator (Institutional): Manages the reinvestment cycles.
 * - The Short-Term Speculator (Individual Moderate): Focused on immediate gains, not long-term growth.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(compounding_logic, 0, 10).
narrative_ontology:constraint_claim(compounding_logic, tangled_rope).

% Base extractiveness score: 0.5.
% In a savings context, it creates value; in a debt context, it is 
% highly extractive. We average to 0.5 for the general logic.
domain_priors:base_extractiveness(compounding_logic, 0.5).

% Suppression score: 0.4.
% One can choose not to participate by consuming immediately, 
% though the mathematical "rules" of the results cannot be altered.
domain_priors:suppression_score(compounding_logic, 0.4).

% Enforcement: Requires "stable currency" and "wisdom" in capital allocation.
domain_priors:requires_active_enforcement(compounding_logic).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(compounding_logic, long_term_savers).
constraint_victim(compounding_logic, debtors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DEBTOR - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (One subject to "runaway" interest or "fiscal folly")
   WHEN: immediate (The "cash register rings" for the lender, not them)
   WHERE: constrained ("Paper money" value evaporates, leaving them with debt)
   
   WHY THIS CLASSIFICATION:
   For the debtor, compounding is a 'Snare'. It is a coercive mechanism that 
   extracts their future labor at an accelerating rate. It creates a 
   "hemorrhaging cash" situation that is "hard to ignore," strangling their
   financial freedom.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    compounding_logic,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CAPITAL ALLOCATOR - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manages the reinvestment cycles)
   WHEN: generational (Long-term strategy for wealth creation)
   WHERE: arbitrage (Utilizing the "magic of long-term compounding")
   
   WHY THIS CLASSIFICATION:
   For the capital allocator, compounding is a 'Rope'—a fundamental principle
   for generating long-term wealth and coordinating economic growth. It
   rewards patient, disciplined reinvestment, acting as a powerful tool
   for capital accumulation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    compounding_logic,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SHORT-TERM SPECULATOR - Mountain
   --------------------------------------------------------------------------
   WHO: individual_moderate (One attempting to "come and go on a dime")
   WHEN: immediate (Focused on "year-by-year numbers")
   WHERE: trapped (Bound by the "unpredictable" swings of the market)
   
   WHY THIS CLASSIFICATION:
   For the short-term speculator, the time requirement of compounding is a 'Mountain'. 
   They cannot force "decades" of results into a single year. The 
   mathematical necessity of time is an unchangeable law they must endure,
   often leading to frustration and underperformance.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    compounding_logic,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(compounding_logic_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(compounding_logic, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(compounding_logic, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(compounding_logic, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(compounding_logic_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Capital Allocator' as the
 *    institutional agent. For them, compounding is a fundamental 'Rope' for
 *    long-term wealth creation and economic coordination.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Debtor (Snare): Compounding extracts future labor and strangles finances.
 *    - Capital Allocator (Rope): A tool for exponential wealth generation.
 *    - Short-Term Speculator (Mountain): Unable to overcome the time requirement.
 * 
 * 3. CORE INSIGHT: The law of compounding returns is a mathematical 'Mountain'
 *    that can be harnessed as a powerful 'Rope' for wealth creation but can
 *    also become a 'Snare' for those caught in its negative feedback loop (debt).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty revolves around the long-term stability of the medium and opportunities for reinvestment.
 */

omega_variable(
    reinvestment_availability,
    "Will there always be 'really outstanding businesses' or 'gems' available for reinvestment that offer sufficient returns for compounding to thrive, or will market saturation and diminishing returns turn the 'Rope' into a 'Tangled Rope'?",
    resolution_mechanism("Monitoring market saturation and 'knee-deep' vs 'nothing looks compelling' cycles in capital markets over decades."),
    impact("If gems disappear: The 'Rope' of compounding goes slack, and the system stalls. If opportunities persist: It remains a powerful 'Rope'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Immediate Consumption ("Anti-Compounding" Model)
 *    Viability: The default human behavior, prioritizing present gratification over future gain.
 *    Suppression: Actively suppressed by financial literacy and cultural narratives promoting saving and investment, which frame it as "fiscal folly".
 *
 * CONCLUSION:
 * Compounding is a 'Rope' that requires the active suppression of the
 * alternative of "immediate consumption" to function. This suppression
 * can be a 'Snare' for those who cannot or choose not to defer gratification,
 * leading to a cycle of debt.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/compounding_logic].
 * 2. Multi-perspective: ?- multi_index_report(compounding_logic).
 * 3. Run tests: ?- run_tests(compounding_logic_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */