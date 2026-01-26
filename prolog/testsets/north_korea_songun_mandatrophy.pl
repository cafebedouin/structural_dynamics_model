% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: north_korea_songun_mandatrophy
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: DPRK Economic Outlook 2024/2025 / Songun Policy Analysis
% ============================================================================

:- module(constraint_nk_mandatrophy, []).

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
 * * constraint_id: north_korea_songun_mandatrophy
 * human_readable: North Korean Songun Mandatrophy
 * domain: political/economic/military
 * temporal_scope: 1995-Present (Songun Era)
 * spatial_scope: North Korea (DPRK)
 * * SUMMARY:
 * North Korean Mandatrophy is the ultimate expression of extracting the 
 * "civilian margin" to preserve a "military mandate." Under the Songun 
 * (Military-First) policy, the regime extracts up to 34% of GDP for military 
 * and nuclear expenditures while the foundational margins of the state—
 * agricultural fertility, public electricity, and basic human capital—atrophy 
 * to near-collapse levels.
 * * KEY AGENTS:
 * - The Kim Regime: Institutional; the architect of the Songun mandate, 
 * viewing the extraction as a necessity for "Juche" (self-reliance).
 * - The North Korean Farmer/Worker: Individual powerless; subject to 
 * "mass mobilization" and food rationing systems that fail to meet 
 * survival minimums.
 * - China: Institutional/External; provides the "Strategic Rope" (food, 
 * fuel, and 90% of trade) that prevents the Snare of mandatrophy from 
 * reaching its terminal state (collapse).
 * * NARRATIVE ARC:
 * The Songun policy was established as a Rope to coordinate national survival 
 * after the fall of the Soviet Union. However, it functioned as a Snare, 
 * cannibalizing the "Arduous March" margin. By 2025, the system persists in 
 * a state of "brittle stability," saved from the physical Mountain of 
 * collapse only by the external arbitrage of Chinese (and recently Russian) 
 * economic support.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(dprk_songun_cycle, 0, 10).
narrative_ontology:constraint_claim(north_korea_songun_mandatrophy, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.95. Near-total extraction; 34% of GDP and the highest 
% military spending per capita ($9,929 PPP) in the world, while 
% significant portions of the populace suffer from malnutrition.
domain_priors:base_extractiveness(north_korea_songun_mandatrophy, 0.95).

% Suppression score (0.0-1.0)
% Rationale: 0.9. Total suppression of internal market alternatives 
% and information; the state maintains a monopoly on food sales and 
% foreign trade to ensure the mandate remains unchallenged.
domain_priors:suppression_score(north_korea_songun_mandatrophy, 0.9).

% Enforcement requirements
% Requires extreme active enforcement (Totalitarian surveillance, 
% political prison camps, and the "Songbu" social classification).
domain_priors:requires_active_enforcement(north_korea_songun_mandatrophy).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, extractiveness, 0.95).
narrative_ontology:constraint_metric(north_korea_songun_mandatrophy, suppression_requirement, 0.9).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(north_korea_songun_mandatrophy, kpa_military_leadership).
constraint_beneficiary(north_korea_songun_mandatrophy, regime_political_continuity).
constraint_victim(north_korea_songun_mandatrophy, civilian_infrastructure_resilience).
constraint_victim(north_korea_songun_mandatrophy, regional_agricultural_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE KIM REGIME - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to dictate the 20x10 Regional Development 
         Plan and military priorities.
   WHEN: generational - Preserving the Kim dynasty across three generations.
   WHERE: mobile - Projecting power via nuclear deterrence and cyber-theft.
   SCOPE: national - The "Unified Leadership" of the DPRK.
   
   WHY THIS CLASSIFICATION:
   For the leadership, Songun is a Rope—a vital coordination mechanism. It 
   is the only way to weave national defense and domestic order in a 
   hostile "encircled" environment. They view the extraction of margins 
   as a patriotic duty of the people to ensure Juche.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    north_korea_songun_mandatrophy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(north_korea_songun_mandatrophy, E),
    E < 0.99,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NORTH KOREAN CITIZEN - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the "People's Economic Planning Law" 
         and the absence of private property.
   WHEN: immediate - Daily tactical struggle for calories and warmth.
   WHERE: trapped - Within a sealed border environment (3.5 years lockdown).
   SCOPE: local - Their home province or work unit.
   
   WHY THIS CLASSIFICATION:
   For the average citizen, the mandate is a Mountain. It is an unchangeable 
   law of the world. They cannot "negotiate" with the military-first 
   rationing; it is as natural as the terrain. They simply adapt 
   to the "permanence" of the scarcity.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    north_korea_songun_mandatrophy,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(north_korea_songun_mandatrophy),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: CHINESE STRATEGIC COMMAND - Rope (Arbitrage)
   --------------------------------------------------------------------------
   
   WHO: institutional/external - Provides the lifeline (oil, food, trade).
   WHEN: biographical - Avoiding a "Sudden Contingency" (collapse) scenario.
   WHERE: mobile - Managing the border buffer zone.
   SCOPE: regional - Northeast Asian stability.
   
   WHY THIS CLASSIFICATION:
   Beijing treats North Korea as a Rope. By providing the minimum necessary 
   economic "tension" (support), they prevent the system from snapping 
   into a terminal Mountain of collapse (refugees and US troops on the 
   Yalu). For China, North Korea's mandatrophy is a manageable cost 
   to maintain a strategic buffer.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    north_korea_songun_mandatrophy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:suppression_score(north_korea_songun_mandatrophy, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INDIVIDUAL CITIZEN (SUBJECT) - Snare
   --------------------------------------------------------------------------
   WHO: agent_power(individual_powerless) - Subjects under total state mandate.
   WHEN: generational - Lifelong exposure to Songun resource extraction.
   WHERE: exit_options(trapped) - Geographically and politically locked.
   SCOPE: spatial_scope(local) - Individual survival and family unit.
   
   WHY THIS CLASSIFICATION:
   The "Snare" is the totalizing Songun mandate. While the state uses this 
   as a "Rope" for institutional survival, the individual feels a 
   terminal tightening. With extraction (0.95) and suppression (0.95) at 
   near-absolute levels, the citizen has zero margin. This is a 
   "Mandatrophic" state where the individual is actively consumed to 
   sustain the rigid military architecture.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    north_korea_songun_mandatrophy,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(north_korea_songun_mandatrophy),
    !.

% Explicit terminal priors reflecting the collapse of individual margin.
domain_priors:base_extractiveness(north_korea_songun_mandatrophy, 0.95).
domain_priors:suppression_score(north_korea_songun_mandatrophy, 0.95).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(nk_mandatrophy_tests).

test(extraction_paradox) :-
    % Verify high extraction while external support prevents terminal failure.
    domain_priors:base_extractiveness(north_korea_songun_mandatrophy, E),
    E > 0.9.

test(china_as_scaffold) :-
    % China's role is to prevent the Snare from closing.
    constraint_indexing:constraint_classification(north_korea_songun_mandatrophy, rope, context(institutional, biographical, mobile, regional)).

:- end_tests(nk_mandatrophy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. MANDATROPHY APPLICATION: North Korea is the purest example of the 
 * term. The "Songun" mandate is explicitly a "Military-First" extraction 
 * of the civilian margin. 
 * 2. CHINESE ARBITRAGE: The user is correct that the system only survives 
 * because of China. In this framework, China is the "Rope" that prevents 
 * the Snare of mandatrophy from hitting the Mountain of absolute collapse.
 * 3. NEW STABILITY: I noted that Russia (Moscow) has recently provided a 
 * "Complementary Rope" in 2024/2025, reducing China's total trade share 
 * but not its strategic centrality.
 */

omega_variable(
    support_threshold_rupture,
    "At what point does North Korea's mandatrophic depletion become a 
     strategic liability that China can no longer support via a Rope?",
    resolution_mechanism("Monitoring the ratio of Chinese energy exports to 
    the rate of North Korean missile provocations that trigger US/NATO escalation"),
    impact("If export cuts occur: The Rope has snapped. If support holds: 
            The Rope remains strategic arbitrage."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The "Reform and Opening" (Vietnamese/Chinese Model)
 * Viability: Proven to restore margins while maintaining political core.
 * Suppression: Systemically suppressed by the Kim regime to prevent 
 * "informational contamination" and loss of totalitarian control.
 * * CONCLUSION:
 * The rejection of reform (the "Alternative Rope") in favor of permanent 
 * extraction confirms the Snare classification for the populace.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [north_korea_songun_mandatrophy].
% 2. Analyze: ?- multi_index_report(north_korea_songun_mandatrophy).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
