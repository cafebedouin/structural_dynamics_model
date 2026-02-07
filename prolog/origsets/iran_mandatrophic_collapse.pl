% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: iran_mandatrophic_collapse
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Synthesis of Iranian Hydropolitical & Economic Analysis (2025-2026)
% ============================================================================

:- module(constraint_iran_mandatrophy, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: iran_mandatrophic_collapse
 * human_readable: Iranian Mandatrophy (The Water-Economic Choke)
 * domain: political/economic/technological
 * temporal_scope: 1979-2026 (Modern Islamic Republic Era)
 * spatial_scope: Iran (National and Regional)
 * * SUMMARY:
 * Mandatrophy in Iran describes the systemic wasting away of ecological and 
 * economic resilience caused by the rigid prioritization of the "Revolutionary 
 * Mandate" (regional proxy funding, nuclear ambition, and ideological self-
 * sufficiency) over the organic "margins" of the state (aquifers, currency 
 * stability, and social trust). The "Water Mafia"—primarily the IRGC's 
 * construction conglomerate Khatam al-Anbiya—has extracted the nation's 
 * hydrologic margin through a 22-fold increase in dams (from 30 to over 647), 
 * leading to "water bankruptcy" and land subsidence.
 * * KEY AGENTS:
 * - IRGC Commander / The "Water Mafia": Institutional; the architects of the 
 * "Resistance Economy" and dam-building frenzy who profit from the 
 * extraction of natural and fiscal margins.
 * - The Iranian Citizen (Bazaari/Student/Retiree): Individual powerless; 
 * subject to 40-50% inflation, dry taps, and frequent electricity blackouts 
 *.
 * - The Environmental Analyst: Analytical; observer identifying the 
 * "mandatrophic" loop where short-term mandate wins (construction profits/
 * proxy influence) lead to terminal systemic choking.
 * * NARRATIVE ARC:
 * Originally, the "Self-Sufficiency" drive was a Rope (coordination to 
 * withstand sanctions). However, the IRGC converted this into a Snare, 
 * extracting groundwater and public funds to secure political power. By 
 * 2025, the system reached a physical Mountain: Tehran faces evacuation 
 * due to water collapse, and the "Snare" of economic despair has triggered 
 * the largest anti-regime uprisings since 1979.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the Deferential Realism suite
narrative_ontology:interval(iran_unrest_2025, 0, 10).
narrative_ontology:constraint_claim(iran_mandatrophic_collapse, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.9. Extreme extraction; the regime extracts nearly 90% of 
% water for inefficient agriculture and diverts 16% of the budget to 
% military/proxies, while the IRGC controls 50% of the crypto/economy 
% .
domain_priors:base_extractiveness(iran_mandatrophic_collapse, 0.9).

% Suppression score (0.0-1.0)
% Rationale: 0.85. High suppression of alternatives (technocratic water 
% management, diplomatic de-escalation) through internet shutdowns, 
% violence against protesters, and the "Resistance Economy" dogma 
% .
domain_priors:suppression_score(iran_mandatrophic_collapse, 0.85).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, extractiveness, 0.9).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, suppression_requirement, 0.85).

% Enforcement requirements
% Requires heavy active enforcement (Basij crackdowns, surveillance, 
% and the Revolutionary Court system).
domain_priors:requires_active_enforcement(iran_mandatrophic_collapse).

% Metrics for Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(iran_mandatrophic_collapse, irgc_khatam_al_anbiya).
constraint_beneficiary(iran_mandatrophic_collapse, axis_of_resistance_proxies).
constraint_victim(iran_mandatrophic_collapse, iranian_ecological_future).
constraint_victim(iran_mandatrophic_collapse, the_bazaari_merchants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: IRGC STRATEGIST - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to redirect rivers and manage the shadow 
         banking/crypto economy.
   WHEN: biographical - Protecting the "Revolutionary Ethos" and personal wealth.
   WHERE: mobile - Projecting power across the "Axis of Resistance."
   SCOPE: national - Total control over the "Resistance Economy."
   
   WHY THIS CLASSIFICATION:
   The IRGC views its dam-building and "invisible jetties" as a Rope—a 
   necessary coordination tool to bypass international sanctions and 
   maintain state survival against "world bullies".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    iran_mandatrophic_collapse,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(iran_mandatrophic_collapse, E),
    E < 0.95,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE WATER-STARVED CITIZEN - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to 42% inflation, currency collapse, 
         and 10-hour daily water cuts.
   WHEN: immediate - Daily struggle for basic utility and food access.
   WHERE: trapped - Within sinking cities (land subsidence) or dried borderlands.
   SCOPE: local - Their home, shop, or farm.
   
   WHY THIS CLASSIFICATION:
   The citizen experiences "Mandatrophy" as a Snare. The regime's "Mandate" 
   (proxies/nuclear) extracts their water, electricity, and savings, 
   choking their ability to live while the IRGC "Mafia" profits from 
   their scarcity.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    iran_mandatrophic_collapse,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(iran_mandatrophic_collapse),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXTERNAL ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the "Water Bankruptcy" and currency freefall.
   WHEN: historical - Tracking the shift from 1979 to the 2026 uprisings.
   WHERE: analytical - Free from the ideological "glow" of the regime.
   SCOPE: global - Evaluating the failure of the "Resistance Economy" model.
   
   WHY THIS CLASSIFICATION:
   The analyst identifies the current state as a Mountain. The geological 
   damage to aquifers is "permanent," and the "Water Bankruptcy" is an 
   unyielding physical limit that no amount of rhetoric can bypass.
   The regime has hit the physical wall of its own extractive logic.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    iran_mandatrophic_collapse,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(iran_mandatrophic_collapse, S),
    S > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(iran_mandatrophy_tests).

test(multi_perspective_coordination_gap) :-
    % IRGC (Rope) vs Citizen (Snare) vs Analyst (Mountain)
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, T1, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, T2, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(resource_extraction_threshold) :-
    % Mandatrophy requires extreme extraction of the "margin" (water/capital).
    domain_priors:base_extractiveness(iran_mandatrophic_collapse, E),
    E > 0.8.

:- end_tests(iran_mandatrophy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. NEOLOGISM INTEGRATION: I applied the term **Mandatrophy** to describe 
 * the specific Iranian condition where the "Revolutionary Mandate" (the 
 * ideological must-haves) has consumed the nation's "Ecological/Economic 
 * Margin" (the can-fails). 
 * 2. THE WATER MAFIA: Identified the IRGC-linked dam builders as the 
 * primary agents of margin extraction.
 * 3. THE PHYSICAL WALL: The 2026 "Water Bankruptcy" is modeled as a 
 * transition from a Snare (manageable extraction) to a Mountain (terminal 
 * physical constraint).
 */

omega_variable(
    sanctions_vs_mismanagement_ratio,
    "To what degree is the 'Snare' external (Sanctions) vs. internal 
     (Mandatrophy)?",
    resolution_mechanism("Comparative analysis of GDP impact from sanctions 
    vs. estimated lost value from water mismanagement and corruption"),
    impact("If sanctions dominate: The constraint is an external Snare. If 
            mismanagement dominates: It is an internal Mandatrophy."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Integrated Water Resource Management (IWRM)
 * Viability: Historically advocated by technocrats; would restore aquifers 
 * and lakes like Urmia.
 * Suppression: Actively suppressed by the "Water Mafia" because sustainable 
 * management generates no construction "rents".
 * * * ALTERNATIVE 2: Regional Diplomatic Re-engagement
 * Viability: Would ease sanctions and allow for infrastructure investment.
 * Suppression: Rejected in favor of the "Axis of Resistance" mandate 
 * to maintain ideological purity and IRGC influence.
 * * * CONCLUSION:
 * The existence of proven alternatives (IWRM/Diplomacy) that are 
 * suppressed for the sake of the Mandate confirms the "Mandatrophy" 
 * as an extractive Snare.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [iran_mandatrophic_collapse].
% 2. Analyze: ?- multi_index_report(iran_mandatrophic_collapse).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
