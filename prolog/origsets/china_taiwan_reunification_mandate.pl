% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: china_taiwan_reunification_mandate
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: PRC 2022/2025 White Papers, "Great Rejuvenation" 2049 Mandate
% ============================================================================

:- module(constraint_china_reunification, []).

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
 * * constraint_id: china_taiwan_reunification_mandate
 * human_readable: The Mandate of National Rejuvenation (PRC Perspective)
 * domain: political/national_identity
 * temporal_scope: 1949–2049 (Centenary Goal)
 * spatial_scope: People's Republic of China / Taiwan Strait
 * * SUMMARY:
 * From the perspective of Beijing, the reunification with Taiwan is not a 
 * strategic choice but a historical necessity and a "core interest" that 
 * cannot be passed down across generations. It is the final step in ending 
 * the "Century of Humiliation" and achieving the "Great Rejuvenation of the 
 * Chinese Nation" by the 2049 centenary.
 * * KEY AGENTS:
 * - PRC Leadership (CCP): Institutional; views reunification as a structural 
 * requirement for the legitimacy of the Chinese Dream.
 * - Mainland Citizen: Individual powerless; socialized to view the "One China" 
 * principle as a natural law of history rather than a policy.
 * - PLA Strategist: Institutional/Analytical; manages the "Snare" of military 
 * deterrence to prevent the "Mountain" of sovereignty from being challenged.
 * * NARRATIVE ARC:
 * The policy has shifted from a "Rope" of peaceful economic integration and 
 * the "1992 Consensus" to a rigid "Mountain" of historical inevitability. 
 * As the 2049 deadline approaches, the "Rope" of "One Country, Two Systems" 
 * is increasingly reinforced by the "Snare" of military exercises like 
 * "Justice Mission 2025" to suppress any deviation from the mandate.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the Deferential Realism suite
narrative_ontology:interval(rejuvenation_centenary_cycle, 0, 10).
narrative_ontology:constraint_claim(china_taiwan_reunification_mandate, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.8. High; the mandate "extracts" massive state resources, 
% military focus, and diplomatic capital to enforce international 
% adherence to the One China Principle.
domain_priors:base_extractiveness(china_taiwan_reunification_mandate, 0.8).

% Suppression score (0.0-1.0)
% Rationale: 0.9. Extreme suppression of the "Taiwan Independence" 
% alternative, which is legally defined as a "red line" triggering 
% the Anti-Secession Law.
domain_priors:suppression_score(china_taiwan_reunification_mandate, 0.9).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, extractiveness, 0.8).
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, suppression_requirement, 0.9).

% Enforcement requirements
% Requires active enforcement (ADIZ incursions, blockade rehearsals, 
% and the diplomatic "Three Propositions").
domain_priors:requires_active_enforcement(china_taiwan_reunification_mandate).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(china_taiwan_reunification_mandate, ccp_regime_legitimacy).
constraint_beneficiary(china_taiwan_reunification_mandate, first_island_chain_control).
constraint_victim(china_taiwan_reunification_mandate, regional_status_quo_stability).
constraint_victim(china_taiwan_reunification_mandate, cross_strait_diplomatic_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: PRC LEADERSHIP - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define the "Taiwan Plan" and the 1C2S framework.
   WHEN: generational - Thinking in terms of the 2049 centenary goal.
   WHERE: mobile - Projecting power across the "First Island Chain."
   SCOPE: national - The core of national security.
   
   WHY THIS CLASSIFICATION:
   For the architects in Beijing, reunification is a Rope—a functional 
   coordination mechanism for national rejuvenation. They view the 
   "One Country, Two Systems" offer as a flexible thread meant to 
   integrate Taiwan while preserving its "social system," provided 
   sovereignty is conceded.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    china_taiwan_reunification_mandate,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(china_taiwan_reunification_mandate, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MAINLAND CITIZEN - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Socialized within the state educational 
         and media apparatus.
   WHEN: immediate - Daily exposure to "patriotic" historical narratives.
   WHERE: trapped - Bound by the consensus of the "One Family" ideology.
   SCOPE: local - National identity and shared kinship.
   
   WHY THIS CLASSIFICATION:
   To the average citizen, Taiwan as part of China is a Mountain. It is 
   not a policy they can debate; it is an unchangeable fact of history 
   and geography. The "return to the motherland" is viewed as an 
   unstoppable trend of history, akin to a natural law.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    china_taiwan_reunification_mandate,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(china_taiwan_reunification_mandate),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PLA STRATEGIST - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical/institutional - Manages the "Justice Mission" military drills.
   WHEN: historical - Reversing the "Century of Humiliation."
   WHERE: analytical - Mapping the "Gray Zone" around the median line.
   SCOPE: global - Counter-intervention against foreign powers.
   
   WHY THIS CLASSIFICATION:
   The strategist views the mandate as a Snare. By tightening the 
   "blockade rehearsals" and diplomatic isolation of Taipei, they 
   intend to "choke" the space for "separatist forces" until the 
   extraction of political surrender becomes the only path forward.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    china_taiwan_reunification_mandate,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(china_taiwan_reunification_mandate, S),
    S > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(china_reunification_tests).

test(historical_inevitability_variance) :-
    % Citizen (Mountain) vs Leadership (Rope)
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, T1, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, T2, context(institutional, generational, mobile, national)),
    T1 \= T2.

test(absolute_suppression_of_independence) :-
    % The mandate requires near-total suppression of the independence alternative.
    domain_priors:suppression_score(china_taiwan_reunification_mandate, S),
    S >= 0.9.

:- end_tests(china_reunification_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. THE "REJUVENATION" SHIFT: The 2049 goal is modeled as a transformation 
 * of the constraint from a passive "Historical Claim" to an active "Strategic 
 * Deadline."
 * 2. SNARE DETECTION: I identified the military's role as a Snare because 
 * current 2025/2026 exercises focus on "Justice Missions" (blockades), which 
 * are extractive and coercive mechanisms aimed at isolating the subject.
 * 3. SUPPRESSION (0.9): Reflects the PRC's refusal to renounce the use of 
 * force, which serves to completely suppress the viability of a 
 * "De Jure Independence" alternative.
 */

omega_variable(
    peaceful_reunification_sincerity,
    "Is 'Peaceful Reunification' a genuine Rope (coordination) or a 
     rhetorical Scaffold hiding the Snare of military coercion?",
    resolution_mechanism("Evaluation of the ratio between economic integration 
    incentives vs. the frequency of blockade-simulation exercises"),
    impact("If incentives dominate: It is a Rope. If blockade-simulations 
            dominate: It is a Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Indefinite Status Quo
 * Viability: Historically the "safe" path; increasingly rejected by 
 * Beijing's claim that the issue "cannot be passed down indefinitely."
 * Suppression: Actively suppressed by 2049 centenary deadlines.
 * * ALTERNATIVE 2: "One China, Two Governments" (Equal Standing)
 * Viability: Proposed by some progressives; strictly suppressed by the 
 * "Three Propositions" (PRC is the ONLY legal government).
 * * CONCLUSION:
 * The existence of these alternatives, and their systemic suppression by 
 * the PRC state, confirms the "Snare/Mountain" classification for those 
 * outside the CCP leadership.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [china_taiwan_reunification_mandate].
% 2. Analyze: ?- multi_index_report(china_taiwan_reunification_mandate).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
