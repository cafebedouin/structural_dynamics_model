% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: taiwan_strait_hegemony_shift
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Geopolitical Strategic Synthesis / Indo-Pacific Security Analysis
% ============================================================================

:- module(constraint_taiwan_hegemony, []).

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
 * * constraint_id: taiwan_strait_hegemony_shift
 * human_readable: The Taiwan Strait Energy & Logistics Chokepoint
 * domain: political/economic/geographical
 * temporal_scope: Post-Unification Era (Hypothetical)
 * spatial_scope: Indo-Pacific (Taiwan, Japan, Philippines, South Korea)
 * * SUMMARY:
 * This constraint models the geopolitical "lock" created by Chinese control 
 * of Taiwan. By occupying the "First Island Chain" anchor, China gains 
 * direct control over the sea lines of communication (SLOCs) that provide 
 * 90% of Japan's energy and a vast majority of its trade. This physical 
 * reality creates a "Client State Gravitational Pull," forcing Japan and 
 * neighboring states to subordinate their foreign policy to Beijing to 
 * ensure national survival.
 * * KEY AGENTS:
 * - The Japanese Prime Minister: Individual powerless; facing the choice 
 * between national starvation and diplomatic subordination.
 * - PLAN (People's Liberation Army Navy): Institutional; the enforcer of 
 * the new maritime "Mountain" that dictates regional flow.
 * - Southeast Asian State (e.g., Philippines): Analytical/Trapped; 
 * observing the "Snare" of the South China Sea tighten into a closed lake.
 * * NARRATIVE ARC:
 * What was once a Rope (international maritime law coordinating free trade) 
 * transforms into a Mountain (the unchangeable physical presence of Chinese 
 * sensors and missiles on Taiwan's east coast). For Japan, this Mountain 
 * acts as a Snare, extracting sovereignty in exchange for the continued 
 * flow of oil and food.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the Deferential Realism suite
narrative_ontology:interval(hegemony_transition_window, 0, 10).
narrative_ontology:constraint_claim(taiwan_strait_hegemony_shift, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.9. Total extraction of strategic autonomy. Japan's 
% existence becomes a "permission-based" reality granted by the hegemon.
domain_priors:base_extractiveness(taiwan_strait_hegemony_shift, 0.9).

% Suppression score (0.0-1.0)
% Rationale: 0.8. High suppression of alternative security architectures 
% (e.g., the U.S. alliance) as the physical barrier of Chinese control 
% renders them logistically unviable.
domain_priors:suppression_score(taiwan_strait_hegemony_shift, 0.8).

% Enforcement requirements
% Requires active enforcement (PLAN patrols, submarine bastions in the 
% deep waters east of Taiwan, and ADIZ control).
domain_priors:requires_active_enforcement(taiwan_strait_hegemony_shift).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, extractiveness, 0.9).
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(taiwan_strait_hegemony_shift, chinese_regional_primacy).
constraint_beneficiary(taiwan_strait_hegemony_shift, land_based_eurasian_logistics).
constraint_victim(taiwan_strait_hegemony_shift, japanese_sovereign_autonomy).
constraint_victim(taiwan_strait_hegemony_shift, south_korean_security_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: JAPANESE LEADERSHIP - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the physical geography of their 
         energy dependency; cannot move the islands of Japan.
   WHEN: immediate - Crisis-level focus on energy and food security.
   WHERE: trapped - The "First Island Chain" has been breached and reversed.
   SCOPE: local - National survival.
   
   WHY THIS CLASSIFICATION:
   To Japan, Chinese control of Taiwan is a Mountain. It is an unchangeable 
   feature of the earth. They cannot "negotiate" with a blockade that 
   exists at the entrance to their home islands; they must simply 
   accept the new terrain of the Indo-Pacific.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    taiwan_strait_hegemony_shift,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(taiwan_strait_hegemony_shift),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: CHINESE STRATEGIC COMMAND - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to set the rules of the new regional order.
   WHEN: generational - Fulfilling the "Great Rejuvenation" and breaking 
         containment.
   WHERE: mobile - Projecting power into the "Second Island Chain" and 
         the Central Pacific.
   SCOPE: national - Total control of the "Near Seas."
   
   WHY THIS CLASSIFICATION:
   Beijing views this control as a Rope—a functional coordination mechanism. 
   It allows them to "weave" the regional economy into a China-centric 
   hierarchy, using the threat of SLOC disruption as a gentle but 
   unyielding tension that keeps neighbors in line.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    taiwan_strait_hegemony_shift,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(taiwan_strait_hegemony_shift, E),
    E < 0.95,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE REGIONAL REBEL / DISSIDENT - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the loss of civil and political autonomy.
   WHEN: historical - Comparing the "Era of Open Seas" to the "Era of Hegemony."
   WHERE: analytical - Mentally resisting the new client-state status.
   SCOPE: global - Impact on the rules-based international order.
   
   WHY THIS CLASSIFICATION:
   The critic sees the Snare. The physical control of Taiwan "chokes" 
   the democratic and sovereign aspirations of the region. By extracting 
   the ability to resist, the hegemon turns the entire Pacific rim 
   into a series of vassal states whose "breath" (energy/trade) is 
   constantly at risk of being cut off.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    taiwan_strait_hegemony_shift,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(taiwan_strait_hegemony_shift, S),
    S > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(taiwan_hegemony_tests).

test(sovereignty_extraction_delta) :-
    % Verify that Japan (Powerless) experiences extreme extraction
    domain_priors:base_extractiveness(taiwan_strait_hegemony_shift, E),
    E > 0.8.

test(geopolitical_lock_in) :-
    % Check if the analytical view correctly identifies the suppression of alternatives
    constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, snare, context(analytical, historical, analytical, global)).

:- end_tests(taiwan_hegemony_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. THE "CLIENT STATE" MECHANISM: Japan's transformation into a client 
 * state is modeled as an energy-logistics Snare. If 90% of your oil passes 
 * through a straw controlled by another, your sovereignty is a fiction.
 * 2. REGIONAL EFFECTS: South Korea and the Philippines are modeled as 
 * "Secondary Nooses"—once Taiwan falls, their own supply lines are 
 * effectively bifurcated and isolated.
 * 3. EXTRACTIVENESS (0.9): This high score reflects the "extraction" of 
 * a nation's soul; it is the ultimate loss of the power to say 'No'.
 */

omega_variable(
    alternative_energy_scaffold,
    "Can Japan develop enough nuclear and renewable autonomy to unloosen 
     the Snare of the Taiwan Strait?",
    resolution_mechanism("Evaluation of Japan's 2050 Energy Plan vs. 
    required maritime imports for industrial survival"),
    impact("If Yes: The Strait remains a Rope for trade. If No: It is a 
            permanent Snare for sovereignty."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Trans-Pacific Bridge (U.S. Alliance)
 * Viability: Currently the "Rope" that secures Japan.
 * Suppression: Becomes a Snare if the U.S. cannot physically penetrate 
 * the A2/AD envelope created by a Chinese-fortified Taiwan.
 * * ALTERNATIVE 2: Arctic Silk Road (Northern Route)
 * Viability: Possible but seasonally constrained and subject to 
 * Russian/Chinese coordination.
 * * CONCLUSION:
 * The collapse of the U.S. alliance (Rope) in the face of the Taiwan 
 * Mountain leads directly to the Hegemonic Snare for the entire 
 * First Island Chain.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [taiwan_strait_hegemony_shift].
% 2. Analyze: ?- multi_index_report(taiwan_strait_hegemony_shift).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
