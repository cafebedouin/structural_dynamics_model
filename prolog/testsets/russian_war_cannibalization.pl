% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: russian_war_cannibalization
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Russian Federal Budget 2025-2027 / Aviation & Utility Crisis Data
% ============================================================================

:- module(constraint_russian_cannibalization, []).

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
 * * constraint_id: russian_war_cannibalization
 * human_readable: Russian Military Cannibalization (The Invisible Mandatrophy)
 * domain: political/economic/technological
 * temporal_scope: 2022-2027 (War Economy Phase)
 * spatial_scope: Russia (National)
 * * SUMMARY:
 * This constraint models the "Invisible Mandatrophy" in Russia, where the state 
 * uses "Military Keynesianism" (massive deficit spending on defense) as a 
 * temporary Rope to mask the extraction of civilian margins. While high wages 
 * for soldiers and factory workers provide a veneer of prosperity, the mandate 
 * (the Ukraine war) has extracted the maintenance margin from public utilities 
 * and aviation. The result is a "Cannibalization Economy" where parts are 
 * stripped from airworthy planes and infrastructure funds are diverted to the 
 * front, tightening a Noose that only becomes visible during physical 
 * failure events (e.g., heating grid collapses or aviation malfunctions).
 * * KEY AGENTS:
 * - The Kremlin / Siloviki: Institutional; rule-makers prioritizing the 
 * "Existential War" mandate over long-term civilian infrastructure.
 * - The Regional Resident (e.g., in Chita or Zabaykalsky): Individual powerless; 
 * facing -25°C temperatures with zero heating due to utility collapse.
 * - The Aviation Engineer: Individual powerless/Analytical; forced to perform 
 * "emergency" repairs with uncertified or cannibalized parts to maintain 
 * the illusion of airworthiness.
 * * NARRATIVE ARC:
 * The 2025 budget represents a terminal transition: for the first time, 
 * pure military spending (13.2 trillion rubles) significantly exceeds 
 * social spending. What was initially a Rope (economic stimulus) has 
 * become a Noose, extracting 2.4 million workers from the labor market and 
 * leaving 1/3 of heating networks in a state of critical decay.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(russia_cannibalization_cycle, 0, 10).
narrative_ontology:constraint_claim(russian_war_cannibalization, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.8. High extraction; the 2025 budget redirects 43% of all 
% federal spending to defense and security, "cannibalizing" the budget 
% for housing, communal services, and human capital.
domain_priors:base_extractiveness(russian_war_cannibalization, 0.8).

% Suppression score (0.0-1.0)
% Rationale: 0.75. High suppression of civilian priorities; any effort to 
% redirect funds back to utilities or education is suppressed by the 
% "existential" framing of the war and the "War Economy" mobilization.
domain_priors:suppression_score(russian_war_cannibalization, 0.75).

% Enforcement requirements
% Requires active enforcement (anti-protest laws, state-controlled media 
% masking infrastructure failure, and military censorship).
domain_priors:requires_active_enforcement(russian_war_cannibalization).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(russian_war_cannibalization, extractiveness, 0.8).
narrative_ontology:constraint_metric(russian_war_cannibalization, suppression_requirement, 0.75).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(russian_war_cannibalization, military_industrial_complex).
constraint_beneficiary(russian_war_cannibalization, front_line_contractors).
constraint_victim(russian_war_cannibalization, civilian_infrastructure_resilience).
constraint_victim(russian_war_cannibalization, domestic_aviation_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE KREMLIN / STATE ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to set the 2025 budget and define "Victory."
   WHEN: biographical - Planning for a "forever war" and regime preservation.
   WHERE: mobile - Projecting power via military spending and deficit management.
   SCOPE: national - Total control over the "War Footing" economy.
   
   WHY THIS CLASSIFICATION:
   The state views the "War Economy" as a Rope—a functional coordination 
   mechanism that keeps the economy growing (via stimulus) and the 
   population unified (via high wages). They treat the extraction of 
   civilian margins as a manageable "trade-off" for national survival.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    russian_war_cannibalization,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(russian_war_cannibalization, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE REGIONAL CITIZEN - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Living in cities like Chita; subject to 
         utility failures and rising inflation.
   WHEN: immediate - Tactical survival during winter freezes.
   WHERE: trapped - Bound by the lack of local infrastructure investment.
   SCOPE: local - Their home and their physical safety.
   
   WHY THIS CLASSIFICATION:
   For the citizen, Mandatrophy is a Noose. The "Mandate" (the war) has 
   extracted the margin that keeps their pipes from bursting. As heating 
   networks collapse in -25°C, they realize the "stimulus" of the war has 
   choked the basic survival infrastructure of their hometown.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    russian_war_cannibalization,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(russian_war_cannibalization),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE AIRLINE PASSENGER / PILOT - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Flying on planes with 4x the malfunction rate 
         of the previous year.
   WHEN: immediate - Mid-flight response to technical failure.
   WHERE: trapped - At 30,000 feet on an uncertified airframe.
   SCOPE: local - A single flight path.
   
   WHY THIS CLASSIFICATION:
   In the air, the failure of a "cannibalized" engine is a Mountain. It 
   is a physical law that cannot be negotiated. The extraction of parts 
   has reached the physical limit of the machine's airworthiness. The 
   mandate of "continuing to fly" hits the unyielding fact of material 
   fatigue.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    russian_war_cannibalization,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(russian_war_cannibalization, S),
    S > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(russian_cannibalization_tests).

test(invisible_mandatrophy_check) :-
    % Extraction of civilian margin (utilities) should be high while 
    % state perception remains "Rope."
    domain_priors:base_extractiveness(russian_war_cannibalization, E),
    E > 0.7.

test(perspectival_asymmetry) :-
    % Institutional (Rope) vs Individual (Mountain/Noose)
    constraint_indexing:constraint_classification(russian_war_cannibalization, T1, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(russian_war_cannibalization, T2, context(individual_powerless, immediate, trapped, local)),
    T1 \= T2.

:- end_tests(russian_cannibalization_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. THE "LESS VISIBLE" MECHANISM: In Russia, Mandatrophy is hidden by 
 * "Military Keynesianism." Unlike Cuba's empty shelves or Iran's dry taps, 
 * Russia's shelves are full of stimulated demand, but the *foundational* * margins (pipes/planes) are being extracted invisibly.
 * 2. AVIATION DATA: The 4x increase in technical malfunctions (800 in 2025 
 * vs 200 in 2024) is a primary sensor for the tightening Noose.
 * 3. BUDGET (2025): The decision to increase defense spending by 25% to 
 * $140bn while cutting social welfare by 16% is the formal "Mandatrophic 
 * Extraction" event.
 */

omega_variable(
    cannibalization_threshold,
    "How many 'unhealthy' aircraft can be stripped to maintain the 
     'healthy' fleet before the entire system hits a terminal Mountain?",
    resolution_mechanism("Monitoring the ratio of airworthy to grounded 
    foreign-made airframes vs. the rate of mid-flight engine failures"),
    impact("If failures plateau: The Rope holds. If they spike: The 
            Noose has closed."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Diplomatic Settlement (Margin Preservation)
 * Viability: Ending the war would return 13.2 trillion rubles to the 
 * civilian margin and allow the import of certified plane parts.
 * Suppression: Explicitly suppressed by the "Existential Threat" narrative 
 * and the 2049 "Great Rejuvenation" timeline.
 * * ALTERNATIVE 2: Import Substitution (The 'Superjet' Scaffold)
 * Viability: Replacing Western parts with domestic ones.
 * Suppression: Failed; even "domestic" jets like the SSJ-100 rely on 
 * Franco-Russian engines that are currently failing due to lack of 
 * Western core components.
 * * CONCLUSION:
 * The failure of the "Scaffold" (Import Substitution) and the suppression 
 * of the "Settlement" alternative proves that Russian Mandatrophy is a 
 * terminal Noose.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [russian_war_cannibalization].
% 2. Analyze: ?- multi_index_report(russian_war_cannibalization).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
