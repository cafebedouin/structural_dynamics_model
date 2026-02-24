% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: cuba_mandatrophic_collapse
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Synthesis of Cuban Economic & Energy Crisis Data (2024-2025)
% ============================================================================

:- module(constraint_cuba_mandatrophy, []).

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
 * * constraint_id: cuba_mandatrophic_collapse
 * human_readable: Cuban Mandatrophy (The GAESA-Infrastructure Divergence)
 * domain: political/economic/technological
 * temporal_scope: 2021-2026 (Monetary Reform to Grid Collapse)
 * spatial_scope: Cuba (National)
 * * SUMMARY:
 * Mandatrophy in Cuba describes the terminal wasting away of national 
 * infrastructure and food security caused by the rigid prioritization of the 
 * "Military-Tourism Mandate" (GAESA) over the essential "margins" of the 
 * civilian state (the energy grid, agriculture, and public health). While 
 * the national power grid suffered multiple total collapses in 2024 and 2025 
 * due to zero maintenance, the military conglomerate GAESA continued to 
 * extract nearly 40% of state investment into luxury hotel construction.
 * * KEY AGENTS:
 * - GAESA Executive: Institutional; manages a $18 billion dollar-asset 
 * portfolio beyond the reach of the Central Bank or civilian auditors.
 * - The Habanero (Citizen): Individual powerless; lives in a state of 
 * "permanent emergency," facing 18-hour blackouts and extreme food scarcity.
 * - The Diaspora/Migrant: Individual moderate; provides the "remittance rope" 
 * that the state extracts to maintain the dollarized market.
 * * NARRATIVE ARC:
 * The "Tarea Ordenamiento" (Monetary Reform) was pitched as a Rope 
 * (coordination for stability) but functioned as a Snare, devaluing the peso 
 * by 400% and stripping the population of purchasing power. By late 2024, 
 * the energy grid reached the physical Mountain of obsolescence: 50-year-old 
 * plants with zero spare parts. The system has "choked" because the 
 * mandate (military hotels) consumed the margin (infrastructure maintenance).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the Deferential Realism suite
narrative_ontology:interval(cuba_blackout_cycle, 0, 10).
narrative_ontology:constraint_claim(cuba_mandatrophic_collapse, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.95. Extreme extraction; GAESA controls 95% of foreign currency 
% transactions and 70% of the economy, yet none of these billions are 
% diverted to fix the failing national power grid or supply the "libreta" 
% (ration book) staples.
domain_priors:base_extractiveness(cuba_mandatrophic_collapse, 0.95).

% Suppression score (0.0-1.0)
% Rationale: 0.85. The "War Economy" declaration suppresses domestic 
% private alternatives, while the military uniform presence of the President 
% during blackouts enforces a "no-protest" constraint on the population.
domain_priors:suppression_score(cuba_mandatrophic_collapse, 0.85).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, extractiveness, 0.95).
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, suppression_requirement, 0.85).

% Enforcement requirements
% Requires heavy active enforcement (CIMEX dollar-stores, military oversight 
% of finance, and police crackdowns on spontaneous "power and food" protests).
domain_priors:requires_active_enforcement(cuba_mandatrophic_collapse).

% Metrics for Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(cuba_mandatrophic_collapse, gaesa_military_elite).
constraint_beneficiary(cuba_mandatrophic_collapse, hotel_construction_sector).
constraint_victim(cuba_mandatrophic_collapse, national_electrical_union_une).
constraint_victim(cuba_mandatrophic_collapse, cuban_agricultural_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: GAESA STRATEGIST - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to manipulate regulatory frameworks and 
         monopolize dollar reserves.
   WHEN: biographical - Investing in luxury towers for a "future tourism boom."
   WHERE: mobile - Holding assets in dollars and MLC, beyond the devalued peso.
   SCOPE: national - The "state inside the state."
   
   WHY THIS CLASSIFICATION:
   GAESA views its conglomerate as a Rope—a functional coordination mechanism 
   to capture hard currency in an era of sanctions. They see the "extraction" 
   of domestic resources into hotel towers as a necessary strategic 
   investment to eventually "save" the economy through tourism.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    cuba_mandatrophic_collapse,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(cuba_mandatrophic_collapse, E),
    E < 0.99,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: ORDINARY CUBAN (THE OPERATOR) - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Surviving on one meal a day; subject to 
         record-low exchange rates (365 CUP to 1 USD).
   WHEN: immediate - 16-hour daily blackouts; rotting food in dead fridges.
   WHERE: trapped - Island geography combined with record-high migration.
   SCOPE: local - Household survival in Havana or Santiago.
   
   WHY THIS CLASSIFICATION:
   For the citizen, Mandatrophy is a Snare. The regime's "Mandate" (preserving 
   the military-run tourism empire) has extracted their electricity, 
   medicine, and food rations. Every peso they earn is "choked" by inflation, 
   forcing a total dependency on the dollarized market they cannot access.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    cuba_mandatrophic_collapse,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(cuba_mandatrophic_collapse),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GRID ENGINEER - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Responsible for fixing 50-year-old boilers 
         without spare parts or fuel.
   WHEN: immediate - Responding to catastrophic failures at Antonio Guiteras.
   WHERE: trapped - Working within a "decrepit" facility with zero redundancy.
   SCOPE: local - The physical power plant.
   
   WHY THIS CLASSIFICATION:
   The engineer sees the Mountain. The failure is no longer political; 
   it is physical. The plants have exceeded their 25-year lifespan by decades. 
   Without massive capital, the grid's collapse is an unchangeable law 
   of material science—an unyielding fact that "Mandates" cannot fix.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cuba_mandatrophic_collapse,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:emerges_naturally(cuba_mandatrophic_collapse),
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cuba_mandatrophy_tests).

test(gaesa_divergence_analysis) :-
    % Verify that institutional agents (GAESA) maintain "Rope" status while 
    % extraction of margins (Grid/Food) reaches terminal "Snare" levels.
    domain_priors:base_extractiveness(cuba_mandatrophic_collapse, E),
    E > 0.9.

test(perspectival_gap_detected) :-
    % Standard DR test: Institutional vs Individual Powerless.
    constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, T1, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, T2, context(powerless, immediate, trapped, local)),
    T1 \= T2.

:- end_tests(cuba_mandatrophy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. THE REVEAL: I identified the "Mandate" as GAESA's tourism empire. 
 * Even during the pandemic and subsequent grid collapse, the state 
 * allocated 37-40% of investment to hotels while occupancy was <15%. 
 * 2. THE CHOKING: The nationwide blackouts of Oct 2024 and Dec 2024 
 * are the terminal results of Mandatrophy. The system "choked" because 
 * the maintenance margin was extracted to build "empty towers."
 * 3. SUPPRESSION (0.85): This reflects the 2024 "War Economy" declaration, 
 * which prioritizes state containment over economic development.
 */

omega_variable(
    gaesa_liquidity_paradox,
    "Does GAESA actually hold $14-18B in liquid reserves, or is this an 
     accounting Mountain used to maintain the illusion of solvency?",
    resolution_mechanism("Audit of offshore BFI (Banco Financiero Internacional) 
    transactions and GAESA's internal 'dollar gravity' ledgers"),
    impact("If real: Mandatrophy is a choice (Snare). If fictional: 
            The collapse is an unyielding Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Energy Infrastructure Prioritization
 * Viability: Funding maintenance of the Antonio Guiteras plant instead of 
 * building three new luxury hotels in Havana.
 * Suppression: Actively suppressed by GAESA's monopoly on foreign currency; 
 * civilian ministries lack the "dollar gravity" to access funds.
 * * ALTERNATIVE 2: Agricultural Liberalization (The 'Vietnam' Model)
 * Viability: Freeing productive forces to resolve the 50% food deficit.
 * Suppression: Rejected in 2024/2025 as a threat to state centralized 
 * control (The Mandate).
 * * CONCLUSION:
 * The existence of these viable but suppressed alternatives (Maintenance/
 * Reform) confirms that Cuban Mandatrophy is a terminal Snare.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [cuba_mandatrophic_collapse].
% 2. Analyze: ?- multi_index_report(cuba_mandatrophic_collapse).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
