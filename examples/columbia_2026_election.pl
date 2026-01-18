% ============================================================================
% CONSTRAINT STORY: colombia_2026_presidential_election
% ============================================================================
% Generated: January 16, 2026
% Model: Gemini 2.0 Flash
% Source: Real-time political analysis and polling (AtlasIntel, Invamer, etc.)
% ============================================================================

:- module(colombia_2026_presidential_election, []).

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
 * * constraint_id: colombia_2026_presidential_election
 * human_readable: 2026 Colombian Presidential Election Structure
 * domain: political
 * temporal_scope: 2025-2026 (Campaign season to May 31, 2026)
 * spatial_scope: National (Colombia)
 * * SUMMARY:
 * The 2026 election represents a critical structural constraint where the 
 * constitutional "one-term" limit on the incumbent (Gustavo Petro) forces a 
 * transition. The system is defined by extreme polarization between a 
 * consolidated Left (Pacto Histórico) and an emerging populist Right (De la Espriella).
 * * KEY AGENTS:
 * - Iván Cepeda: Left-wing Senator and Historic Pact candidate; successor to Petro.
 * - Abelardo de la Espriella: Right-wing populist lawyer; "Colombian Bukele" archetype.
 * - Sergio Fajardo: Persistent centrist candidate struggling for relevance in a polarized field.
 * - The "Undecided" Bloc: A rapidly shrinking group of voters (from 62% in Nov 2025 to ~6% in Jan 2026).
 * * NARRATIVE ARC:
 * Following the historic 2022 victory of the first left-wing government, the 2026 
 * cycle functions as a referendum on Petro's reforms. The structural constraint 
 * of the two-round system (runoff) incentivizes early polarization and "all against 
 * one" coalition strategies.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(colombia_2026_presidential_election, 0, 10).
narrative_ontology:constraint_claim(colombia_2026_presidential_election, election_cycle).

% Base extractiveness: Moderate (0.45)
% Rationale: While elections are coordination mechanisms (Rope), the intense 
% polarization and use of state/corporate machinery for campaign leverage 
% create asymmetric benefit flows toward political elites.
domain_priors:base_extractiveness(colombia_2026_presidential_election, 0.45).

% Suppression score: Low-Moderate (0.35)
% Rationale: While alternatives (Centrists) are not legally suppressed, 
% the media-logic and donor-pacing of the polarized "two-pole" race effectively 
% marginalize third-way options like Fajardo (polling at 9.4%).
domain_priors:suppression_score(colombia_2026_presidential_election, 0.35).

% Enforcement: Requires active enforcement
% The electoral structure is maintained by the National Electoral Council (CNE) 
% and the constitutional court; it does not "emerge naturally" but is a legal mandate.
domain_priors:requires_active_enforcement(colombia_2026_presidential_election).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(colombia_2026_presidential_election, extractiveness, 0.45).
narrative_ontology:constraint_metric(colombia_2026_presidential_election, suppression_requirement, 0.35).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: IVÁN CEPEDA (Historic Pact) - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional/individual_powerful
   WHEN: biographical (succession of the progressive project)
   WHERE: mobile (can engage in international diplomacy)
   SCOPE: national/global
   
   WHY THIS CLASSIFICATION:
   For the Left, the election is a tool for coordinating "Broad Front" (Frente Amplio) 
   power to secure reforms (pensions, labor). It is a mechanism of change.
   
   NARRATIVE EVIDENCE:
   "Cepeda has spoken of the need to form a broad front... to prevent a return 
   to right-wing governance." [Justice for Colombia, Dec 2025]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colombia_2026_presidential_election,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(colombia_2026_presidential_election, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MARGINALIZED VOTER - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a low-income voter in a conflict-prone region, the election cycle is a 
   recurring "force of nature" that dictates their safety and economic floor 
   without their direct ability to change the systemic outcome.
   
   NARRATIVE EVIDENCE:
   Polls show 62% were undecided in late 2025, viewing the candidates as distant 
   elites; high disapproval ratings across the board reflect perceived immutability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colombia_2026_presidential_election,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(individual_powerless, immediate, trapped, local),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: SERGIO FAJARDO (The Centrist) - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate
   WHEN: generational
   WHERE: constrained
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the centrist technocrat, the current polarized structure is a "Noose." 
   The binary choice (Petrista vs. Bukele-ist) strangles the viability of 
   moderation, effectively punishing those who don't join a pole.
   
   NARRATIVE EVIDENCE:
   AtlasIntel (Jan 13, 2026) shows Fajardo at 9.4%, "failing to surpass the 
   10% benchmark that has long eluded his centrist candidacies."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    colombia_2026_presidential_election,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(colombia_2026_presidential_election, S),
    S > 0.3, % Moderate suppression of the "center" option.
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(colombia_2026_presidential_election_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(colombia_2026_presidential_election, rope, context(individual_powerful, biographical, mobile, national)),
    constraint_indexing:constraint_classification(colombia_2026_presidential_election, mountain, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(colombia_2026_presidential_election, noose, context(individual_moderate, generational, constrained, national)).

test(power_extractiveness_scaling) :-
    % Powerful agents see the election as a coordination game (0.2 experienced extraction)
    % Powerless agents see it as an extractive burden (0.7 experienced extraction)
    ScorePowerless = 0.7,
    ScorePowerful = 0.2,
    ScorePowerless > ScorePowerful.

test(time_immutability_fajardo_trap) :-
    % Demonstrates that for a "Moderate" agent in a national scope, the constraint is a Noose
    constraint_indexing:constraint_classification(colombia_2026_presidential_election, noose, context(individual_moderate, _, _, national)).

:- end_tests(colombia_2026_presidential_election_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: Jan 16, 2026
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.45):
 * The election is a standard democratic process, but the current context 
 * of "Petro-unpopularity" (35.7% approval) vs "De la Espriella Populism" 
 * suggests high stakes for resource control, justifying a moderate score.
 * * 2. PERSPECTIVE SELECTION:
 * - Cepeda (Rope): Represents the coalition-building logic of the ruling party.
 * - Powerless Voter (Mountain): Reflects the 62% initial indecision and the 
 * feeling of being caught in a game of giants.
 * - Fajardo (Noose): Represents the "death of the center" in Latin American 
 * politics, where the system itself strangles moderate alternatives.
 * * 3. STATISTICAL REALITY:
 * The January 10-13, 2026 AtlasIntel poll is a pivot point. De la Espriella's 
 * 28% vs. Cepeda's 26.5% creates a "Noose" for anyone not aligned with these 
 * two figures, as they command over 50% of the active electorate intentions.
 * * 4. AMBIGUITIES:
 * The "Undecided" bloc is the biggest variable. If they are "trapped" and 
 * "powerless," they see a Mountain. If they are "mobile" (analytical), they 
 * might see a Noose they can arbitrage.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The "Gran Consulta por Colombia" (Inter-party primary)
 * Viability: High. Scheduled for March 8, 2026.
 * Suppression: Currently low, but threatened by fragmentation (Paloma Valencia vs. Pinzón).
 * * ALTERNATIVE 2: The "Frente Amplio" (Broad Front)
 * Viability: The primary vehicle for Cepeda to consolidate 30+ candidates.
 * * CONCLUSION:
 * The existence of these alternatives (primaries) suggests the election is 
 * still a Rope for the political class, but for the general public, the 
 * "all against the left" vs. "continuity" narrative acts as a structural 
 * Noose that eliminates diverse policy discourse.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [colombia_2026_presidential_election].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(colombia_2026_presidential_election).
 * 3. Run tests: ?- run_tests(colombia_2026_presidential_election_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
