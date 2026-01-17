% ============================================================================
% CONSTRAINT STORY: ergo_autolykos_asic_resistance
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Ergo Platform Documentation (Autolykos v2) & Whitepapers
% ============================================================================

:- module(ergo_autolykos, []).

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
 * * constraint_id: ergo_autolykos_asic_resistance
 * human_readable: Autolykos PoW Algorithm (ASIC Resistance)
 * domain: technological/economic
 * temporal_scope: 2019-Ongoing
 * spatial_scope: Global (Distributed Mining Network)
 * * SUMMARY:
 * Autolykos is Ergo's Proof-of-Work (PoW) algorithm, specifically designed 
 * to be memory-hard and ASIC-resistant. By requiring large 
 * datasets (the dataset size increases over time) to be held in GPU memory, 
 * it prevents specialized mining hardware (ASICs) from dominating the 
 * hash rate, thus favoring commodity GPU hardware.
 * * KEY AGENTS:
 * - GPU Miners: Individual and small-scale participants using off-the-shelf 
 * hardware.
 * - ASIC Manufacturers: Institutional entities that build high-efficiency 
 * specialized hardware.
 * - Network Security: The collective robustness against 51% attacks and 
 * hash rate centralization.
 * * NARRATIVE ARC:
 * Autolykos functions as a democratic barrier. It ensures that 
 * the "Mountain" of mining difficulty can be scaled by common citizens with 
 * gaming GPUs, rather than only by those with access to multi-million dollar 
 * industrial ASIC supply chains.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(ergo_mining_era, 0, 10).
narrative_ontology:constraint_claim(ergo_autolykos_asic_resistance, mountain).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Low direct extraction; however, it extracts energy and requires 
% capital for GPU hardware. It "extracts" opportunity from ASIC makers.
domain_priors:base_extractiveness(ergo_autolykos_asic_resistance, 0.2).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: High suppression of specialized hardware. The algorithm 
% actively punishes non-memory-intensive hardware.
domain_priors:suppression_score(ergo_autolykos_asic_resistance, 0.85).

% Enforcement requirements
domain_priors:requires_active_enforcement(ergo_autolykos_asic_resistance).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(ergo_autolykos_asic_resistance, extractiveness, 0.2).
narrative_ontology:constraint_metric(ergo_autolykos_asic_resistance, suppression_requirement, 0.85).

% Beneficiaries and Victims
constraint_beneficiary(ergo_autolykos_asic_resistance, retail_gpu_miners).
constraint_victim(ergo_autolykos_asic_resistance, asic_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ASIC Manufacturer - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional - Powerful entities with massive R&D budgets.
   WHEN: historical - Building hardware for multi-year cycles.
   WHERE: arbitrage - Seeking the most profitable PoW chains to dominate.
   
   WHY THIS CLASSIFICATION:
   For an ASIC maker, Autolykos is a "Noose." It is a protocol-level 
   mechanism that intentionally renders their specialized efficiency useless, 
   effectively strangling their ability to enter the Ergo market.
   
   NARRATIVE EVIDENCE:
   "The algorithm is memory-hard... aimed at discouraging ASIC 
   development to keep the network decentralized".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_autolykos_asic_resistance,
    noose,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(ergo_autolykos_asic_resistance, retail_participants),
        constraint_victim(ergo_autolykos_asic_resistance, asic_capital),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(ergo_autolykos_asic_resistance, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Home Miner (GPU) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_powerless / moderate - Common users with gaming PCs.
   WHEN: biographical - Seeking supplemental income or supporting a network.
   WHERE: mobile - Can easily switch mining to other GPU-friendly chains.
   
   WHY THIS CLASSIFICATION:
   For the home miner, Autolykos is a "Rope." it is the tool that allows 
   them to compete on a level playing field with industrial farms. 
   It pulls them into the security structure of the network as active 
   participants rather than just spectators.
   
   NARRATIVE EVIDENCE:
   "Mining Ergo is intended to be accessible for ordinary people... promoting 
   fair distribution".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_autolykos_asic_resistance,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        constraint_beneficiary(ergo_autolykos_asic_resistance, decentralization),
        constraint_victim(ergo_autolykos_asic_resistance, none),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(ergo_autolykos_asic_resistance, E),
    E < 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Protocol - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - The hardcoded logic of the Sigmastate.
   WHEN: civilizational.
   WHERE: analytical.
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, Autolykos is the "Mountain." It is the 
   immutable proof-of-work reality that secures the state transition. 
   One cannot argue with the math; one must simply solve the hash.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_autolykos_asic_resistance,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        constraint_beneficiary(ergo_autolykos_asic_resistance, network_integrity),
        constraint_victim(ergo_autolykos_asic_resistance, none),
        spatial_scope(global)
    )
) :-
    true,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(ergo_autolykos_tests).

test(asic_exclusion_logic) :-
    % Test that high-power institutional agents see a Noose (Suppression of efficiency)
    constraint_indexing:constraint_classification(ergo_autolykos_asic_resistance, noose, 
        context(institutional, _, _, _, _, _)).

test(fair_entry_rope) :-
    % Test that moderate-power GPU miners see a Rope (Opportunity)
    constraint_indexing:constraint_classification(ergo_autolykos_asic_resistance, rope, 
        context(individual_moderate, _, _, _, _, _)).

:- end_tests(ergo_autolykos_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION (0.85): This is the core "intent" of Autolykos. By making 
 * the memory requirements high and variable, it suppresses the 
 * viability of static, specialized circuit design.
 * 2. CLASSIFICATION: The "Noose" for ASIC makers is the primary source 
 * of the project's decentralization "Rope" for users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    asic_breakthrough_event,
    "Will a specialized manufacturer develop a 'programmable' memory-dense ASIC that can bypass Autolykos v2?",
    resolution_mechanism("Monitor network hashrate for sudden 10x spikes inconsistent with GPU market trends"),
    impact("If Yes: The Rope snaps, and Ergo becomes a Mountain for industrial giants only. If No: Decentralization holds."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: SHA-256 (Bitcoin Style)
 * Viability: Technically simpler, but invites ASIC centralization.
 * Suppression: Rejection of SHA-256 is the defining technological 
 * constraint that creates the Ergo ecosystem.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [ergo_autolykos_asic_resistance].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
