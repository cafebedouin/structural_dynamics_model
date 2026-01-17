% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent_mechanism
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Ergo Platform Documentation (ErgoDocs) and Whitepapers
% ============================================================================

:- module(ergo_storage_rent, []).

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
 * * constraint_id: ergo_storage_rent
 * human_readable: Ergo Storage Rent (Demurrage)
 * domain: economic/technological
 * temporal_scope: Post-2019 (Operational Mainnet)
 * spatial_scope: Global (Distributed Network)
 * * SUMMARY:
 * Ergo introduces a "Storage Rent" or demurrage fee for inactive data stored 
 * on the blockchain. Any UTXO (box) that remains unspent for 4 years is 
 * subject to a recurring fee. This prevents "blockchain bloat" 
 * and ensures miners have a long-term revenue stream after the emission 
 * of new coins ends.
 * * KEY AGENTS:
 * - Long-term Holders (HODLers): Agents who store value without frequent 
 * movement; they must periodically "refresh" their boxes.
 * - Miners: Beneficiaries who collect the rent as part of block rewards to 
 * sustain network security.
 * - Protocol Developers: Architects of the eUTXO model aiming for "predictable 
 * scalability".
 * * NARRATIVE ARC:
 * Unlike Bitcoin, where "lost coins" are permanently removed from circulation, 
 * Ergo treats the blockchain as a physical storage space. If 
 * a box is abandoned, miners eventually "garbage collect" the value, returning 
 * it to the economic loop.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(ergo_operational_era, 0, 10).
narrative_ontology:constraint_claim(ergo_storage_rent, rope).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Small fee (approx 0.13 ERG every 4 years) prevents total loss 
% but extracts value from inactive addresses.
domain_priors:base_extractiveness(ergo_storage_rent, 0.15).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: The rule is hardcoded in the sigmastate-interpreter; the only 
% escape is to spend/move the coins every 4 years.
domain_priors:suppression_score(ergo_storage_rent, 0.8).

% Enforcement requirements
domain_priors:requires_active_enforcement(ergo_storage_rent).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(ergo_storage_rent, extractiveness, 0.15).
narrative_ontology:constraint_metric(ergo_storage_rent, suppression_requirement, 0.8).

% Beneficiaries and Victims
constraint_beneficiary(ergo_storage_rent, ergo_miners).
constraint_victim(ergo_storage_rent, inactive_wallet_addresses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: The "HODLer" (Passive Investor) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Users subject to protocol-level consensus.
   WHEN: biographical - Holding for decades; 4 years is a short horizon.
   WHERE: trapped - Cannot opt-out of rent if the box is on-chain.
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   To an investor who treats crypto like "digital gold" (buy and forget), 
   storage rent is a "Noose." It forces them to maintain "active awareness" 
   or suffer the gradual extraction of their assets.
   
   NARRATIVE EVIDENCE:
   "Lost coins... would be slowly returned to the ecosystem... making it 
   less viable as a 'store and forget' asset without maintenance".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        constraint_beneficiary(ergo_storage_rent, ergo_miners),
        constraint_victim(ergo_storage_rent, hodlers),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(ergo_storage_rent, E),
    E > 0.1,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Network Miner - Rope
   --------------------------------------------------------------------------
   
   WHO: collective_organized / institutional - Miners vote on parameters.
   WHEN: historical - Building a self-sustaining network post-emission.
   WHERE: arbitrage - Can move hash-rate between PoW chains.
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   For miners, the rent is a "Rope." It is a functional tool that guarantees 
   security budgets in a world where transaction fees alone might be 
   insufficient after the 2045 emission end.
   
   NARRATIVE EVIDENCE:
   "Storage Rent... as ERGO ages and storage rent comes into play miners 
   will see rewards coming in more than one way".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent,
    rope,
    context(
        agent_power(collective_organized),
        time_horizon(historical),
        exit_options(arbitrage),
        constraint_beneficiary(ergo_storage_rent, network_security),
        constraint_victim(ergo_storage_rent, none),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(ergo_storage_rent, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Protocol Architect / Foundation - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Designers of the state management rules.
   WHEN: civilizational - Solving for "state bloat" over centuries.
   WHERE: analytical.
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   From a design perspective, storage rent is a "Mountain"—a fundamental 
   economic law required for a UTXO system to survive indefinitely without 
   infinite storage growth.
   
   NARRATIVE EVIDENCE:
   "Ergo is designed with controlled state size growth... making the 
   blockchain predictably scalable".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        constraint_beneficiary(ergo_storage_rent, network_longevity),
        constraint_victim(ergo_storage_rent, none),
        spatial_scope(global)
    )
) :-
    true,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ergo_storage_rent_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ergo_storage_rent, Type1, 
        context(individual_powerless, biographical, trapped, _, _, global)),
    constraint_indexing:constraint_classification(ergo_storage_rent, Type2, 
        context(collective_organized, historical, arbitrage, _, _, global)),
    Type1 = noose,
    Type2 = rope.

test(time_immutability_shift) :-
    % Long-term historical views should see the functionality (Rope/Mountain)
    constraint_indexing:constraint_classification(ergo_storage_rent, rope, 
        context(_, historical, _, _, _, _)).

:- end_tests(ergo_storage_rent_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.15): While low per-box, it is mandatory. I chose 0.15 
 * because it is not "severe" but is structurally extractive.
 * 2. CLASSIFICATION: The most interesting tension in Ergo is the HODLer 
 * (who sees a Noose) vs. the Miner (who sees a Rope). This highlights 
 * the "social contract" difference from Bitcoin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    state_garbage_collection_adoption,
    "Will miners actually spend the effort to 'garbage collect' tiny dust boxes if fees are too low?",
    resolution_mechanism("Monitor the first 4-year cycle (2023 onwards) to see box collection rates"),
    impact("If NO: the rent is a failed Rope (ineffective). If YES: the Rope secures the chain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Permanent UTXOs (The Bitcoin Model)
 * Viability: The industry standard.
 * Suppression: Ergo explicitly rejects this to solve the "State Bloat" problem, 
 * treating Bitcoin's permanent storage as an unsustainable "free lunch".
 * * CONCLUSION:
 * The rejection of permanent storage transforms "holding" from a passive 
 * right into a managed responsibility.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [ergo_storage_rent].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
