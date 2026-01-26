% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Ergo Platform Whitepapers
% ============================================================================

:- module(constraint_ergo_storage_rent, []).

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
 * constraint_id: ergo_storage_rent
 * human_readable: Ergo Storage Rent (UTXO Demurrage)
 * domain: technological/economic
 * temporal_scope: 2019 - Present
 * spatial_scope: Global (Ergo Blockchain)
 * 
 * SUMMARY:
 * Storage rent is a "demurrage" fee charged to UTXOs (unspent coins) that have 
 * not moved in four years. This mechanism prevents blockchain bloat from "dust"
 * and recycles lost or abandoned coins back to the miners, ensuring long-term
 * network security.
 * 
 * KEY AGENTS:
 * - The Long-Term Holder (Individual Powerless): Subject to the fee if inactive; may lose funds if keys are lost.
 * - The Miner (Institutional): The beneficiary who "collects" the rent, securing the network.
 * - The Ergo Foundation (Analytical): The designers of the economic model.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(ergo_storage_rent, 0, 10).
narrative_ontology:constraint_claim(ergo_storage_rent, tangled_rope).

% Base extractiveness: Moderate (0.4)
% Rationale: It reclaims value from users who are inactive. While this has a
% collective benefit, it is a direct extraction from the individual holder.
domain_priors:base_extractiveness(ergo_storage_rent, 0.4).

% Suppression: Low (0.2)
% Rationale: The rule is transparent and predictable. The alternative (no rent)
% is the default on other chains like Bitcoin.
domain_priors:suppression_score(ergo_storage_rent, 0.2).

% Enforcement: Enforced automatically by the protocol's consensus rules.
domain_priors:emerges_naturally(ergo_storage_rent).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(ergo_storage_rent, ergo_miners).
constraint_victim(ergo_storage_rent, inactive_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INACTIVE HOLDER - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Lost keys, forgotten funds)
   WHEN: historical (The 4-year cycle)
   WHERE: trapped (Cannot move funds they cannot access)
   
   WHY THIS CLASSIFICATION:
   For an uninformed user or someone who has lost their keys, the rent is a 'Snare'.
   It slowly drains their assets until they are gone, an unstoppable process
   they are powerless to prevent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent,
    snare,
    context(agent_power(individual_powerless), time_horizon(historical), exit_options(trapped), spatial_scope(local))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MINER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Provides security for the network)
   WHEN: generational (Securing the chain after block rewards diminish)
   WHERE: arbitrage (Benefits from the collection of rent)
   
   WHY THIS CLASSIFICATION:
   For miners, storage rent is a 'Rope'. It is a vital coordination mechanism for
   long-term network sustainability. It guarantees a source of revenue after
   block rewards cease, ensuring the chain remains secure and doesn't die.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent,
    rope,
    context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(global))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PROTOCOL DESIGNER - Tangled Rope
   --------------------------------------------------------------------------
   WHO: analytical (Designing the economic model)
   WHEN: civilizational (Planning for the entire lifespan of the blockchain)
   WHERE: analytical (Balancing competing interests)
   
   WHY THIS CLASSIFICATION:
   The designer sees it as a 'Tangled Rope'. It's a 'Rope' because it solves the
   critical problems of blockchain bloat and long-term security. It's 'Tangled'
   because it does so by introducing an extractive 'Snare' for inactive users,
   creating a complex trade-off between individual property rights and collective health.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent,
    tangled_rope,
    context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(ergo_storage_rent_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ergo_storage_rent, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(ergo_storage_rent_tests).

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
 * 1. CLASSIFICATION: The core tension is between the individual holder's
 *    property rights (Snare) and the collective's need for a sustainable
 *    security model (Rope). The 'Tangled Rope' classification for the designer
 *    best captures this trade-off.
 * 
 * 2. EXTRACTIVENESS (0.4): This is a direct extraction of value from a user's
 *    holdings. While it serves a network purpose, the individual loss is real,
 *    justifying a moderate score.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether the social consensus around the rent parameters will hold.
 */

omega_variable(
    rent_adjustment_social_consensus,
    "Will the 4-year rent period be shortened or lengthened via social consensus and a subsequent soft-fork?",
    resolution_mechanism("Monitoring miner voting, community sentiment, and developer proposals on Ergo's governance channels."),
    impact("A shorter period increases the 'Snare' pressure on holders and may be seen as more extractive, while a longer period may weaken the long-term security argument."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: No Storage Rent (The Bitcoin Model)
 *    Viability: This is the default in most UTXO blockchains.
 *    Suppression: Rejected by Ergo's designers to solve the perceived long-term
 *    problems of "blockchain bloat" from dust UTXOs and the eventual decline
 *    of the block reward for miners.
 *
 * CONCLUSION:
 * Storage rent is an intentional design choice that prioritizes the long-term
 * health and security of the network over absolute, untouched individual property
 * rights for inactive users. It is a pragmatic trade-off that defines Ergo's
 * economic policy.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/ergo_storage_rent].
 * 2. Multi-perspective: ?- multi_index_report(ergo_storage_rent).
 * 3. Run tests: ?- run_tests(ergo_storage_rent_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
