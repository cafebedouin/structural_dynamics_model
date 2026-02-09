% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent_mechanism
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Ergo Platform Documentation (ErgoDocs) and Whitepapers
% ============================================================================

:- module(constraint_ergo_storage_rent_mechanism, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: ergo_storage_rent_mechanism
 * human_readable: Ergo Storage Rent (Demurrage)
 * domain: economic/technological
 * temporal_scope: Post-2019 (Operational Mainnet)
 * spatial_scope: Global (Distributed Network)
 * 
 * SUMMARY:
 * Ergo introduces a "Storage Rent" or demurrage fee for inactive data stored 
 * on the blockchain. Any UTXO (box) that remains unspent for 4 years is 
 * subject to a recurring fee. This prevents "blockchain bloat" 
 * and ensures miners have a long-term revenue stream after the emission 
 * of new coins ends.
 * 
 * KEY AGENTS:
 * - Long-term Holders (Individual Powerless): Subject to the fee if inactive.
 * - Miners (Institutional): The beneficiaries who collect the rent.
 * - Protocol Developers (Analytical): Architects of the eUTXO model.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(ergo_storage_rent_mechanism, 0, 10).
narrative_ontology:constraint_claim(ergo_storage_rent_mechanism, tangled_rope).

% Base extractiveness: 0.15.
% Small fee (approx 0.13 ERG every 4 years) prevents total loss 
% but extracts value from inactive addresses.
domain_priors:base_extractiveness(ergo_storage_rent_mechanism, 0.15).

% Suppression score: 0.8.
% The rule is hardcoded in the sigmastate-interpreter; the only 
% escape is to spend/move the coins every 4 years.
domain_priors:suppression_score(ergo_storage_rent_mechanism, 0.8).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, extractiveness, 0.15).
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, suppression_requirement, 0.8).

% Enforcement: The protocol enforces itself through consensus rules.
domain_priors:requires_active_enforcement(ergo_storage_rent_mechanism).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(ergo_storage_rent_mechanism, ergo_miners).
narrative_ontology:constraint_victim(ergo_storage_rent_mechanism, inactive_wallet_addresses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE "HODLER" (PASSIVE INVESTOR) - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Users subject to protocol-level consensus)
   WHEN: biographical (Holding for decades; 4 years is a short horizon)
   WHERE: trapped (Cannot opt-out of rent if the box is on-chain)
   
   WHY THIS CLASSIFICATION:
   To an investor who treats crypto like "digital gold" (buy and forget), 
   storage rent is a 'Snare'. It forces them to maintain "active awareness" 
   or suffer the gradual extraction of their assets, strangling a purely
   passive investment strategy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent_mechanism,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NETWORK MINER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Provides security for the network)
   WHEN: historical (Building a self-sustaining network post-emission)
   WHERE: arbitrage (Can move hash-rate between PoW chains)
   
   WHY THIS CLASSIFICATION:
   For miners, the rent is a 'Rope'. It is a functional tool that guarantees 
   security budgets in a world where transaction fees alone might be 
   insufficient after the 2045 emission end, ensuring long-term network security.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent_mechanism,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PROTOCOL ARCHITECT / FOUNDATION - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Designers of the state management rules)
   WHEN: civilizational (Solving for "state bloat" over centuries)
   WHERE: analytical (Observing the long-term health of the UTXO set)
   
   WHY THIS CLASSIFICATION:
   From a design perspective, storage rent is a 'Mountain'—a fundamental 
   economic law required for a UTXO system to survive indefinitely without 
   infinite storage growth. It is an immutable necessity for the long-term
   viability of the protocol.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_storage_rent_mechanism,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ergo_storage_rent_mechanism_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(ergo_storage_rent_mechanism_tests).

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
 * 1. CLASSIFICATION RATIONALE:
 *    - HODLer (Snare): A passive investment strategy is penalized.
 *    - Miner (Rope): A tool for long-term network sustainability.
 *    - Protocol Architect (Mountain): An economic necessity for UTXO management.
 * 
 * 2. CORE INSIGHT: Ergo's storage rent is a 'Tangled Rope'. It's a 'Rope' for
 *    network security and a 'Mountain' of economic necessity for designers, but
 *    it creates a 'Snare' for long-term passive holders, highlighting the
 *    trade-off between network health and individual asset sovereignty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the long-term effectiveness of the storage rent mechanism.
 */

omega_variable(
    state_garbage_collection_adoption,
    "Will miners consistently 'garbage collect' tiny dust boxes if the fees collected are lower than the computational cost of doing so?",
    resolution_mechanism("Monitoring the first 4-year cycle (2023 onwards) to see box collection rates vs. network fees; economic modeling of miner incentives."),
    impact("If NO: The rent is a failed 'Rope' (ineffective). If YES: The 'Rope' successfully secures the chain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Permanent UTXOs (The Bitcoin Model)
 *    Viability: The industry standard, prioritizing untouched asset sovereignty.
 *    Suppression: Ergo explicitly rejects this to solve the "State Bloat" problem, 
 *    treating Bitcoin's permanent storage as an unsustainable "free lunch" that
 *    threatens long-term decentralization.
 *
 * CONCLUSION:
 * The rejection of permanent storage transforms "holding" from a passive 
 * right into a managed responsibility. This makes the storage rent mechanism
 * a core component of Ergo's economic philosophy, a 'Rope' for sustainability
 * that deliberately creates a 'Snare' for inactive accounts.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/ergo_storage_rent_mechanism].
 * 2. Multi-perspective: ?- multi_index_report(ergo_storage_rent_mechanism).
 * 3. Run tests: ?- run_tests(ergo_storage_rent_mechanism_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Mixed coordination/extraction — theater masks extraction component
domain_priors:theater_ratio(ergo_storage_rent_mechanism, 0.31).
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, theater_ratio, 0.31).
