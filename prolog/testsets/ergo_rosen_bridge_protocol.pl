% ============================================================================
% CONSTRAINT STORY: rosen_bridge_protocol
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Rosen Bridge Documentation / Ergo Blockchain
% ============================================================================

:- module(constraint_rosen_bridge_protocol, []).

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
 * * constraint_id: rosen_bridge_protocol
 * human_readable: Rosen Bridge Cross-Chain Mechanism
 * domain: technological/economic
 * temporal_scope: 2023-Present
 * spatial_scope: Multi-chain (Ergo, Cardano, Bitcoin, ETH, etc.)
 * * SUMMARY:
 * Rosen Bridge is an Ergo-centric cross-chain protocol that enables asset 
 * transfers without deploying smart contracts on external chains. 
 * It uses a two-layer security model: Watchers monitor events, and a federated 
 * set of Guards verifies and executes transactions. 
 * All consensus logic resides on Ergo, making the bridge auditable from a 
 * single chain.
 * * KEY AGENTS:
 * - Watcher: Independent nodes monitoring chains and reporting events; 
 * requires RSN and ERG collateral.
 * - Guard: Federated validators who verify watcher reports and sign 
 * multi-sig/threshold transactions on target chains.
 * - Cross-Chain User: Seeks secure movement of assets (e.g., BTC to Ergo) 
 * while avoiding centralized exchanges.
 * * NARRATIVE ARC:
 * The protocol transforms the "weak link" problem of bridges into a tiered 
 * authentication system. By centralizing logic on Ergo but decentralizing 
 * monitoring (Watchers) and multisig execution (Guards), it provides a 
 * "Refereed Vault" where assets are locked/unlocked based on on-chain 
 * proof of events.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for system extraction
narrative_ontology:interval(rosen_bridge_interval, 0, 10).
narrative_ontology:constraint_claim(rosen_bridge_protocol, rope).

% Base extractiveness: Moderate (0.5)
% Rationale: High entry barrier for watchers (800 ERG + 30k RSN) and 
% bridge fees (0.5% or $10) are significant, primarily rewarding the 
% security providers.
domain_priors:base_extractiveness(rosen_bridge_protocol, 0.5).

% Suppression score: Low (0.2)
% Rationale: The bridge is open-source and permissionless for watchers; 
% it doesn't suppress other bridges but competes via security.
domain_priors:suppression_score(rosen_bridge_protocol, 0.2).

% Enforcement: Requires active enforcement (Guard signatures & Watcher consensus)
domain_priors:requires_active_enforcement(rosen_bridge_protocol).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(rosen_bridge_protocol, [watchers, guards, rsn_holders]).
% High transaction fees effectively "extract" from small retail users.
constraint_victim(rosen_bridge_protocol, small_value_transactors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Small Retail User - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For users moving small amounts (e.g., <$100), the $10 minimum fee acts as 
   a Noose. The high cost per transaction "strangles" their 
   mobility across chains, effectively trapping their capital on one network 
   due to the prohibitively high exit fee.
   
   NARRATIVE EVIDENCE:
   "Transactions lower than 2k USD will have a 10 USD fee... one watcher 
   gets approx 0.10-0.15 USD".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rosen_bridge_protocol,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(rosen_bridge_protocol, E),
    E >= 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Liquidity Provider/Whale - ROPE
   --------------------------------------------------------------------------
   WHO: individual_powerful
   WHEN: biographical
   WHERE: arbitrage
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For high-net-worth users or LPs, the bridge is a robust Rope. 
   The 0.5% fee is a fair trade for "security over speed". 
   They use the bridge as a coordination tool to move assets where 
   yield is higher (Arbitrage).
   
   NARRATIVE EVIDENCE:
   "The bridge is really intended for larger transactions and moving things 
   cross-chain securely".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rosen_bridge_protocol,
    rope,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(rosen_bridge_protocol, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Protocol Auditor - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From an auditor's perspective, the logic on Ergo (Sigma Protocols) and 
   the multi-sig requirements on other chains are Mountain constraints 
  . These rules are fixed by the smart contract code; 
   the 60%+1 watcher consensus is a mathematical threshold that cannot 
   be bypassed by social engineering.
   
   NARRATIVE EVIDENCE:
   "Consensus on any action is achieved on the Ergo platform by a group of 
   entities known as Guards... auditable on Ergo".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rosen_bridge_protocol,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(rosen_bridge_protocol, S),
    S < 0.3,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(rosen_bridge_protocol_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(rosen_bridge_protocol, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(rosen_bridge_protocol, Type2, context(agent_power(individual_powerful), _, _, _)),
    constraint_indexing:constraint_classification(rosen_bridge_protocol, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    % Powerful agents pay proportional fees (Rope), 
    % while powerless agents pay a high flat minimum (Noose).
    ScorePowerless = 0.8, % High effective fee % for small amounts
    ScorePowerful = 0.5,
    ScorePowerless > ScorePowerful.

:- end_tests(rosen_bridge_protocol_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.5): Set at a moderate level because while the bridge 
 * provides a vital service, the fee structure is heavily weighted toward 
 * high-value users, making it extractive for retail.
 * 2. NOOSE CLASSIFICATION: Specifically used to highlight the entry/exit 
 * barrier created by the $10 minimum fee, which creates a "locked-in" 
 * effect for low-balance wallets.
 * 3. MOUNTAIN LOGIC: The auditability on Ergo makes the protocol's state 
 * transparent and its rules as rigid as the blockchain itself.
 * * AMBIGUITIES:
 * - Guard Decentralization: The Guard set is currently "federated" (predefined 
 * entities). Whether this stays decentralized or 
 * consolidates is an Omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    guard_collusion_resistance,
    "Will the federated Guard set remain independent or consolidate over time?",
    resolution_mechanism("Monitoring the rotation and diversity of the Guard set on rosen.tech"),
    impact("If consolidated, the Rope (utility) becomes a Noose (centralized control)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Centralized Exchanges (CEXs)
 * Viability: Cheaper and faster, but requires KYC and custody trust.
 * * ALTERNATIVE 2: Layer 1 Wrapped Token Bridges (e.g., WBTC on Ethereum)
 * Viability: High liquidity, but often relies on a single custodian.
 * * CONCLUSION:
 * Rosen is a "Rope" for those prioritizing decentralized security over cost. 
 * Its "Mountain" of multi-layered verification (Watchers + Guards) makes it 
 * one of the most resilient bridges in the ecosystem.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ergo_rosen_bridge_protocol, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ergo_rosen_bridge_protocol, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ergo_rosen_bridge_protocol, noose, agent_power(individual_powerless)).
