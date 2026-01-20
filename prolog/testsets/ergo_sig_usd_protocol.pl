% ============================================================================
% CONSTRAINT STORY: sig_usd_protocol
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: AgeUSD Protocol / Ergo Blockchain Stablecoin System
% ============================================================================

:- module(constraint_sig_usd_protocol, []).

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
 * * constraint_id: sig_usd_protocol
 * human_readable: SigUSD Stability Mechanism
 * domain: economic/technological
 * temporal_scope: 2021-Present (Post-AgeUSD launch)
 * spatial_scope: Ergo Blockchain (Global)
 * * SUMMARY:
 * SigUSD is an algorithmic, crypto-backed stablecoin based on the AgeUSD protocol. 
 * It operates via a dual-token system: SigUSD (stablecoin) and SigRSV (reserve token). 
 * Stability is maintained by SigRSV holders who act as the "bank," absorbing ERG 
 * price volatility in exchange for protocol fees, while SigUSD holders maintain 
 * value peg as long as the reserve ratio stays within 400-800%.
 * * KEY AGENTS:
 * - SigUSD_Holder: Seeks price stability and a "safe haven" within the ecosystem.
 * - SigRSV_Holder: Seeks profit from volatility and fees; acts as the liquidity provider/underwriter.
 * - Arbitrageur: Technical agent exploiting price gaps between the protocol and external DEXs.
 * * NARRATIVE ARC:
 * The protocol functions as a decentralized bank. When ERG price rises, SigRSV 
 * value grows exponentially. When ERG price crashes, SigRSV holders are "diluted" 
 * to ensure SigUSD can always be redeemed for $1 worth of ERG, until a critical 
 * collateral floor is hit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for system extraction
narrative_ontology:interval(sig_usd_interval, 0, 10).
narrative_ontology:constraint_claim(sig_usd_protocol, rope).

% Base extractiveness: Moderate (0.4)
% Rationale: The protocol charges fees (minting/burning) that flow from users 
% to reserve holders. While asymmetric, it is a service-for-risk trade.
domain_priors:base_extractiveness(sig_usd_protocol, 0.4).

% Suppression score: Low (0.2)
% Rationale: Alternatives like wrapped USDT or other algorithmic coins exist 
% on Ergo; the protocol does not actively suppress them, though network effects favor SigUSD.
domain_priors:suppression_score(sig_usd_protocol, 0.2).

% Enforcement: Emerges naturally via Smart Contracts
domain_priors:emerges_naturally(sig_usd_protocol).

% BENEFICIARIES & VICTIMS
% Primary beneficiaries are the reserve holders during bull markets.
constraint_beneficiary(sig_usd_protocol, sig_rsv_holders).
% Primary "victims" (risk-bearers) are reserve holders during bear markets, 
% and stablecoin holders if the peg breaks.
constraint_victim(sig_usd_protocol, sig_rsv_holders_at_low_reserve).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: SigUSD_Holder (Retail User) - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: individual_powerless (User of the interface, not the dev or whale)
   WHEN: immediate (Needs the peg to hold TODAY)
   WHERE: trapped (Asset is locked in the contract logic)
   SCOPE: local (Their specific wallet/liquidity)
   
   WHY THIS CLASSIFICATION:
   For a retail user, the protocol's math is "natural law." They cannot change 
   the 400% reserve requirement or the fee structure. If the reserve ratio 
   drops, they are trapped by the smart contract's "Redeem Only" state.
   
   NARRATIVE EVIDENCE:
   "I just want to hold dollars. I don't control the code; the code tells me 
   if I can mint or not."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sig_usd_protocol,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(sig_usd_protocol, S),
    S < 0.5, % Even though suppression is low, powerless agents see it as fixed.
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SigRSV_Holder (The "Banker") - ROPE
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has capital, chooses to provide liquidity)
   WHEN: biographical (Long-term investment horizon)
   WHERE: mobile (Can "burn" SigRSV for ERG and leave when ratio allows)
   SCOPE: regional (Impacts the Ergo ecosystem liquidity)
   
   WHY THIS CLASSIFICATION:
   The SigRSV holder sees the protocol as a tool (Rope) for leverage. It is a 
   functional coordination mechanism that allows them to earn fees. They 
   willingly enter and can exit if they play the "game" correctly.
   
   NARRATIVE EVIDENCE:
   "I provide the reserve to earn the minting fees from the SigUSD users. It's 
   a fair trade for the risk I take."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sig_usd_protocol,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(sig_usd_protocol, E),
    E < 0.6, % Beneficial coordination.
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Protocol Auditor/Whale - NOOSE
   --------------------------------------------------------------------------
   WHO: institutional (Can move markets or influence protocol evolution)
   WHEN: historical (Looking at systemic collapse risks over years)
   WHERE: arbitrage (Can exit through secondary markets or hedge elsewhere)
   SCOPE: global (Financial stability perspective)
   
   WHY THIS CLASSIFICATION:
   From a systemic view, if the reserve ratio stays near 400% during a 
   prolonged ERG crash, the protocol becomes a "Noose." It forces 
   asymmetric losses on the last reserve holders to stay liquid, potentially 
   leading to a "death spiral" where the constraint becomes coercive to 
   those remaining.
   
   NARRATIVE EVIDENCE:
   "The 400% limit protects the stablecoin but strangulates the reserve 
   holders during a crash, preventing them from exiting until the peg is safe."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sig_usd_protocol,
    noose,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(sig_usd_protocol, E),
    E > 0.3, 
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(sig_usd_protocol_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(sig_usd_protocol, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(sig_usd_protocol, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(sig_usd_protocol, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    % Institutional agents can arbitrage (lower experienced extraction)
    % whereas powerless agents pay the flat protocol fee.
    ScorePowerless = 0.4,
    ScorePowerful = 0.2, 
    ScorePowerless > ScorePowerful.

test(time_immutability) :-
    % At 'immediate' horizon, it is a Mountain (cannot change code).
    constraint_indexing:constraint_classification(sig_usd_protocol, mountain, context(_, immediate, _, _)).

:- end_tests(sig_usd_protocol_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): Chosen because while the protocol is "fair," the 2% fee 
 * is an asymmetric transfer from those needing stability to those providing it.
 * 2. PERSPECTIVES: I chose the Holder/Banker/Institutional split because crypto 
 * protocols are defined by the "Reserve Ratio" state, which changes the 
 * user's freedom of movement.
 * 3. NOOSE LOGIC: I classified the institutional view as Noose because of the 
 * "Liquidation Risk"—the protocol's constraints (locking minting/burning) 
 * can become a trap during black swan events.
 * * AMBIGUITIES:
 * - Oracle Risk: The protocol relies on an external price feed. If the oracle 
 * fails, the constraint shifts from "Mountain" to "Chaos." I modeled this 
 * via the Omega below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    oracle_integrity,
    "Is the ERG/USD oracle price feed accurate and manipulation-resistant?",
    resolution_mechanism("Post-hoc analysis of oracle data vs. global market average during high volatility"),
    impact("If oracle fails, the Mountain (math) becomes a Noose (error-driven extraction)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Fiat-backed Stablecoins (USDT/USDC)
 * Viability: Highly liquid, but introduces central party risk (the anti-thesis of Ergo).
 * Suppression: Not suppressed, but culturally rejected by the community.
 * * ALTERNATIVE 2: DexyGold (Non-custodial algorithmic gold peg)
 * Viability: Operates on similar principles but different collateral.
 * * CONCLUSION:
 * The presence of alternatives makes SigUSD a "Rope" for those who value 
 * decentralization. For those who *must* use Ergo and need stability, it 
 * approaches "Mountain" status due to the lack of other high-liquidity options.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ergo_sig_usd_protocol, noose, agent_power(individual_powerless)).
