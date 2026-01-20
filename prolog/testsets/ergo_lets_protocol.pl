% ============================================================================
% CONSTRAINT STORY: ergo_lets_protocol
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Ergo Platform - Mutual Credit / LETS Design Patterns
% ============================================================================

:- module(constraint_ergo_lets, []).

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
 * * constraint_id: ergo_lets_protocol
 * human_readable: Ergo Local Exchange Trading System (LETS)
 * domain: economic/social
 * temporal_scope: 2020-Present
 * spatial_scope: Ergo Blockchain (Community-specific)
 * * SUMMARY:
 * LETS on Ergo is a trustless mutual credit system where the sum of all 
 * participant balances is always zero. It allows communities 
 * to trade goods and services using a local currency created through 
 * "IOUs" backed by collateral or reputation, enforced via smart contracts 
 *.
 * * KEY AGENTS:
 * - Community_Member: Uses the system to facilitate local trade without 
 * needing external fiat or volatile crypto.
 * - New_Entrant: A user seeking to join who must meet the protocol's 
 * entry constraints (collateral or social vouching).
 * - Protocol_Auditor: Observes the mathematical integrity of the zero-sum 
 * invariant.
 * * NARRATIVE ARC:
 * LETS transforms currency from a "commodity" to be extracted into a 
 * "public utility" (Rope) for coordination. However, to prevent "leaking" 
 * value (defaults), the protocol imposes rigid entry barriers (Mountain) 
 * that can feel like a "Noose" to those lacking the required initial 
 * capital or social standing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(ergo_lets_interval, 0, 10).
narrative_ontology:constraint_claim(ergo_lets_protocol, rope).

% Base extractiveness: Low (0.15)
% Rationale: The protocol is designed to be non-extractive; fees are 
% typically minimal and used only to prevent spam.
domain_priors:base_extractiveness(ergo_lets_protocol, 0.15).

% Suppression score: Low (0.1)
% Rationale: It is a voluntary, opt-in coordination layer. It does not 
% suppress other economic models.
domain_priors:suppression_score(ergo_lets_protocol, 0.1).

% Enforcement: Emerges naturally from Sigma Protocol script logic.
domain_priors:emerges_naturally(ergo_lets_protocol).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(ergo_lets_protocol, local_communities).
constraint_beneficiary(ergo_lets_protocol, unbanked_users).
% Victims are arguably "bad actors" whose collateral is seized upon default, 
% though this is a self-imposed risk.
constraint_victim(ergo_lets_protocol, defaulters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Active Community Member - ROPE
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: immediate
   WHERE: mobile
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the established user, LETS is a Rope. It is a tool they use to 
   transact when liquidity in ERG or fiat is low. They have the agency 
   to issue credit and settle debts.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_lets_protocol,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(ergo_lets_protocol, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Under-collateralized User - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: biographical
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For a user without the required ERG to post as collateral, the protocol's 
   entry requirement is a Mountain. It is an unchangeable law that 
   prevents them from accessing the "Rope" of credit.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_lets_protocol,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(ergo_lets_protocol, S),
    S < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: Protocol Architect/Institutional - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From an institutional or designer's view, the "Sum = 0" rule is a 
   Mountain. It is the core mathematical constraint that ensures systemic 
   solvency and cannot be relaxed without destroying the protocol.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_lets_protocol,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(ergo_lets_protocol, S),
    S < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: Over-leveraged Defaulter - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: constrained
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   When a user defaults, the collateral seizure mechanism becomes a Noose. 
   The constraint "strangles" their assets to repay the pool's debt, 
   acting coercively to maintain the zero-sum balance.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_lets_protocol,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(ergo_lets_protocol, E),
    E >= 0.1, % Even low extractiveness feels like a Noose during liquidation.
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(ergo_lets_protocol_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ergo_lets_protocol, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(ergo_lets_protocol, Type2, context(agent_power(individual_powerless), _, trapped, _)),
    Type1 \= Type2.

test(zero_sum_immutability) :-
    % Analytical perspective correctly identifies the math as Mountain.
    constraint_indexing:constraint_classification(ergo_lets_protocol, mountain, context(institutional, historical, _, _)).

:- end_tests(ergo_lets_protocol_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE ON POWERLESS: I chose to classify the entry barrier as 
 * "Mountain" rather than "Noose" because it is a passive structural 
 * requirement, not an active extraction from those already inside.
 * 2. EXTRACTIVENESS (0.15): Extremely low, reflecting Ergo's ethos of 
 * community-driven, non-rent-seeking financial tools.
 * 3. NOOSE LOGIC: Liquidation is the only point where this "Rope" becomes 
 * "coercive," hence the Noose classification for the defaulter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    reputation_portability,
    "Can LETS reputation be ported between communities without collateral?",
    resolution_mechanism("Development of cross-LETS reputation standards"),
    impact("If yes, the 'Mountain' of collateral drops, making it a universal Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Traditional Credit Unions
 * Viability: High social trust, but geographically and legally bound.
 * * ALTERNATIVE 2: SigUSD (Stablecoins)
 * Viability: Better for external trade, but requires 400% collateral.
 * * CONCLUSION:
 * LETS is a "Rope" specifically because it allows for negative balances 
 * (credit) which SigUSD does not, making it more flexible for local trade 
 * despite the "Mountain" of setup complexity.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
