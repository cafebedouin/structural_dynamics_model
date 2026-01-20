% ============================================================================
% CONSTRAINT STORY: ergo_mixer_protocol
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: ErgoMixer (ZeroJoin) / Ergo Privacy Infrastructure
% ============================================================================

:- module(constraint_ergo_mixer_protocol, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: ergo_mixer_protocol
 * human_readable: ErgoMixer Privacy Mechanism
 * domain: social/technological
 * temporal_scope: 2020-Present
 * spatial_scope: Ergo Blockchain (Global)
 * * SUMMARY:
 * ErgoMixer is the first non-interactive, non-custodial mixer in the crypto 
 * industry. It utilizes the ZeroJoin protocol, which relies 
 * on Sigma protocols (Zero-Knowledge Proofs) and ring signatures to break 
 * on-chain links between deposit and withdrawal addresses. 
 * Unlike interactive mixers like CoinJoin, users do not need to be online 
 * simultaneously to mix their funds.
 * * KEY AGENTS:
 * - Privacy_User: Individual seeking to restore fungibility to their assets 
 * to avoid profiling.
 * - Observer/Chain_Analyst: External party attempting to trace fund 
 * provenance.
 * - Mix_Participant: Other users in the same pool who collectively create 
 * the anonymity set.
 * * NARRATIVE ARC:
 * Funds enter the "mosh pit" (a shared eUTXO pool). Every round, 
 * the coins are mixed with others', increasing the difficulty for an 
 * observer to guess the origin—mathematically halving the probability 
 * with each round. The process continues until the user 
 * decides to exit to a fresh address.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(ergo_mixer_interval, 0, 10).
narrative_ontology:constraint_claim(ergo_mixer_protocol, rope).

% Base extractiveness: Low (0.1)
% Rationale: The protocol is non-custodial and serverless; there is no 
% central entity extracting value beyond minimal network fees.
domain_priors:base_extractiveness(ergo_mixer_protocol, 0.1).

% Suppression score: Low (0.1)
% Rationale: It is an opt-in tool. While it suppresses the ability of 
% analysts to trace funds, it does not hide its own existence or prevent 
% users from using transparent transactions.
domain_priors:suppression_score(ergo_mixer_protocol, 0.1).

% Enforcement: Emerges naturally from cryptographic primitives (Sigma Protocols).
domain_priors:emerges_naturally(ergo_mixer_protocol).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(ergo_mixer_protocol, privacy_seeking_users).
% Victims are chain analysis firms whose data-extraction business model 
% is degraded by the obfuscation.
constraint_victim(ergo_mixer_protocol, chain_analysts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Privacy User - ROPE
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: immediate
   WHERE: mobile
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the user, the mixer is a liberating tool (Rope) that they use to 
   regain control over their financial history. They choose 
   when to enter, how many rounds to mix, and when to exit.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_mixer_protocol,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(ergo_mixer_protocol, E),
    E < 0.3, 
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Protocol Auditor - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From an auditor's view, the Sigma Protocols and the math of ZeroJoin 
   are "Mountain" constraints. These are immutable cryptographic laws 
   that define what is and isn't possible within the system. 
   You cannot "negotiate" with a discrete log problem.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_mixer_protocol,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(ergo_mixer_protocol, S),
    S < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: User in a Small Pool - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: biographical
   WHERE: trapped
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   In a scenario with very few participants (low anonymity set), the mixer 
   becomes a "Noose". The user is "trapped" because exiting 
   would be statistically traceable, yet staying doesn't provide the 
   desired privacy—the constraint "strangles" the anonymity it promised.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergo_mixer_protocol,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(ergo_mixer_protocol, E),
    E >= 0.1, 
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(ergo_mixer_protocol_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ergo_mixer_protocol, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(ergo_mixer_protocol, Type2, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(ergo_mixer_protocol, Type3, context(agent_power(individual_powerless), _, trapped, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(time_immutability) :-
    % Historical/Analytical perspective correctly identifies the math as Mountain.
    constraint_indexing:constraint_classification(ergo_mixer_protocol, mountain, context(analytical, historical, _, _)).

:- end_tests(ergo_mixer_protocol_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.1): The lowest score possible for a functioning system, 
 * as the ErgoMixer is purely peer-to-peer and non-custodial.
 * 2. NOOSE CLASSIFICATION: I used this to model the "Anonymity Set" failure 
 * state—a critical insight where a lack of participants turns a privacy 
 * tool into a visibility trap.
 * 3. SUPPRESSION (0.1): Low because the mixer doesn't stop analysts from 
 * looking; it simply makes their look-ups useless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    anonymity_set_density,
    "Is the current number of active participants enough to ensure k-anonymity?",
    resolution_mechanism("Statistical analysis of pool activity over time"),
    impact("If low: Protocol becomes a Noose (trap). If high: Protocol is a Rope (tool)."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Tornado Cash (Ethereum)
 * Viability: Historically high liquidity but interactive/custodial-adjacent 
 * risks and central entity vulnerabilities.
 * * ALTERNATIVE 2: ZCash (Native Shielded Transactions)
 * Viability: Built into the base layer but less flexible for custom tokens.
 * * CONCLUSION:
 * ErgoMixer's non-interactive nature makes it a "Rope" with far 
 * fewer logistical knots than predecessors, though it still relies on 
 * the "Mountain" of Sigma Protocols for its integrity.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraints/ergo_mixer_protocol].
 * 2. Multi-perspective: ?- multi_index_report(ergo_mixer_protocol).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
