% ============================================================================
% CONSTRAINT STORY: proof_of_work_consensus
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Bitcoin: A Peer-to-Peer Electronic Cash System (Satoshi Nakamoto)
% ============================================================================

:- module(constraint_proof_of_work_consensus, []).

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
 * constraint_id: proof_of_work_consensus
 * human_readable: Hash-Based Proof-of-Work
 * domain: technological/economic/social
 * temporal_scope: 2008 - Present (Digital Era)
 * spatial_scope: Global (Distributed Network)
 * 
 * SUMMARY:
 * A distributed consensus mechanism that replaces trusted third parties with 
 * computational proof. It timestamps transactions by hashing them into an 
 * ongoing chain of hash-based proof-of-work, forming a record that cannot 
 * be changed without redoing the work.
 * 
 * KEY AGENTS:
 * - Non-Mining User (Individual Powerless): Relies on the network's security without contributing hash power.
 * - The Network Protocol (Institutional): The rule-set that defines validity and incentive structures.
 * - The Honest Node (Individual Moderate): Contributes CPU power to extend the blockchain.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(proof_of_work_consensus, 0, 10).
narrative_ontology:constraint_claim(proof_of_work_consensus, rope).

% Base extractiveness: 0.2.
% The system does not "extract" value in a zero-sum sense; 
% instead, it consumes energy to produce the "security" of the ledger, 
% rewarding participants with new coins.
domain_priors:base_extractiveness(proof_of_work_consensus, 0.2).

% Suppression score: 0.9.
% The mechanism makes it "computationally impractical" for 
% an attacker to change the record once it is buried under subsequent 
% blocks.
domain_priors:suppression_score(proof_of_work_consensus, 0.9).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(proof_of_work_consensus, extractiveness, 0.2).
narrative_ontology:constraint_metric(proof_of_work_consensus, suppression_requirement, 0.9).

% Enforcement: Emerges naturally from computation and rules.
domain_priors:emerges_naturally(proof_of_work_consensus).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(proof_of_work_consensus, peer_to_peer_network).
narrative_ontology:constraint_victim(proof_of_work_consensus, double_spenders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: NON-MINING USER - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Relies on the network's security without contributing hash power)
   WHEN: biographical (Long-term trust in the ledger's immutability)
   WHERE: trapped (Cannot alter the blockchain without immense computational resources)
   
   WHY THIS CLASSIFICATION:
   For a non-mining user, the Proof-of-Work consensus is a 'Mountain'. They 
   rely on the immutable mathematical difficulty of reversing transactions. 
   This forms an unchangeable foundation of trust, a fixed reality that ensures
   the integrity of their digital assets without their direct involvement.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    proof_of_work_consensus,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NETWORK PROTOCOL - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The rule-set that defines validity and incentive structures)
   WHEN: historical (From whitepaper inception to ongoing development)
   WHERE: arbitrage (Balances security, decentralization, and scalability)
   
   WHY THIS CLASSIFICATION:
   For the Network Protocol itself, Proof-of-Work is a 'Rope'. It is the
   functional coordination mechanism that ensures distributed consensus.
   It leverages computational work to secure the ledger, align incentives,
   and resist attacks, creating a robust, trustless system.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    proof_of_work_consensus,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ATTACKER - Snare
   --------------------------------------------------------------------------
   WHO: powerful (An entity seeking to subvert the system)
   WHEN: immediate (The moment of the "race" against the honest network)
   WHERE: constrained (Prevented from succeeding by the cumulative work)
   
   WHY THIS CLASSIFICATION:
   To the attacker, the Proof-of-Work is a 'Snare'. It is a coercive 
   mechanism that "penalizes" their behavior by making the cost of 
   cheating (energy and CPU) likely higher than the potential gain. 
   It strangles their ability to profitably manipulate the ledger.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    proof_of_work_consensus,
    snare,
    context(
        agent_power(powerful),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(proof_of_work_consensus_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(proof_of_work_consensus, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proof_of_work_consensus, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(proof_of_work_consensus, Type3, context(agent_power(powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(proof_of_work_consensus_tests).

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
 * 1. INDIVIDUAL POWERLESS PERSPECTIVE: Added 'Non-Mining User' as the
 *    individual powerless agent. For them, the Proof-of-Work is a 'Mountain',
 *    an immutable foundation of trust.
 *
 * 2. INSTITUTIONAL PERSPECTIVE: Added 'The Network Protocol' as the
 *    institutional agent. For them, Proof-of-Work is a 'Rope', the core
 *    coordination mechanism of the system.
 *
 * 3. CLASSIFICATION RATIONALE:
 *    - Non-Mining User (Mountain): Immutable trust in computational difficulty.
 *    - Network Protocol (Rope): Mechanism for distributed consensus.
 *    - Attacker (Snare): Computationally impractical to subvert.
 * 
 * 4. CORE INSIGHT: Proof-of-Work creates a powerful 'Rope' of trustless
 *    coordination by leveraging a 'Mountain' of computational difficulty.
 *    This simultaneously acts as a 'Snare' for malicious actors and provides
 *    an immutable foundation for passive users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty revolves around the long-term security and sustainability of the Proof-of-Work mechanism.
 */

omega_variable(
    fifty_one_percent_threshold,
    "What happens when the 'majority of CPU power' on the network is no longer controlled by honest participants, leading to a 51% attack?",
    resolution_mechanism("Requires continuous monitoring of network hash distribution and decentralization metrics; analysis of economic incentives for honest mining versus attacking."),
    impact("If majority is attacker-controlled: The 'Rope' snaps, and the Ledger becomes a 'Snare' for all users, eroding trust. If always honest: The 'Rope' remains secure."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Trusted Third Parties (e.g., Banks, Financial Intermediaries)
 *    Viability: The traditional system for managing electronic cash, relying on central authorities for trust.
 *    Suppression: Rejected by Nakamoto due to the "double-spending problem" and the need for a trustless, peer-to-peer system, effectively turning this "Rope" into a "Snare" for users.
 *
 * CONCLUSION:
 * Proof-of-Work fundamentally suppresses the traditional 'Rope' of trusted
 * third parties, instead building a new 'Rope' of trustless consensus.
 * This shift transforms the "double-spending problem" from an unavoidable
 * 'Mountain' into a solvable constraint.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/proof_of_work_consensus].
 * 2. Multi-perspective: ?- multi_index_report(proof_of_work_consensus).
 * 3. Run tests: ?- run_tests(proof_of_work_consensus_tests).
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
% Functional coordination mechanism — primarily substantive
domain_priors:theater_ratio(proof_of_work_consensus, 0.11).
narrative_ontology:constraint_metric(proof_of_work_consensus, theater_ratio, 0.11).

% --- Analytical perspective classification (missing) ---
% chi = 0.2 * 1.15 (analytical) * 1.2 (global) = 0.276
% Classification: scaffold
constraint_indexing:constraint_classification(proof_of_work_consensus, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
