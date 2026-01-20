% ============================================================================
% CONSTRAINT STORY: proof_of_work_consensus
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: proof_of_work_consensus
 * human_readable: Hash-Based Proof-of-Work
 * domain: technological/economic
 * temporal_scope: 2008 - Present (Digital Era)
 * spatial_scope: Global (Distributed Network)
 * * SUMMARY:
 * A distributed consensus mechanism that replaces trusted third parties with 
 * computational proof. It timestamps transactions by hashing them into an 
 * ongoing chain of hash-based proof-of-work, forming a record that cannot 
 * be changed without redoing the work.
 * * KEY AGENTS:
 * - The Node (Individual Powerless/Moderate): Expresses acceptance of valid 
 * blocks by working on extending them.
 * - The Attacker (Individual Powerful): Must outpace the honest network by 
 * controlling more CPU power.
 * - The Network Protocol (Institutional): The rule-set that defines validity 
 * and incentive structures.
 * * NARRATIVE ARC:
 * The text identifies the "double-spending" problem inherent in electronic 
 * cash. It moves from a state of reliance on financial 
 * institutions (Ropes/Nooses) to a peer-to-peer system where "consensus" is 
 * reached through a "Mountain" of cumulative CPU work.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(bitcoin_consensus_era, 0, 10).
narrative_ontology:constraint_claim(proof_of_work_consensus, rope).

% Base extractiveness score: 0.2
% Rationale: The system does not "extract" value in a zero-sum sense; 
% instead, it consumes energy to produce the "security" of the ledger, 
% rewarding participants with new coins.
domain_priors:base_extractiveness(proof_of_work_consensus, 0.2).

% Suppression score: 0.9
% Rationale: The mechanism makes it "computationally impractical" for 
% an attacker to change the record once it is buried under subsequent 
% blocks.
domain_priors:suppression_score(proof_of_work_consensus, 0.9).

% Enforcement requirements: Emerges naturally from computation and rules.
domain_priors:emerges_naturally(proof_of_work_consensus).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(proof_of_work_consensus, extractiveness, 0.2).
narrative_ontology:constraint_metric(proof_of_work_consensus, suppression_requirement, 0.9).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(proof_of_work_consensus, [peer_to_peer_network, honest_nodes]).
constraint_victim(proof_of_work_consensus, [double_spenders, financial_intermediaries]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE HONEST NODE - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - A participant with CPU power to "vote".
   WHEN: immediate - The process of extending the chain block-by-block.
   WHERE: mobile - Nodes can leave and rejoin the network at will.
   SCOPE: global - A single network that spans the internet.
   
   WHY THIS CLASSIFICATION:
   For the honest node, the consensus protocol is a "Rope"—a functional 
   coordination mechanism that allows them to cooperate without a master. 
   It is beneficial and they "vote" with their CPU power to uphold it 
  .
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    proof_of_work_consensus,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        constraint_beneficiary(proof_of_work_consensus, honest_nodes),
        constraint_victim(proof_of_work_consensus, []),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(proof_of_work_consensus, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ATTACKER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerful - An entity seeking to subvert the system.
   WHEN: immediate - The moment of the "race" against the honest chain.
   WHERE: constrained - Prevented from succeeding by the cumulative work.
   SCOPE: national - Attempting to "outpace" a global system.
   
   WHY THIS CLASSIFICATION:
   To the attacker, the Proof-of-Work is a "Noose." It is a coercive 
   mechanism that "penalizes" their behavior by making the cost of 
   cheating (energy and CPU) likely higher than the potential gain 
  .
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    proof_of_work_consensus,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(proof_of_work_consensus, honest_nodes),
        constraint_victim(proof_of_work_consensus, attacker),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(proof_of_work_consensus, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: CRYPTOGRAPHIC REALITY - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observing the mathematical difficulty.
   WHEN: civilizational - A permanent law of computational complexity.
   WHERE: trapped - "Redoing the proof-of-work" is the only path to change.
   SCOPE: global - Universal across all computing environments.
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, the SHA-256 hash difficulty is a 
   "Mountain." It is an unchangeable law of nature (probability/physics) 
   that dictates zero degrees of freedom for anyone trying to find a 
   hash without calculation.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    proof_of_work_consensus,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        constraint_beneficiary(proof_of_work_consensus, []),
        constraint_victim(proof_of_work_consensus, []),
        spatial_scope(global)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(bitcoin_consensus_tests).

test(honest_vs_attacker_gap) :-
    % Honest node sees a Rope (coordination); Attacker sees a Noose (coercion).
    constraint_indexing:constraint_classification(proof_of_work_consensus, rope, context(agent_power(individual_moderate), _, _, _, _, _)),
    constraint_indexing:constraint_classification(proof_of_work_consensus, noose, context(agent_power(individual_powerful), _, _, _, _, _)).

test(cumulative_work_mountain) :-
    % Analytical observers see the hash difficulty as a Mountain.
    constraint_indexing:constraint_classification(proof_of_work_consensus, mountain, context(agent_power(analytical), _, _, _, _, _)).

test(incentive_alignment) :-
    % Verify low extractiveness facilitates the "Rope" experience for honest participants.
    domain_priors:base_extractiveness(proof_of_work_consensus, 0.2).

:- end_tests(bitcoin_consensus_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: Proof-of-Work is the ultimate "Rope" built on top of 
 * a "Mountain." The mathematical difficulty (Mountain) provides the 
 * friction that makes the coordination (Rope) possible. 
 * 2. THE PERSPECTIVAL GAP: The paper explicitly leverages the gap between 
 * the Honest Node (who views the rules as a beneficial Rope for commerce) 
 * and the Attacker (who views the rules as a Noose that forces them to be 
 * honest for profit).
 * 3. EXTRACTIVENESS (0.2): Nakamoto's genius was in ensuring that the 
 * "extraction" of CPU power was converted into the "benefit" of ledger 
 * security, keeping the score low for systemic participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fifty_one_percent_threshold,
    "What happens when the 'majority of CPU power' is no longer honest?",
    resolution_mechanism("Requires empirical observation of network hash distribution."),
    impact("If majority is attacker-controlled: The Rope snaps, and the Ledger becomes a Noose for all users."),
    confidence_without_resolution(medium)
).

omega_variable(
    incentive_stability,
    "Will transaction fees be sufficient to support the network once new coin subsidies end?",
    resolution_mechanism("Long-term economic modeling of block space demand."),
    impact("If incentives fail: The system reverts to a Noose of centralizing pools."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Trusted Third Parties (Mint/Bank)
 * Viability: The existing system of commerce.
 * Suppression: Rejected because it requires "trust" and is subject to 
 * "arbitrary" reversals/double-spending risks.
 * * ALTERNATIVE 2: CPU-Voting / Consensus
 * Viability: The Bitcoin model.
 * * CONCLUSION:
 * The active rejection of "Trusted Intermediaries" (Traditional Ropes) 
 * necessitates the construction of this "Calculated Rope" (PoW).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
