% ============================================================================
% CONSTRAINT STORY: cap_theorem
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Eric Brewer (2000) / Seth Gilbert & Nancy Lynch (2002)
% ============================================================================

:- module(constraint_cap_theorem, []).

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
 * * constraint_id: cap_theorem
 * human_readable: CAP Theorem (Brewer's Theorem)
 * domain: technological
 * temporal_scope: Permanent (Universal Laws of Distributed Computing)
 * spatial_scope: Global (Digital Networks)
 * * SUMMARY:
 * The CAP theorem states that any distributed data store can only provide two 
 * of three guarantees: Consistency (every read receives the most recent write), 
 * Availability (every request receives a response), and Partition Tolerance 
 * (the system continues to operate despite network failures). 
 * * KEY AGENTS:
 * - The Distributed Systems Architect: Analytical observer mapping the 
 * triangular trade-offs of data integrity.
 * - The Cloud Infrastructure Provider: Institutional agent offering specialized 
 * database "Ropes" (e.g., CP or AP systems) to developers.
 * - The On-Call Engineer: Individual powerless agent trapped by a network 
 * partition ("Split Brain") where the theorem acts as a Snare.
 * * NARRATIVE ARC:
 * CAP functions as a "Mountain" of logic—a fundamental law of information 
 * physics. For a CTO, it is a "Rope" used to coordinate design choices. 
 * However, during a network failure, it becomes a "Snare" for the SRE, 
 * as the theorem extracts the possibility of having both a working 
 * and an accurate system simultaneously.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(cap_theorem_interval, 0, 10).
narrative_ontology:constraint_claim(cap_theorem, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.4 (Moderate). The theorem extracts "system reliability." 
% It forces a cost where one must sacrifice either uptime or data 
% correctness, effectively "taxing" every distributed architecture.
domain_priors:base_extractiveness(cap_theorem, 0.4).

% Suppression score (0.0-1.0)
% Rationale: 0.7 (High). The "Perfect Database" (CAP-complete) is 
% physically and logically suppressed. The theorem renders the 
% alternative invisible to any viable engineering plan.
domain_priors:suppression_score(cap_theorem, 0.7).

% Enforcement requirements
% Emerges naturally from the physics of network latency and logic.
domain_priors:emerges_naturally(cap_theorem).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(cap_theorem, extractiveness, 0.4).
narrative_ontology:constraint_metric(cap_theorem, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cap_theorem, [cloud_consultants, architectural_purity]).
constraint_victim(cap_theorem, [on_call_engineers, real_time_consistency_requirements]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COMPUTER SCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal informational laws.
   WHEN: civilizational - Viewing the laws of logic as permanent.
   WHERE: trapped - Logic cannot bypass the pigeonhole-like limits of state.
   SCOPE: global - Universal computation.
   
   WHY THIS CLASSIFICATION:
   To the scientist, CAP is a Mountain. It is an unchangeable feature of the 
   computational "hardware" of the universe. No amount of innovation or 
   speed-of-light networking can bypass the fundamental impossibility of 
   having all three properties during a partition.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cap_theorem,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CTO / INFRASTRUCTURE LEAD - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to select technologies and define architecture.
   WHEN: biographical - Planning the 10-year growth of a platform.
   WHERE: arbitrage - Can choose different databases for different use cases.
   SCOPE: national - Corporate-wide infrastructure.
   
   WHY THIS CLASSIFICATION:
   For the architect, CAP is a Rope. It is a coordination mechanism. 
   By understanding the trade-offs, they can "tether" their business 
   to the right system (e.g., choosing "Availability" for a social feed 
   but "Consistency" for a bank ledger). It is a functional tool for 
   managing complexity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cap_theorem,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ON-CALL SRE - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the failure of the network.
   WHEN: immediate - 3 AM incident response.
   WHERE: trapped - The system is already built and the partition is happening.
   SCOPE: local - Immediate server cluster.
   
   WHY THIS CLASSIFICATION:
   For the engineer on call during a network split, CAP is a Snare. 
   They are forced to choose between a "lying" system (Consistency loss) 
   or a "broken" system (Availability loss). The theorem strangles their 
   ability to provide a perfect fix, extracting their time and sanity 
   while providing no escape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cap_theorem,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cap_theorem_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Snare
    constraint_indexing:constraint_classification(cap_theorem, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(cap_theorem, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(cap_theorem, snare, context(individual_powerless, immediate, trapped, local)).

test(power_extractiveness_reliability) :-
    % SREs feel the extraction of their agency during a failure (Snare).
    % CTOs use the theorem to justify budget and tech stack (Rope).
    domain_priors:base_extractiveness(cap_theorem, E),
    E >= 0.4.

test(time_immutability_logic) :-
    % Civilizational view of distributed logic = Mountain.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(cap_theorem_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): CAP extracts the "ideal." It forces an energy 
 * and logic cost upon every system designer, preventing a "perfect" system.
 * 2. SUPPRESSION (0.7): High. Because the "Perfect System" is logically 
 * impossible, it is effectively suppressed from our reality.
 * 3. NOOSE LOGIC: Applied to the SRE because they are the "victims" of the 
 * theorem's trade-off when the physical world (network) breaks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pacelc_transition_cost,
    "To what extent can the PACELC extension (addressing latency vs consistency 
    when no partition exists) untie the CAP Snare (Rope), or is the cost 
    always a net-negative for complexity (Snare)?",
    resolution_mechanism("Monitor industry shift toward latency-aware 
    consistent databases"),
    impact("If Rope: The Snare is loosened during normal operation. 
    If Snare: Complexity just creates a different kind of trap."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: PACELC Theorem
 * Viability: High. Describes the trade-offs during normal operation 
 * (Latency vs. Consistency) as well as during partitions.
 * Suppression: None. It is a more detailed map of the same Mountain.
 * * CONCLUSION:
 * The CAP theorem remains a Mountain because there is no alternative that 
 * provides all three guarantees. It is a fundamental law of the digital 
 * universe.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_cap_theorem].
 * 2. Multi-perspective: ?- multi_index_report(cap_theorem).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
