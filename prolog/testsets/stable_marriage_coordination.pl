% ============================================================================
% CONSTRAINT STORY: stable_marriage_coordination
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: David Gale & Lloyd Shapley (1962) / College Admissions and the Stability of Marriage
% ============================================================================

:- module(constraint_stable_marriage, []).

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
 * * constraint_id: stable_marriage_coordination
 * human_readable: Stable Marriage Problem (Gale-Shapley Algorithm)
 * domain: economic/social/technological
 * temporal_scope: 1962 - Present
 * spatial_scope: Global/Abstract (Matching Markets)
 * * SUMMARY:
 * The Stable Marriage Problem involves finding a stable matching between two equally 
 * sized sets of elements given a list of preferences for each element. A matching 
 * is "stable" if there is no pair that would both prefer each other over their 
 * current partners. It represents a fundamental constraint on how decentralized 
 * preferences can be reconciled into a self-enforcing social equilibrium.
 * * KEY AGENTS:
 * - The Suitor (Subject): The powerless agent (in a fixed-rule iteration) whose 
 * outcome depends on the sequential rejection or acceptance by the other set.
 * - The Matching Coordinator (Institutional): An agent (like the NRMP for doctors) 
 * who uses the algorithm as a "Rope" to ensure market stability and prevent "poaching."
 * - The Strategic Outlier (Victim): An agent whose optimal individual preference 
 * is "strangled" by the global requirement for stability, often resulting in a 
 * sub-optimal match to prevent systemic collapse.
 * * NARRATIVE ARC:
 * The Stable Marriage Problem is a "Mountain" of game-theoretic reality—in any 
 * matching market, the existence of a stable state is a mathematical certainty, 
 * yet the specific outcome is biased toward the set that proposes. In labor 
 * markets (like medical residency), it is a "Rope" used to coordinate thousands 
 * of individuals. However, for an agent with highly specific, non-conformist 
 * preferences, the stability requirement acts as a "Noose," extracting their 
 * individual utility (extraction) to maintain the "choking" grip of the 
 * aggregate equilibrium.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for the DR-Audit Suite
narrative_ontology:interval(matching_theory_era, 1962, 2026).
narrative_ontology:constraint_claim(stable_marriage_coordination, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.4. The algorithm "extracts" the possibility of "perfect" 
% matches for all individuals to enforce a "stable" state where no one 
% has the incentive to defect. It inherently favors the group that proposes 
% (Suitor-Optimality), creating a systematic extraction of utility from 
% the receiving group.
domain_priors:base_extractiveness(stable_marriage_coordination, 0.4).

% Suppression score (0.0-1.0)
% Rationale: 0.3. It suppresses the visibility of "unstable" but 
% higher-utility matches, rendering them functionally illegal or 
% "fraudulent" within coordinated markets like residency matching.
domain_priors:suppression_score(stable_marriage_coordination, 0.3).

% Enforcement: Emerges naturally from the Gale-Shapley mechanism.
domain_priors:emerges_naturally(stable_marriage_coordination).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(stable_marriage_coordination, extractiveness, 0.4).
narrative_ontology:constraint_metric(stable_marriage_coordination, suppression_requirement, 0.3).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(stable_marriage_coordination, proposing_set_agents). % Suitor-optimal results.
constraint_beneficiary(stable_marriage_coordination, market_clearing_houses).
constraint_victim(stable_marriage_coordination, receiving_set_agents). % Receiver-pessimal results.
constraint_victim(stable_marriage_coordination, niche_preference_outliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RESIDENCY APPLICANT - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - The student cannot change the algorithm's rules.
   WHEN: immediate - True at the moment the "Match Day" results are released.
   WHERE: trapped - Bound by the institutional rules of the medical profession.
   SCOPE: national - Part of a country-wide labor allocation.
   
   WHY THIS CLASSIFICATION:
   For the individual student, the algorithm is an absolute Mountain. They 
   submit their ranks and must accept the result. The existence of a "stable" 
   match means they cannot negotiate a better spot elsewhere because no 
   hospital they prefer would prefer them back. The math is an unyielding law.
   
   
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    stable_marriage_coordination,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MARKET COORDINATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to mandate the use of a centralized matching system.
   WHEN: biographical - Managing the stability of the labor market over decades.
   WHERE: mobile - Can choose to make the algorithm suitor-optimal or receiver-optimal.
   SCOPE: global - Applying matching theory to diverse sectors.
   
   WHY THIS CLASSIFICATION:
   For the institution (e.g., NRMP), the Stable Marriage algorithm is a 
   "Rope"—a functional coordination tool. It prevents "unraveling" where 
   hospitals and students make sub-optimal early offers. It pulls the 
   entire market toward a stable standard of achievement (equilibrium).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    stable_marriage_coordination,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE RECEIVING-SIDE AGENT - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has the tools to rank, but is bound by the proposer's advantage.
   WHEN: immediate - Realizing they have been matched to their least-preferred stable option.
   WHERE: constrained - The "exit" (breaking the match) is professionally ruinous.
   SCOPE: local - Impacting their specific career trajectory.
   
   WHY THIS CLASSIFICATION:
   For the agent on the receiving side of the proposals, the algorithm is a 
   "Noose." While the outcome is "stable," it is mathematically proven to be 
   the *worst* possible stable match for them. It extracts their potential 
   satisfaction (extraction) to guarantee the stability of the system, 
   "choking" their agency in favor of the proposing set.
   
   
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    stable_marriage_coordination,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(stable_marriage_coordination, E),
    E >= 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(stable_marriage_tests).

test(multi_perspective_variance) :-
    % Applicant -> Mountain
    constraint_indexing:constraint_classification(stable_marriage_coordination, Type1, context(individual_powerless, immediate, trapped, national)),
    % Coordinator -> Rope
    constraint_indexing:constraint_classification(stable_marriage_coordination, Type2, context(institutional, biographical, mobile, global)),
    Type1 \= Type2.

test(receiver_extraction_penalty) :-
    % Receiver-side agents feel the 0.4 extraction of utility as a Noose.
    Context = context(individual_moderate, immediate, constrained, local),
    constraint_indexing:extractiveness_for_agent(stable_marriage_coordination, Context, Score),
    Score >= 0.3.

test(natural_emergence) :-
    domain_priors:emerges_naturally(stable_marriage_coordination).

:- end_tests(stable_marriage_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.4): 
 * I chose 0.4 because of the inherent "Proposer-Optimality" vs "Receiver-Pessimality" 
 * duality. The system extracts utility from one set to benefit the other while 
 * maintaining the "Mountain" of stability.
 * 2. CLASSIFICATION: 
 * Captured how the "Gale-Shapley Mountain" serves as a "Rope" for institutions 
 * to clear markets, but can be a "Noose" for the passive side of the proposal.
 */

% OMEGA IDENTIFICATION
omega_variable(
    strategic_misreporting_impact,
    "Is the 'Mountain' of stability vulnerable if agents lie about their preferences (Scaffold)?",
    resolution_mechanism("Analysis of incentive compatibility: Gale-Shapley is strategy-proof for proposers but not for receivers."),
    impact("If strategic lying is rampant: The 'Mountain' of stability is a mirage, and the algorithm becomes a 'Noose' of manipulation."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Decentralized Search (The "Dating Market")
 * Viability: Individuals search for partners without a central coordinator.
 * Suppression: Often leads to "unstable" pairs and high search costs, 
 * suppressed by institutional "Ropes" in professional settings to ensure 
 * 100% market clearing.
 * * ALTERNATIVE 2: Random Allocation
 * Viability: Fair in terms of probability, but highly unstable.
 * Suppression: Rejected because it violates the "Mountain" of 
 * individual rationality.
 * * CONCLUSION:
 * The existence of decentralized "instability" (Alternative 1) is what 
 * makes the Stable Marriage "Rope" necessary for large-scale coordination.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_stable_marriage].
 * 2. Multi-perspective: ?- multi_index_report(stable_marriage_coordination).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
