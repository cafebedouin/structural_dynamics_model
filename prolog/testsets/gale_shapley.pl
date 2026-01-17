% ============================================================================
% CONSTRAINT STORY: gale_shapley_matching
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Gale-Shapley Algorithm / Stable Marriage Problem
% ============================================================================

:- module(constraint_gale_shapley_matching, []).

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
 * * constraint_id: gale_shapley_matching
 * human_readable: The Gale-Shapley Stable Matching
 * domain: technological/social/mathematical
 * temporal_scope: Biographical to Historical
 * spatial_scope: Regional to Global
 * * SUMMARY:
 * The Gale-Shapley algorithm ensures a "stable" matching between two sets of agents 
 * (e.g., students and schools), where no two agents prefer each other over their 
 * current partners. While stable, the outcome is systematically biased toward 
 * the "proposing" side (who get their best possible stable match) over the 
 * "receiving" side (who get their worst).
 * * KEY AGENTS:
 * - The Proposers: Agents who initiate offers (e.g., Medical Students in the NRMP).
 * - The Receivers: Agents who accept/reject offers (e.g., Residency Programs).
 * - The Designer: The institutional entity that mandates the algorithmic matching.
 * * NARRATIVE ARC:
 * The constraint functions as a "fairness trap." It guarantees stability (no 
 * divorces/defections) but encodes a silent asymmetry into the result based 
 * solely on who is allowed to move first.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(gale_shapley_matching, 0, 10).
narrative_ontology:constraint_claim(gale_shapley_matching, algorithmic_determinism).

% Base extractiveness: Moderate (0.5).
% The algorithm extracts a "preference tax" from the receiving side to ensure 
% the proposers are optimized.
domain_priors:base_extractiveness(gale_shapley_matching, 0.5).

% Suppression: Moderate (0.6).
% Alternatives like "decentralized markets" are often suppressed by institutions 
% (like the NRMP) to prevent "exploding offers" and market failure.
domain_priors:suppression_score(gale_shapley_matching, 0.6).

% Enforcement: Requires active enforcement (Institutional mandate).
domain_priors:requires_active_enforcement(gale_shapley_matching).

% Beneficiaries: The Proposing Side (receive their optimal stable match).
constraint_beneficiary(gale_shapley_matching, proposing_agents).

% Victims: The Receiving Side (receive their pessimal stable match).
constraint_victim(gale_shapley_matching, receiving_agents).

% Metrics
narrative_ontology:constraint_metric(gale_shapley_matching, extractiveness, 0.5).
narrative_ontology:constraint_metric(gale_shapley_matching, suppression_requirement, 0.6).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PROPOSER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the proposer, the algorithm is a "Rope." It provides a clear, 
   strategy-proof path to the best possible outcome. They are encouraged 
   to list their true preferences, and the system coordinates their 
   ascent to an optimal partner.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gale_shapley_matching,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(gale_shapley_matching, E),
    E =< 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE RECEIVER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (relative to the algorithm's bias)
   WHEN: immediate
   WHERE: trapped
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   The receiver experiences the algorithm as a "Noose." They are forced 
   into their worst possible stable outcome. While they are "stable," 
   the structural asymmetry of the algorithm's proposal-response 
   dynamic squeezes their agency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gale_shapley_matching,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(gale_shapley_matching, S),
    S > 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GAME THEORIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the theorist, stability is a "Mountain." The existence of a stable 
   matching is a mathematical certainty. The trade-offs between proposer 
   and receiver are not "unfair," but rather properties of the 
   solution space itself.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gale_shapley_matching,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(gale_shapley_matching), % Logical emergence
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(gale_shapley_tests).

test(proposer_advantage_variance) :-
    % Test that proposers see a Rope while receivers see a Noose
    constraint_indexing:constraint_classification(gale_shapley_matching, rope, context(agent_power(individual_moderate), _, mobile, _)),
    constraint_indexing:constraint_classification(gale_shapley_matching, noose, context(agent_power(individual_powerless), _, trapped, _)).

test(stability_immutability) :-
    % Analytical perspective should always find the 'Mountain' of stability
    constraint_indexing:constraint_classification(gale_shapley_matching, mountain, context(agent_power(analytical), _, _, _)).

:- end_tests(gale_shapley_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.5): The asymmetry is inherent but not strictly "evil." 
 * It is an extraction of the receiver's preference-rank to benefit 
 * the proposer's rank.
 * 2. NOOSE CLASSIFICATION: I assigned "Noose" to receivers because they 
 * are mathematically guaranteed to get the "pessimal" version of 
 * a stable match, and they are usually "trapped" by institutional 
 * requirements to participate (e.g., medical residency).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    preference_honesty,
    "Do agents actually report true preferences, or do they game the system?",
    resolution_mechanism("Audit of ranking data vs. long-term satisfaction surveys"),
    impact("If Truthful: Proposer-Optimal holds. If Strategic: The 'Rope' may fray/break."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Decentralized Markets: Chaotic, but allows for negotiation.
 * 2. Receiver-Proposing: Swaps the beneficiaries.
 * 3. Random Proposer Assignment: Balances the "Noose" effect across both sides.
 * * CONCLUSION:
 * The choice of WHO proposes transforms the algorithm from a 
 * neutral Mountain into a directional Rope/Noose.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
