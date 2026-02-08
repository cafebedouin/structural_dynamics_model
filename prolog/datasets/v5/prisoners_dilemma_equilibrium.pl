% ============================================================================
% CONSTRAINT STORY: prisoners_dilemma_equilibrium
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Merrill Flood & Melvin Dresher (1950) / Albert Tucker / Game Theory
% ============================================================================

:- module(constraint_prisoners_dilemma, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: prisoners_dilemma_equilibrium
 * human_readable: The Prisoner's Dilemma (Nash Equilibrium)
 * domain: economic/social/political
 * temporal_scope: 1950 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Interdependent Choice)
 * * SUMMARY:
 * The Prisoner's Dilemma is a standard example of a game analyzed in game theory 
 * that shows why two completely rational individuals might not cooperate, even 
 * if it appears that it is in their best interest to do so. It defines a 
 * structural constraint where individual rationality leads to collective 
 * sub-optimality.
 * * KEY AGENTS:
 * - The Prisoner (Subject): The powerless agent trapped in a binary choice 
 * (Cooperate/Defect) where the optimal individual move destroys the group.
 * - The Institutional Designer: An agent who uses the dilemma as a "Rope" to 
 * coordinate behavior through external incentives (laws, contracts).
 * - The Exploiter (Individual Powerful): An agent who treats the dilemma as 
 * a "Snare" to extract value from cooperative subjects through defection.
 * * NARRATIVE ARC:
 * The Prisoner's Dilemma is the "Mountain" of non-cooperative reality—the Nash 
 * Equilibrium is a fixed, unyielding gravity that pulls rational agents toward 
 * mutual defection. In civil society, it is a "Rope" for coordination; we 
 * build institutions to "pull" ourselves out of the equilibrium. However, in 
 * unregulated markets or warfare, the dilemma acts as a "Snare," extracting 
 * total utility (extraction) and strangling the possibility of peace.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for the Deferential Realism Auditor
narrative_ontology:interval(game_theory_era, 1950, 2026).
narrative_ontology:constraint_claim(prisoners_dilemma_equilibrium, mountain).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: 0.7. The dilemma is highly extractive; it "takes" the potential 
% surplus of cooperation and discards it into the "waste" of mutual defection.
domain_priors:base_extractiveness(prisoners_dilemma_equilibrium, 0.7).

% Suppression score (0.0-1.0)
% Rationale: 0.4. It suppresses the visibility of cooperative alternatives 
% by making them appear "irrational" from an individual perspective.
domain_priors:suppression_score(prisoners_dilemma_equilibrium, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, extractiveness, 0.7).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, suppression_requirement, 0.4).

% Enforcement: Emerges naturally from the interaction of rational self-interest.
domain_priors:emerges_naturally(prisoners_dilemma_equilibrium).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(prisoners_dilemma_equilibrium, opportunistic_defectors).
constraint_beneficiary(prisoners_dilemma_equilibrium, centralized_authorities). % They justify their existence as the "Rope" to solve it.
constraint_victim(prisoners_dilemma_equilibrium, uncoordinated_collectives).
constraint_victim(prisoners_dilemma_equilibrium, trust_based_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE TRAPPED PRISONER - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - The prisoner cannot negotiate or change the payoffs.
   WHEN: immediate - The choice must be made now; the outcome is instant.
   WHERE: trapped - Bound within the interrogation room/payoff matrix.
   SCOPE: local - Focused on their own sentence.
   
   WHY THIS CLASSIFICATION:
   For the prisoner, the incentive to defect is a natural law of survival. 
   Because they cannot communicate or enforce a contract, the "Defect" 
   strategy is an unyielding Mountain of logic. Any attempt to "Cooperate" 
   without an exit option is a suicide mission.
   
   NARRATIVE EVIDENCE:
   "Rationality leads you to the cell, but logic keeps you there." (General 
   Game Theory Maxim).
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    prisoners_dilemma_equilibrium,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STATE LEGISLATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to rewrite the payoff matrix through law.
   WHEN: biographical - Planning the stability of a market or society.
   WHERE: mobile - Can "exit" the dilemma by imposing external penalties for defection.
   SCOPE: national - Applying the solution across the country.
   
   WHY THIS CLASSIFICATION:
   For the state, the dilemma is a "Rope"—a tool for functional coordination. 
   By recognizing the "Mountain" of the dilemma, the institution creates 
   contracts and police forces to "pull" the agents toward the 
   standard of achievement (Mutual Cooperation).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    prisoners_dilemma_equilibrium,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ARMS RACE NATION - Snare
   --------------------------------------------------------------------------
   
   WHO: powerful - Possesses nukes/power, but bound by the opponent's logic.
   WHEN: historical - Spanning decades of the Cold War.
   WHERE: constrained - The "exit" (disarmament) is too risky to take alone.
   SCOPE: global - The entire planet is at risk.
   
   WHY THIS CLASSIFICATION:
   In an arms race, the dilemma is a "Snare." It "strangles" both nations, 
   extracting trillions of dollars in wealth (extraction) for weapons that 
   neither side wants to use. Both are powerful, yet both are "choked" by 
   the inability to reach the cooperative "Rope" without being exploited.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    prisoners_dilemma_equilibrium,
    snare,
    context(
        agent_power(powerful),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(prisoners_dilemma_equilibrium, E),
    E >= 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(prisoners_dilemma_tests).

test(multi_perspective_variance) :-
    % Prisoner -> Mountain
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, Type1, context(powerless, immediate, trapped, local)),
    % Legislator -> Rope
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, Type2, context(institutional, biographical, mobile, national)),
    % Nation -> Snare
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, Type3, context(powerful, historical, constrained, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(extraction_of_cooperation_value) :-
    % Powerless agents feel the 0.7 extraction of their potential surplus.
    domain_priors:base_extractiveness(prisoners_dilemma_equilibrium, E),
    E > 0.5.

test(iterated_exit_rope) :-
    % Demonstrates that shifting to a historical/mobile context (Iteration) allows Rope classification.
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, rope, context(institutional, biographical, mobile, national)).

:- end_tests(prisoners_dilemma_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.7):
 * The dilemma is purely extractive of human flourishing. It takes a 
 * positive-sum possibility and "extracts" it into a zero-sum or 
 * negative-sum reality.
 * * 2. CLASSIFICATION RATIONALE:
 * - Prisoner (Mountain): The classic one-shot scenario.
 * - Legislator (Rope): Using the "Rope" of the law to bind defectors.
 * - Arms Race (Snare): High-power agents trapped by mutual fear.
 * * 3. OMEGAS:
 * Formalized the uncertainty of "Inherent Altruism"—does a biological 
 * Mountain of empathy override the mathematical Mountain of the dilemma?
 */

% YOUR OMEGAS HERE:
omega_variable(
    biological_altruism_override,
    "Do biological/evolutionary empathy markers function as an internal Mountain that prevents the Nash Equilibrium?",
    resolution_mechanism("Long-term neuro-economic study of pre-rational cooperation in social mammals."),
    impact("If Yes: The Dilemma is a Scaffold for humans. If No: It is a permanent Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Iterated Prisoner's Dilemma (Tit-for-Tat)
 * Viability: In the "Long Run," cooperation can emerge as a stable strategy.
 * Status: A "Rope" that turns the Mountain into a navigable path.
 * * ALTERNATIVE 2: External Coercion (Leviathan)
 * Viability: A third party punishes defection, changing the payoffs.
 * Suppression: Often presented as the "only" way to prevent chaos, 
 * suppressing the viability of Alternative 1 (Self-organized trust).
 * * CONCLUSION:
 * The presence of "Trust" as a viable alternative (Alternative 1) is what 
 * makes the dilemma a "Snare" rather than a "Mountain" for sophisticated 
 * societies. If trust is suppressed, the "Snare" tightens.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [prisoners_dilemma_equilibrium].
 * 2. Multi-perspective: ?- multi_index_report(prisoners_dilemma_equilibrium).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
