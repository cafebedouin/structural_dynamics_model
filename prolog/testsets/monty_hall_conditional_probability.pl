% ============================================================================
% CONSTRAINT STORY: monty_hall_conditional_probability
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Steve Selvin (1975) / Marilyn vos Savant (1990)
% ============================================================================

:- module(constraint_monty_hall, []).

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
 * * constraint_id: monty_hall_conditional_probability
 * human_readable: The Monty Hall Problem
 * domain: mathematical/probabilistic
 * temporal_scope: 1975 - Present
 * spatial_scope: Global/Abstract (Game Theory)
 * * SUMMARY:
 * The Monty Hall problem is a counter-intuitive probability puzzle based on a 
 * game show scenario. It demonstrates how new information (opening a door) 
 * shifts probabilities in a way that defies linear human intuition, creating 
 * a structural constraint where "switching" is mathematically superior.
 * * KEY AGENTS:
 * - The Contestant (Subject): The powerless agent facing a choice with 
 * hidden variables and counter-intuitive odds.
 * - The Game Show Host (Institutional): The rule-enforcer who knows the 
 * location of the "Car" and uses the constraint to maintain dramatic tension.
 * - The Intuitive Skeptic (Victim): An agent whose common-sense logic 
 * ("it's 50/50") is "strangled" by the underlying Bayesian Mountain.
 * * NARRATIVE ARC:
 * The problem functions as a "Mountain" of Bayesian reality—the 2/3 vs 1/3 
 * odds are a fixed, unyielding truth of the system's state space. In 
 * education, it is a "Rope" for coordinating an understanding of conditional 
 * probability. However, for the contestant's ego, the paradox acts as a 
 * "Snare," extracting the possibility of a "rational" 50/50 choice and 
 * "strangling" those who refuse to pivot when the rules shift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor for the Deferential Realism Auditor
narrative_ontology:interval(probability_paradox_era, 1975, 2026).
narrative_ontology:constraint_claim(monty_hall_conditional_probability, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.33. The paradox "extracts" 1/3 of the winning probability from 
% those who stay with their original choice, effectively taxing "loyalty" 
% or "stasis" in a changing environment.
domain_priors:base_extractiveness(monty_hall_conditional_probability, 0.33).

% Suppression score (0.0-1.0)
% Rationale: 0.6. It heavily suppresses the visibility of the "50/50" 
% alternative, rendering the intuitive answer mathematically fraudulent 
% and socially ridiculed in academic circles.
domain_priors:suppression_score(monty_hall_conditional_probability, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(monty_hall_conditional_probability, extractiveness, 0.33).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, suppression_requirement, 0.6).

% Enforcement: Emerges naturally from the laws of conditional probability.
domain_priors:emerges_naturally(monty_hall_conditional_probability).
domain_priors:requires_active_enforcement(monty_hall_conditional_probability).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(monty_hall_conditional_probability, bayesian_statisticians).
narrative_ontology:constraint_beneficiary(monty_hall_conditional_probability, logical_pivoters).
narrative_ontology:constraint_victim(monty_hall_conditional_probability, intuitive_thinkers).
narrative_ontology:constraint_victim(monty_hall_conditional_probability, static_choice_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DOORS (STATE SPACE) - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - The physical configuration of the car has no agency.
   WHEN: immediate - The probability is set the moment the car is placed.
   WHERE: trapped - Bound within the three-door system.
   SCOPE: local - Immediate physical arrangement.
   
   WHY THIS CLASSIFICATION:
   For the system itself, the conditional probabilities are a natural law. 
   Opening a goat-door *must* concentrate the remaining probability into 
   the unchosen, unopened door. It is an unyielding Mountain of logic.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    monty_hall_conditional_probability,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PROBABILITY PROFESSOR - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to use the paradox to coordinate classroom learning.
   WHEN: biographical - Spanning the duration of a student's cognitive shift.
   WHERE: mobile - Can choose different variants (100 doors) to illustrate the point.
   SCOPE: global - Universal pedagogical tool.
   
   WHY THIS CLASSIFICATION:
   For the educator, the problem is a "Rope"—a functional coordination 
   mechanism. It allows them to "pull" students away from the "Snare" of 
   faulty intuition toward a standard of achievement in Bayesian thinking.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    monty_hall_conditional_probability,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SKEPTICAL CONTESTANT - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Lacks the tools to compute the shift in real-time.
   WHEN: immediate - Forced to choose between switching or staying NOW.
   WHERE: constrained - The "exit" (switching) feels like a betrayal of intuition.
   SCOPE: local - A specific, high-stakes game.
   
   WHY THIS CLASSIFICATION:
   For the intuitive thinker, the problem is a "Snare." It "strangles" their 
   common-sense confidence. Because they perceive a 50/50 choice, the system 
   extracts 33% of their winning chance (extraction) if they fail to 
   overcome their cognitive bias. The logic of the host "chokes" their 
   original intent.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    monty_hall_conditional_probability,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(monty_hall_conditional_probability, E),
    E >= 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(monty_hall_conditional_probability_tests).

test(multi_perspective_variance) :-
    % Doors -> Mountain
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, Type1, context(powerless, immediate, trapped, local)),
    % Professor -> Rope
    constraint_indexing:constraint_classification(monty_hall_conditional_probability, Type2, context(institutional, biographical, mobile, global)),
    Type1 \= Type2.

test(intuitive_extraction_penalty) :-
    % Demonstrates that the Snare classification extracts more value (win %) from the powerless agent.
    ContextPowerless = context(powerless, immediate, constrained, local),
    constraint_indexing:extractiveness_for_agent(monty_hall_conditional_probability, ContextPowerless, Score),
    Score >= 0.33.

test(natural_emergence) :-
    domain_priors:emerges_naturally(monty_hall_conditional_probability).

:- end_tests(monty_hall_conditional_probability_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.33): 
 * The system is mathematically biased against the "Stay" strategy. It 
 * extracts exactly 33.3% of the potential win-rate compared to the "Switch" 
 * strategy.
 * 2. PERSPECTIVE SELECTION: 
 * Chose Doors (Subject), Professor (Institution), and Skeptic (Victim) 
 * to highlight the gap between "State," "Utility," and "Trap."
 * 3. SUPPRESSION SCORE (0.6): 
 * The paradox is famous because it so effectively suppresses the "linear" 
 * alternative, making those who hold onto it look "irrational."
 */

% OMEGA IDENTIFICATION
omega_variable(
    host_behavior_certainty,
    "Is the 'Mountain' stable if the host's behavior is not guaranteed (e.g., Monty only offers the switch when you have the car)?",
    resolution_mechanism("Meta-analysis of game rules and host intent across different game show iterations."),
    impact("If Host is malicious: The 'Switch' Rope becomes a 'Snare'. If Host is rule-bound: It is a Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The "Stay" Strategy
 * Viability: Winning 1/3 of the time.
 * Suppression: Actively suppressed by math educators as "wrong."
 * Evidence: Vos Savant's column receiving thousands of angry (incorrect) letters.
 * * ALTERNATIVE 2: Malicious Host (The "Monty Fall" variant)
 * Viability: If the host only opens a door when you are currently winning.
 * Suppression: Ignored in the standard "idealized" version of the problem.
 * * CONCLUSION:
 * The standard problem assumes a "Rule-Bound" host, turning the "Switch" into 
 * a "Rope." If the host is not rule-bound, the entire Mountain collapses 
 * into a psychological Scaffold.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [monty_hall_conditional_probability].
 * 2. Multi-perspective: ?- multi_index_report(monty_hall_conditional_probability).
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
% Formal truth — substantive with near-zero performative component
domain_priors:theater_ratio(monty_hall_conditional_probability, 0.0).
narrative_ontology:constraint_metric(monty_hall_conditional_probability, theater_ratio, 0.0).

% --- Analytical perspective classification (missing) ---
% chi = 0.33 * 1.15 (analytical) * 1.2 (global) = 0.455
% Classification: tangled_rope
constraint_indexing:constraint_classification(monty_hall_conditional_probability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
