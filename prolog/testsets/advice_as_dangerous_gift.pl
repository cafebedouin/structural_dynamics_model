% ============================================================================
% CONSTRAINT STORY: advice_as_dangerous_gift
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: J.R.R. Tolkien, "The Fellowship of the Ring" (via cafebedouin.org)
% ============================================================================

:- module(constraint_advice_as_dangerous_gift, []).

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
 * 
 * constraint_id: advice_as_dangerous_gift
 * human_readable: The Hazard of Counsel
 * domain: social/philosophical/literary
 * temporal_scope: High Fantasy Third Age / R7
 * spatial_scope: The Shire / Middle-earth
 * 
 * SUMMARY:
 * Advice is framed as a "dangerous gift" because it involves one agent 
 * attempting to choose a path for another without full context. 
 * Because "all courses may run ill," the act of giving advice creates a 
 * systemic risk for both the giver (responsibility) and the receiver (error).
 * 
 * KEY AGENTS:
 * - Frodo Baggins (Individual Powerless): The seeker of advice, caught between subtle forces.
 * - Gildor Inglorion (Analytical): A High Elf who gives guarded counsel "for friendship’s sake."
 * - Elrond / The Council of Elrond (Institutional): Guides the course of Middle-earth's fate.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(advice_as_dangerous_gift, 0, 10).
narrative_ontology:constraint_claim(advice_as_dangerous_gift, tangled_rope).

% Base extractiveness: 0.2.
% Advice is primarily generative, intended to help the receiver. 
% However, "unguarded" advice extracts autonomy from the seeker, 
% making the giver responsible for the "ill run".
domain_priors:base_extractiveness(advice_as_dangerous_gift, 0.2).

% Suppression score: 0.4.
% Elves "say both no and yes" to avoid suppressing the 
% seeker's own agency. Gildor explicitly notes that 
% unguarded advice is "seldom" given, suppressing the alternative of 
% direct, simple directives.
domain_priors:suppression_score(advice_as_dangerous_gift, 0.4).

% Enforcement: Advice emerges naturally from social interaction/friendship.
domain_priors:emerges_naturally(advice_as_dangerous_gift).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(advice_as_dangerous_gift, the_receiver).
constraint_victim(advice_as_dangerous_gift, the_giver).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: FRODO (THE SEEKER) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Targeted by unknown forces, dependent on others)
   WHEN: immediate (Must decide "to go or wait" right now)
   WHERE: trapped (Gandalf is late, the Elves are guarded)
   
   WHY THIS CLASSIFICATION:
   For Frodo, the advice (or lack thereof) feels like a 'Snare.' If he 
   follows the Elves' "Yes and No," he remains paralyzed. If he follows 
   Gildor's directive and it runs ill, he is trapped by a choice he didn't 
   fully author.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    advice_as_dangerous_gift,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: ELROND / THE COUNCIL OF ELROND - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Guides the course of Middle-earth's fate)
   WHEN: historical (Deliberating long-term strategies for Middle-earth)
   WHERE: arbitrage (Can weigh various counsels and outcomes)
   
   WHY THIS CLASSIFICATION:
   For Elrond and the Council, giving advice is a 'Rope' – a crucial tool to
   guide the course of Middle-earth's fate. They understand its dangers and
   the need for great wisdom in its application, using it to coordinate actions
   against a larger threat.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    advice_as_dangerous_gift,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(continental)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: GANDALF (THE ABSENT GUIDE) - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerful (A wizard whose lateness "does not bode well")
   WHEN: biographical (Operating on a timeframe beyond the Shire)
   WHERE: constrained (His "subtle" affairs dictate his movements)
   
   WHY THIS CLASSIFICATION:
   Gandalf's lateness is a 'Mountain'—a fixed, unchangeable fact that 
   Frodo and Gildor must navigate around. His affairs 
   are a natural law of the current situation that dictates the necessity 
   of the "dangerous gift".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    advice_as_dangerous_gift,
    mountain,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(continental)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(advice_as_dangerous_gift_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, Type3, context(agent_power(individual_powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(advice_as_dangerous_gift_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'Elrond / The Council of Elrond' to
 *    represent the institutional agent. This highlights how advice can be a
 *    tool ('Rope') for high-stakes geopolitical coordination.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Frodo (Snare): Trapped by the burden of choice and uncertainty.
 *    - Elrond (Rope): A tool for strategic guidance.
 *    - Gandalf (Mountain): An immutable, external factor dictating events.
 * 
 * 3. TANGLED ROPE: Advice is a 'Tangled Rope'. It's a 'Rope' when given wisely
 *    and received with agency, guiding to a better path. But it becomes 'Tangled'
 *    when it imposes responsibility on the giver and potentially extracts
 *    autonomy from the receiver if the outcome is negative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the true nature of the wisdom and its impact.
 */

omega_variable(
    advice_as_dangerous_gift_outcome,
    "Do 'all courses run ill' because of the advice given, or because of the inherent complexity of the circumstances (e.g., Gandalf's 'subtle affairs')?",
    resolution_mechanism("Comparative analysis of outcomes for seekers who followed versus rejected various types of counsel in similar high-stakes, uncertain situations."),
    impact("If advice causes ill: 'Snare'. If external factors: 'Mountain' of circumstance."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Guarded Counsel ("Yes and No")
 *    Viability: The standard Elven response; preserves the seeker's agency by not imposing a single path.
 *    Suppression: Gildor breaks this norm because of his "friendship" for Frodo, indicating a situation beyond standard protocols.
 *
 * CONCLUSION:
 * The "Danger" of the gift lies in the suppression of truly neutral alternatives.
 * By giving a directive, Gildor shifts the social constraint from a neutral
 * "Yes/No" 'Mountain' into a directional "Go/Not Alone" 'Rope', taking on some of
 * Frodo's burden of choice.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/advice_as_dangerous_gift].
 * 2. Multi-perspective: ?- multi_index_report(advice_as_dangerous_gift).
 * 3. Run tests: ?- run_tests(advice_as_dangerous_gift_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */