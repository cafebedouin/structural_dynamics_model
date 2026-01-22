% ============================================================================
% CONSTRAINT STORY: advice_as_dangerous_gift
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
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
 * * constraint_id: advice_as_dangerous_gift
 * human_readable: The Hazard of Counsel
 * domain: social/philosophical
 * temporal_scope: High Fantasy Third Age / R7
 * spatial_scope: The Shire / Middle-earth
 * * SUMMARY:
 * Advice is framed as a "dangerous gift" because it involves one agent 
 * attempting to choose a path for another without full context. 
 * Because "all courses may run ill," the act of giving advice creates a 
 * systemic risk for both the giver (responsibility) and the receiver (error).
 * * KEY AGENTS:
 * - Gildor Inglorion: A High Elf who gives guarded counsel "for friendship’s sake".
 * - Frodo Baggins: The seeker of advice, caught between the "subtle" affairs of wizards and his own peril.
 * - Gandalf: The absent authority whose lateness creates the vacuum that advice must fill.
 * * NARRATIVE ARC:
 * Frodo is trapped by Gandalf's lateness and seeks a "Rope" (direction) from the Elves. 
 * Gildor warns that such Ropes are often "Nooses" in disguise because they cannot 
 * account for the hidden variables of the seeker's soul. 
 * He eventually provides a specific directive—to go at once and not alone—but 
 * does so with "caution".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(advice_as_dangerous_gift_interval, 0, 10).
narrative_ontology:constraint_claim([advice_as_dangerous_gift], [social_hazard]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Advice is primarily generative, intended to help the receiver. 
% However, "unguarded" advice extracts autonomy from the seeker, 
% making the giver responsible for the "ill run".
domain_priors:base_extractiveness(advice_as_dangerous_gift, 0.2).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Elves "say both no and yes" to avoid suppressing the 
% seeker's own agency. Gildor explicitly notes that 
% unguarded advice is "seldom" given, suppressing the alternative of 
% direct, simple directives.
domain_priors:suppression_score(advice_as_dangerous_gift, 0.4).

% Enforcement requirements
% Advice emerges naturally from social interaction/friendship.
domain_priors:emerges_naturally(advice_as_dangerous_gift).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(advice_as_dangerous_gift, extractiveness, 0.2).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(advice_as_dangerous_gift, the_receiver). % Receives path-finding data
constraint_victim(advice_as_dangerous_gift, the_giver).      % Assumes moral risk/responsibility

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: GILDOR (THE ELVEN COUNSEL) - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (A wise observer with historical perspective)
   WHEN: historical (Aware of the "subtle and quick to anger" nature of wizards)
   WHERE: arbitrage (Can see multiple courses, even if they run ill)
   SCOPE: regional (Knowledge of Middle-earth events)
   
   WHY THIS CLASSIFICATION:
   To Gildor, advice is a "Rope"—a fragile tool to coordinate Frodo's 
   immediate survival. He understands its limitations and only "demands" it 
   be used if the seeker acknowledges the risk.
   
   NARRATIVE EVIDENCE:
   "I will for friendship’s sake give it... I think you should now go at once... 
   and if Gandalf does not come... do not go alone".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    advice_as_dangerous_gift,
    rope,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: FRODO (THE SEEKER) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Targeted by unknown forces, dependent on others)
   WHEN: immediate (Must decide "to go or wait" right now)
   WHERE: trapped (Gandalf is late, the Elves are guarded)
   SCOPE: local (The immediate environment of the Shire)
   
   WHY THIS CLASSIFICATION:
   For Frodo, the advice (or lack thereof) feels like a "Noose." If he 
   follows the Elves' "Yes and No," he remains paralyzed. If he follows 
   Gildor's directive and it runs ill, he is trapped by a choice he didn't 
   fully author.
   
   NARRATIVE EVIDENCE:
   "Go not to the Elves for counsel, for they will say both no and yes".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    advice_as_dangerous_gift,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: GANDALF (THE ABSENT GUIDE) - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerful (A wizard whose lateness "does not bode well")
   WHEN: biographical (Operating on a timeframe beyond the Shire)
   WHERE: constrained (His "subtle" affairs dictate his movements)
   SCOPE: continental (Middle-earth)
   
   WHY THIS CLASSIFICATION:
   Gandalf's lateness is a "Mountain"—a fixed, unchangeable fact that 
   Frodo and Gildor must navigate around. His affairs 
   are a natural law of the current situation that dictates the necessity 
   of the "dangerous gift".
   
   NARRATIVE EVIDENCE:
   "That Gandalf should be late, does not bode well... Do not meddle in the 
   affairs of wizards, for they are subtle".
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
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(advice_as_dangerous_gift_tests).

test(multi_perspective_counsel) :-
    % Gildor (Analytical) sees a Rope
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, rope, context(analytical, historical, arbitrage, regional)),
    % Frodo (Powerless) sees a Noose
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, noose, context(individual_powerless, immediate, trapped, local)),
    % Gandalf's absence (Powerful) is a Mountain
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, mountain, context(individual_powerful, biographical, constrained, continental)).

test(wisdom_hazard_scaling) :-
    % Unguarded advice (low suppression of directives) increases the "danger" (extractiveness) 
    % because the giver assumes more responsibility for the outcome.
    domain_priors:base_extractiveness(advice_as_dangerous_gift, E),
    E > 0.1.

test(time_immutability_counsel) :-
    % Short-term desperation (immediate) makes advice feel like a Noose/Mountain.
    % Historical wisdom (historical) sees it as a Rope (coordination).
    constraint_indexing:effective_immutability(historical, arbitrage, rope).

:- end_tests(advice_as_dangerous_gift_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.2):
 * Reasoning: Advice is technically a gift, but "danger" implies a cost. 
 * The extraction here is psychological: the burden of choice for the 
 * giver and the potential loss of autonomy for the receiver.
 * * 2. SUPPRESSION SCORE (0.4):
 * Reasoning: High-Elven social norms suppress "unguarded" advice. The 
 * "Yes and No" tradition is a systemic suppression of definitive 
 * directives to protect the seeker's agency.
 * * 3. PERSPECTIVE SELECTION:
 * Chose Frodo (Powerless), Gildor (Wise/Analytical), and Gandalf (Absent/Powerful). 
 * This highlights how the same act of counsel is a lifeline (Rope), 
 * a trap (Noose), and a response to an unchangeable absence (Mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    advice_as_dangerous_gift_outcome,
    "Do 'all courses run ill' because of the advice given, or because of the 
     inherent complexity of the wizard's affairs?",
    resolution_mechanism("Comparative analysis of outcomes for seekers who followed vs. rejected Elven counsel"),
    impact("If advice causes ill: Noose. If affairs cause ill: Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    gandalf_lateness_intent,
    "Was Gandalf's lateness an unavoidable Mountain (physical delay) or a 
     Rope (a strategic choice to force Frodo's growth)?",
    resolution_mechanism("Access to Gandalf's internal records or future accounts of the delay"),
    impact("If strategic: Gandalf is an Institutional beneficiary. If accidental: He is a victim of the Churn."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Guarded Counsel ("Yes and No")
 * Viability: The standard Elven response; preserves the seeker's agency.
 * Suppression: Gildor breaks this norm because of his "friendship" for Frodo.
 * * ALTERNATIVE 2: Total Self-Reliance (Wait for Gandalf)
 * Viability: Frodo's initial instinct to wait for the primary guide.
 * Suppression: Rejected by Gildor because "Gandalf being late does not bode well".
 * * CONCLUSION:
 * The "Danger" of the gift lies in the suppression of Alternative 1. By giving 
 * a directive, Gildor shifts the social constraint from a neutral "Yes/No" 
 * Mountain into a directional "Go/Not Alone" Rope.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_advice_as_dangerous_gift].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
