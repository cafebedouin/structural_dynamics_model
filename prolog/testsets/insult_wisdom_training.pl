% ============================================================================
% CONSTRAINT STORY: insult_wisdom_training
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Ward, Benedicta. tr. “The Sayings of the Desert Fathers.”
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_insult_wisdom_training, []).

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
 * * constraint_id: insult_wisdom_training
 * human_readable: The Odd Assignment (Paying for Insults)
 * domain: religious/social/philosophical
 * temporal_scope: Ancient Greece / Perennial
 * spatial_scope: Greece (Athens and its outskirts)
 * * SUMMARY:
 * A disciple is commanded by his Master to pay anyone who insults him for three years as a prerequisite for wisdom. This artificial constraint forces a radical decoupling of ego from social feedback, transforming a galling extraction into a state of "free" resilience where insults no longer have coercive power.
 * * KEY AGENTS:
 * - The Disciple: A "young man" seeking spiritual awakening who undergoes a three-year trial of forced extraction.
 * - The Master: An "ancient and wise" philosopher who sets the extractive constraint to test the student.
 * - The Wise Man at the Gate: A gatekeeper who tests the disciple's final state by providing "free" insults.
 * * NARRATIVE ARC:
 * The disciple experiences three years of a "Snare" where money is extracted and social standing is surrendered. Upon reaching Athens, the constraint is inverted; the realization that insults are now "nothing" (free) reveals the previous extraction as a functional "Rope" for achieving wisdom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(insult_wisdom_interval, 0, 10).
narrative_ontology:constraint_claim([insult_wisdom_training], [ego_liquidation]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.75). The constraint literally extracts financial capital and emotional well-being for a period of three years without an immediate tangible return.
domain_priors:base_extractiveness(insult_wisdom_training, 0.75).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.5). It actively suppresses the natural human "first reaction" to defend oneself.
domain_priors:suppression_score(insult_wisdom_training, 0.5).

% Enforcement requirements
% The constraint is a command from a Master: "He was commanded by his Master...".
domain_priors:requires_active_enforcement(insult_wisdom_training).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(insult_wisdom_training, extractiveness, 0.75).
narrative_ontology:constraint_metric(insult_wisdom_training, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(insult_wisdom_training, spiritual_awakening). % The ultimate goal
constraint_victim(insult_wisdom_training, the_disciple_ego). % The part of the self that feels "ridiculous" or "stupid"

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISCIPLE (DURING TRIAL) - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A student following a command)
   WHEN: immediate (The three-year period of "trial")
   WHERE: trapped (Committed to the path for spiritual awakening)
   SCOPE: local (Immediate social interactions)
   
   WHY THIS CLASSIFICATION:
   During the trial, the disciple sees the constraint as a "Snare." It is "galling" and involves a constant extraction of money and pride. He has no choice but to pay if he wants to achieve his goal.
   
   NARRATIVE EVIDENCE:
   "Every time he was insulted, he gave money... no matter how galling the experience was".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    insult_wisdom_training,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(insult_wisdom_training, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE DISCIPLE (AT THE GATE) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has gained the agency of detachment)
   WHEN: biographical (Looking back at the trial as a career milestone)
   WHERE: mobile (He is ready to "enter the city")
   SCOPE: regional (Athens as a center of wisdom)
   
   WHY THIS CLASSIFICATION:
   Once the trial is over, the disciple realizes the constraint was a "Rope"—a functional tool for coordination between his internal state and external reality. The previous "payment" now serves as a comparative baseline that allows him to laugh at what would previously have been harmful.
   
   NARRATIVE EVIDENCE:
   "for three whole years I have been paying for this kind of thing and now you give it to me for NOTHING!".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    insult_wisdom_training,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MASTER (PHILOSOPHER) - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power of the philosophical tradition)
   WHEN: civilizational (Ancient wisdom being passed down)
   WHERE: analytical (He knows exactly what the training will produce)
   SCOPE: global (Perennial wisdom)
   
   WHY THIS CLASSIFICATION:
   To the Master, the process is a "Mountain"—an unchangeable natural law of spiritual growth. The ego *must* be liquidated through this kind of rigorous, unyielding friction before wisdom can take root.
   
   NARRATIVE EVIDENCE:
   "Now you can go to Athens, for you are ready to learn wisdom".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    insult_wisdom_training,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(insult_wisdom_training_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates the Mandatrophy resolution: The "Snare" becomes a "Rope".
 */
test(multi_perspective_insult) :-
    % Disciple during trial sees Snare
    constraint_indexing:constraint_classification(
        insult_wisdom_training,
        snare,
        context(individual_powerless, immediate, trapped, local)
    ),
    % Disciple at gate sees Rope
    constraint_indexing:constraint_classification(
        insult_wisdom_training,
        rope,
        context(individual_moderate, biographical, mobile, regional)
    ),
    % Master sees Mountain
    constraint_indexing:constraint_classification(
        insult_wisdom_training,
        mountain,
        context(institutional, civilizational, analytical, global)
    ).

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that the Disciple pays the "tax" while the Goal (Wisdom) benefits.
 */
test(power_extractiveness_insult) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextInstitutional = context(institutional, civilizational, analytical, global),
    constraint_indexing:extractiveness_for_agent(insult_wisdom_training, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(insult_wisdom_training, ContextInstitutional, Score2),
    Score1 > Score2.

:- end_tests(insult_wisdom_training_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: The extraction is mandatory and prolonged (3 years). It drains financial resources and forces the subject to endure public ridicule.
 * * 2. MANDATROPHY STATUS:
 * The constraint is high-extraction, but it is "Resolved" because it is a pedagogical tool. Viewing it from the end of the biographical horizon transforms it into a Rope.
 * * 3. PERSPECTIVE SELECTION:
 * The "Before vs After" transition of the Disciple is the core of the insight. The Master represents the "Mountain" of institutional wisdom.
 * * 4. AMBIGUITIES:
 * - The text doesn't specify if the money came from the Master or the Disciple, but the "assignment" implies the Disciple's sacrifice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insult_wisdom_training_extraction_intent,
    "Is the three-year trial a functional necessity for ego-liquidation or an arbitrary hazing ritual by the Master?",
    resolution_mechanism("Audit of long-term student success rates vs. resource-drain severity"),
    impact("If necessity: Mountain of learning. If arbitrary: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Intellectual Study (Monosophical)
 * Viability: Studying books or rhetoric in Athens without the preliminary emotional trial.
 * Suppression: Explicitly rejected by the Master as insufficient; the disciple is only "ready to learn wisdom" *after* the trial.
 * * CONCLUSION:
 * The Master suppresses the "Short Cut" alternative, identifying the trial as an inescapable Mountain. For the student, this suppression is what makes the trial feel like a Snare until the final realization.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/insult_wisdom_training].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(insult_wisdom_training).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
