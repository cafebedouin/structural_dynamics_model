% ============================================================================
% CONSTRAINT STORY: conversational_dogmas_interruption
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The Church of Interruption: A conversation with a Wizard"
% ============================================================================

:- module(constraint_conversational_dogmas_interruption, []).

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
 * * constraint_id: conversational_dogmas_interruption
 * human_readable: Conversational Dogmas (Interruption vs. Strong Civility)
 * domain: social/technological
 * temporal_scope: Decades (Habits built over lifetimes)
 * spatial_scope: Global (Interpersonal interactions)
 * * SUMMARY:
 * This constraint represents the rigid, often unconscious "dogmas" that govern 
 * human conversation. Specifically, it contrasts the "Church of Interruption" 
 * (COI), which values efficiency and interruptions as signs of understanding, 
 * with the "Church of Strong Civility," which mandates silence and brevity. 
 * Misalignment between these churches leads to severe social friction and 
 * misunderstanding.
 * * KEY AGENTS:
 * - Sam (The COI Member): An individual who interrupts to signal understanding 
 * and speaks until stopped.
 * - The Wizard (Analytical Observer): An "ancient and wise" figure who 
 * identifies the underlying dogmas and their consequences.
 * - The Strong Civilist: An agent who views interruption as a violation of 
 * doctrine and uses physical cues instead of verbal ones.
 * - The Meek: Agents who "shut up tight, like mussels" when interrupted.
 * * NARRATIVE ARC:
 * Sam discovers that their conversational "talking troubles" are not personal 
 * failings but the result of adhering to a "self-consistent" dogma. 
 * When Sam (COI) interacts with a Civilist, their signals for "keep talking" 
 * are read by the other as "SHUT UP ALREADY!". The constraint 
 * functions as a social filter that makes it hard to keep friends outside one's 
 * "church".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(conversational_dogmas_interval, 0, 10).
narrative_ontology:constraint_claim([conversational_dogmas_interruption], [social_governance]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: The extraction is primarily of time, attention, and social 
% harmony. A COI member "muffs" conversations and offends others 
% unintentionally, extracting their patience.
domain_priors:base_extractiveness(conversational_dogmas_interruption, 0.3).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: These habits are built over decades and are "harder... than 
% you think" to change. The alternative church's cues are often completely 
% invisible or misread.
domain_priors:suppression_score(conversational_dogmas_interruption, 0.6).

% Enforcement: Emerges naturally through decades of habit building.
domain_priors:emerges_naturally(conversational_dogmas_interruption).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(conversational_dogmas_interruption, extractiveness, 0.3).
narrative_ontology:constraint_metric(conversational_dogmas_interruption, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
% Beneficiary: Efficiency within a single church (e.g., two COI members 
% converse "happily" and "efficiently").
constraint_beneficiary(conversational_dogmas_interruption, intra_church_coordination).
% Victim: Inter-church relations (ecumenical relations) and the "Meek" 
% who are silenced by the COI.
constraint_victim(conversational_dogmas_interruption, ecumenical_social_harmony).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COI MEMBER (Sam) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has agency within their own social circle)
   WHEN: biographical (Habits built over decades)
   WHERE: mobile (Can "happily converse with anyone else in [their] church")
   SCOPE: local (Immediate interpersonal interaction)
   
   WHY THIS CLASSIFICATION:
   For a member of the Church of Interruption, the dogma is a "Rope"—a highly 
   efficient coordination mechanism. It ensures understanding through immediate 
   feedback and summary, saving time for both parties.
   
   NARRATIVE EVIDENCE:
   "It’s very efficient... if you finish my statement in a way I agree with, 
   I know that you understand".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    conversational_dogmas_interruption,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MEEK / THE CIVILIST - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Silenced or steamrolled by the COI member)
   WHEN: immediate (Short-term conversational frustration)
   WHERE: trapped (Caught in a "harder problem than you think" with no 
   neutral ground)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For those outside the COI, being interrupted is a "Noose." It strangles 
   their expression and makes them "shut up tight." Their own cues for 
   politeness are turned against them, forcing them into silence or 
   offense.
   
   NARRATIVE EVIDENCE:
   "If they meet the COI, they shut up tight, like mussels... Silence is the 
   only knife thin enough to wedge into the shell".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    conversational_dogmas_interruption,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE WIZARD (ANALYTICAL) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Ancient and wise observer)
   WHEN: historical (Viewing decades of built-up habits)
   WHERE: analytical (Observer stance, identifying rigid dogmas)
   SCOPE: global (A universal mapping of human social types)
   
   WHY THIS CLASSIFICATION:
   To the analytical observer, these dogmas are "Mountains." They are 
   "self-consistent" systems that agents are rarely able to leave. Changing 
   them is an "advanced technique" because habits don't die easily.
   
   NARRATIVE EVIDENCE:
   "Conversational habits you’ve been building for decades won’t work 
   anymore... there’s not much neutral ground... habits don’t die easily" 
  .
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    conversational_dogmas_interruption,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(conversational_dogmas_tests).

test(multi_perspective_dogma) :-
    % COI member sees Rope (Efficiency)
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, rope, context(individual_moderate, biographical, mobile, local)),
    % Meek/Civilist sees Noose (Stifling)
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, noose, context(individual_powerless, immediate, trapped, local)),
    % Wizard sees Mountain (Inflexible habit)
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, mountain, context(analytical, historical, analytical, global)).

test(power_extractiveness_dogma) :-
    % The powerless (Meek) experience more extraction (silencing) than the COI member.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, local),
    constraint_indexing:extractiveness_for_agent(conversational_dogmas_interruption, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(conversational_dogmas_interruption, ContextModerate, Score2),
    Score1 > Score2.

test(time_immutability_dogma) :-
    % Short term = habit can be identified (Rope/Noose).
    % Historical/Decades = habit becomes an unchangeable law of personality (Mountain).
    constraint_indexing:effective_immutability(historical, analytical, mountain).

:- end_tests(conversational_dogmas_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.3):
 * Reasoning: Chose low-moderate because while it extracts time and energy, 
 * it is often unintentional. However, for "The Meek," the extraction of 
 * their voice is significant.
 * * 2. PERSPECTIVE SELECTION:
 * Analyzed from the view of the COI member (Efficiency), the victim (Silencing), 
 * and the Wizard (Immutability) to show how conversational style 
 * transforms from a tool to a trap.
 * * 3. CLASSIFICATION RATIONALE:
 * The Wizard's "Mountain" view is critical here; he emphasizes that these 
 * are "dogmas" and "habits don't die easily," suggesting they act as 
 * unchangeable social terrain for most participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conversational_style_plasticity,
    "Can conversational dogmas be truly unlearned (Rope), or are they 
     permanent 'blink of an eye' transformations into Barkers (Mountain)?",
    resolution_mechanism("Long-term behavioral study of agents attempting to switch churches"),
    impact("If plastic: Rope. If fixed: Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    ecumenical_neutral_ground,
    "Is there truly 'not much neutral ground' (Mountain), or can new protocols 
     for ecumenical relations be engineered (Rope)?",
    resolution_mechanism("Development and testing of hybrid conversational doctrines"),
    impact("If engineered: Social Rope. If naturally absent: Systemic Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Turncoat / Hybrid Mastery
 * Viability: Sam wonders if the wizard has "mastered both dogmas".
 * Suppression: Suppressed by the sheer weight of "decades" of habit building.
 * * ALTERNATIVE 2: Neutral Ground / Shared Protocol
 * Viability: Sam asks if they can learn the signs and recognize them.
 * Suppression: Rejected by the Wizard as a "harder problem than you think".
 * * CONCLUSION:
 * The existence of alternative "churches" makes the current one a "Noose" when 
 * interacting outside it. The lack of easy exit options (habit change) 
 * pushes the constraint toward Mountain status for the majority of agents.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into main system: ?- [constraint_conversational_dogmas_interruption].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
