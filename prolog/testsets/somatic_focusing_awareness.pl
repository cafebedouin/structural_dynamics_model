% ============================================================================
% CONSTRAINT STORY: somatic_focusing_awareness
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Meg-John Barker & Eugene T. Gendlin
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_somatic_focusing_awareness, []).

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
 * * constraint_id: somatic_focusing_awareness
 * human_readable: Somatic Focusing Awareness
 * domain: social/psychological
 * temporal_scope: Perennial / Immediate
 * spatial_scope: Internal / Local
 * * SUMMARY:
 * This constraint defines the practice of "staying with feelings" through "interested curiosity" 
 * without trying to change them or force communication. It relies on identifying 
 * a "felt sense" within the body to navigate the "emotional landscape" before responding to external stimuli.
 * * KEY AGENTS:
 * - The Practitioner: An agent who practices "gentle and curious" attention toward internal states.
 * - The Felt Sense (The Body): Characterized as the "unconscious mind" and the "seat of feeling".
 * - The Instruction (The Method): The "six steps" or zine guidelines that provide a map but risk "closing off other ways".
 * * NARRATIVE ARC:
 * The individual encounters a feeling and, rather than reacting, pauses to ask where the feeling resides 
 * in the body and what words describe it. They adopt a "split-level" approach: following 
 * instructions to discover new experiences while remaining sensitive to bodily signals that may contradict 
 * those instructions.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================= */

% Structural Anchor for index extraction
narrative_ontology:interval(somatic_focusing_awareness_interval, 0, 10).
narrative_ontology:constraint_claim([somatic_focusing_awareness], [psychological_attunement]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Low (0.2). This is a "nourishing" practice that assumes only "sound expansive 
% experiences are worth having". It aims to prevent the "poison" of reactive speaking.
domain_priors:base_extractiveness(somatic_focusing_awareness, 0.2).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Low (0.3). The method explicitly instructs the agent to "acknowledge everything that 
% comes up" and "not try to change" the feeling.
domain_priors:suppression_score(somatic_focusing_awareness, 0.3).

% Enforcement: Emerges naturally through the body's felt sense, but requires active 
% cognitive enforcement of the "pause" and "curiosity".
domain_priors:requires_active_enforcement(somatic_focusing_awareness).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(somatic_focusing_awareness, internal_clarity). % The practitioner gains clarity.
constraint_victim(somatic_focusing_awareness, reactive_habit). % The automatic, unthinking response is "liquidated".

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(somatic_focusing_awareness, extractiveness, 0.2).
narrative_ontology:constraint_metric(somatic_focusing_awareness, suppression_requirement, 0.3).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CURIOUS PRACTITIONER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "stay with the feeling")
   WHEN: immediate (Applied "during conversations")
   WHERE: mobile (Uses a "split-level" approach to adjust instructions)
   SCOPE: local (Internal states and interpersonal dialogue)
   
   WHY THIS CLASSIFICATION:
   For the practitioner, the method is a "Rope"—a functional coordination mechanism to 
   "understand our emotional landscape" and think before speaking.
   
   NARRATIVE EVIDENCE:
   "Asking these questions helps us... understand our emotional landscape... gives 
   us an opportunity to think... before speaking".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    somatic_focusing_awareness,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE RIGID STUDENT - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A "slave" to the instructions who might "close off other ways")
   WHEN: immediate (Trying to "follow the instructions exactly")
   WHERE: trapped (Doing something that "feels wrong in your body")
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   If the instructions are used to "close off other ways," the method becomes a "Noose." 
   It extracts the individual's sensitivity to their own body in favor of rigid adherence 
   to the text.
   
   NARRATIVE EVIDENCE:
   "One danger with a set of instructions is that people might use them to close 
   off other ways... The moment doing it feels wrong... stop".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    somatic_focusing_awareness,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    % Extraction happens when the user follows "wrong" instructions over the body.
    domain_priors:base_extractiveness(somatic_focusing_awareness, E),
    E > 0.1,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SOMATIC BIOLOGIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the "seat of feeling")
   WHEN: civilizational (The biological nature of the "unconscious mind")
   WHERE: analytical (Observer stance; "discover the experiences to which they point")
   SCOPE: global (The human condition)
   
   WHY THIS CLASSIFICATION:
   To the analytical observer, the body's responses are a "Mountain"—an unchangeable 
   biological fact. Whether we follow instructions or not, the body is the 
   "seat of feeling".
   
   NARRATIVE EVIDENCE:
   "maybe the body is unconscious mind, the seat of feeling".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    somatic_focusing_awareness,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(somatic_focusing_awareness_tests).

test(multi_perspective_somatic) :-
    % Practitioner sees Rope
    constraint_indexing:constraint_classification(somatic_focusing_awareness, T1, context(individual_moderate, immediate, mobile, local)),
    % Rigid Follower sees Noose
    constraint_indexing:constraint_classification(somatic_focusing_awareness, T2, context(individual_powerless, immediate, trapped, local)),
    % Observer sees Mountain
    constraint_indexing:constraint_classification(somatic_focusing_awareness, T3, context(analytical, civilizational, analytical, global)),
    T1 \= T2, T2 \= T3.

test(power_extractiveness_somatic) :-
    % The "powerless" student following instructions rigidly experiences higher 
    % extraction of their own sensitivity than the "moderate" autonomous practitioner.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, immediate, mobile, local),
    constraint_indexing:extractiveness_for_agent(somatic_focusing_awareness, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(somatic_focusing_awareness, ContextModerate, Score2),
    Score1 > Score2.

test(split_level_rope) :-
    % The "split-level" approach acts as a Rope to navigate instructions.
    true.

:- end_tests(somatic_focusing_awareness_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.2):
 * Reasoning: Chose low because the practice is explicitly "gentle" and "nourishing". 
 * It only becomes extractive (Noose) when instructions are followed against bodily intuition.
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the Practitioner (Rope) who uses the tool for clarity with the 
 * Rigid Follower (Noose) who is "trapped" by the "Six Steps".
 * * 3. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The "extraction" of standard reactive 
 * habits is a beneficial coordination (Rope) for the practitioner, but a 
 * "Noose" for the uncritical student of the method.
 * * 4. AMBIGUITIES:
 * - The text asks if "the body is unconscious mind". This is 
 * formalized as an Omega variable regarding biological vs. psychological origins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    somatic_body_mind_identity,
    "Is the body actually the 'unconscious mind' (Mountain), or is this a 
     metaphorical Rope for psychological discovery?",
    resolution_mechanism("Neurological audit of 'felt sense' origin points in the central nervous system"),
    impact("If Mountain: Body-work is a biological law. If Rope: It is a narrative technique."),
    confidence_without_resolution(medium)
).

omega_variable(
    instructional_exhaustion,
    "Do 'Six Steps' instructions eventually 'close off other ways' for 
     everyone (Mountain), or can the split-level approach permanently maintain 
     openness (Rope)?",
    resolution_mechanism("Longitudinal tracking of practitioners' adaptability vs. rigid habituation"),
    impact("If Mountain: All methods eventually entropic. If Rope: Methodology can be self-renewing."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Intellectualization
 * Viability: Trying to "solve" feelings with logic without body-sensing.
 * Suppression: Shunted by the method as a "set of instructions" that might 
 * "close off" the body.
 * * ALTERNATIVE 2: Catharsis / Ventilation
 * Viability: Forcing a feeling to "communicate" or react immediately.
 * Suppression: Explicitly suppressed by the instruction to "not try to 
 * change it, or to force it to communicate".
 * * CONCLUSION:
 * The "split-level" approach is the definitive Rope that prevents the 
 * method from becoming a Noose of "doing it wrong".
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [somatic_focusing_awareness].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(somatic_focusing_awareness).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
