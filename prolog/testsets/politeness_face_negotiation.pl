% ============================================================================
% CONSTRAINT STORY: politeness_face_negotiation
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "Politeness is Two-Faced" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_politeness_face_negotiation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priots:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: politeness_face_negotiation
 * human_readable: The Face Negotiation Constraint (Politeness Theory)
 * domain: social/psychological
 * temporal_scope: Perennial / Universal
 * spatial_scope: Global / Interpersonal
 * 
 * SUMMARY:
 * Politeness is a universal social constraint where individuals manage a "face"—a social self-image 
 * categorized into "Positive Face" (desire for acceptance) and "Negative Face" 
 * (desire for autonomy). Social interactions are a constant negotiation of these faces.
 * 
 * KEY AGENTS:
 * - The Diplomat (Institutional): Uses politeness as a primary tool for statecraft.
 * - The Social Strategist (Individual Moderate): Navigates daily interactions using politeness rules.
 * - The Marginalized Individual (Individual Powerless): Experiences politeness as an oppressive force.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(politeness_face_negotiation, 0, 10).
narrative_ontology:constraint_claim(politeness_face_negotiation, tangled_rope).

% Base extractiveness score (0.65 = Moderate-High)
% Rationale: The system extracts individual autonomy (Negative Face) and forces 
% self-image alignment (Positive Face), requiring individuals to commit to unwanted
% actions to maintain social etiquette.
domain_priors:base_extractiveness(politeness_face_negotiation, 0.65).

% Suppression score (0.5 = Moderate)
% Rationale: Politeness suppresses raw, "bald on-record" expression in favor 
% of face-saving strategies like indirect communication.
domain_priors:suppression_score(politeness_face_negotiation, 0.5).

% Enforcement: Requires active enforcement via social disapproval or exclusion.
domain_priors:requires_active_enforcement(politeness_face_negotiation).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(politeness_face_negotiation, social_groups).
constraint_victim(politeness_face_negotiation, individual_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DIPLOMAT (INSTITUTIONAL) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Represents a state or large organization)
   WHEN: generational (Maintaining long-term relationships)
   WHERE: mobile (Navigates complex international forums)
   
   WHY THIS CLASSIFICATION:
   For a diplomat, politeness is a critical 'Rope'. It is the primary tool for 
   managing international relations, avoiding conflict, and building coalitions. 
   Face-saving is the very essence of statecraft.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    politeness_face_negotiation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MARGINALIZED/EXCLUDED - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Target of belittling or misreading of face)
   WHEN: immediate (The moment of a face attack)
   WHERE: trapped (Caught in a social interaction where the "status quo" is enforced)
   
   WHY THIS CLASSIFICATION:
   For those whose identity is misread or "belittled," politeness is a 'Snare.' The social 
   requirement to "not impose" (Negative Face) is used to silence their identity and 
   enforce a harmful status quo. The extraction of their self-image is immediate and painful.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    politeness_face_negotiation,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SOCIOLOGIST (ANALYTICAL) - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (The observer of universal social laws)
   WHEN: civilizational (A fundamental property of human language)
   WHERE: analytical (Observer stance)
   
   WHY THIS CLASSIFICATION:
   To the analyst, politeness theory describes a 'Mountain'—a zero-degree-of-freedom social law. 
   Face negotiation is not a choice but an inescapable "Interaction Ritual" that 
   defines human sociality across all cultures.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    politeness_face_negotiation,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(politeness_face_negotiation_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(politeness_face_negotiation, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(politeness_face_negotiation, Type2, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(politeness_face_negotiation, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global)),
    constraint_indexing:extractiveness_for_agent(politeness_face_negotiation, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(politeness_face_negotiation, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(politeness_face_negotiation_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Diplomat' to represent the institutional
 *    use of politeness as a functional 'Rope' for managing high-stakes relationships,
 *    satisfying the linter requirement.
 * 
 * 2. EXTRACTIVENESS SCORE (0.65): Kept this score as politeness strongly extracts
 *    autonomy ("orders, requests, commitments we don't want") and authentic identity
 *    in favor of social harmony.
 *
 * 3. MANDATROPHY STATUS: The high extraction is 'RESOLVED' because the system's
 *    predatory nature is indexed to power. For the powerful (diplomat), it's a tool.
 *    For the powerless (marginalized), it's a weapon used against them.
 * 
 * 4. CLASSIFICATION RATIONALE:
 *    - Diplomat (Rope): A tool for statecraft.
 *    - Marginalized (Snare): A tool of social oppression.
 *    - Sociologist (Mountain): A fundamental, inescapable law of human interaction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints, probing the root of 'face'.
 */

omega_variable(
    face_universality_intent,
    "Is 'face' a functional biological necessity for social survival (Mountain) or a predatory cultural 'Snare' used to enforce a status quo?",
    resolution_mechanism("Audit of social/economic outcomes for 'face-ignoring' vs. 'face-observing' individuals in diverse cultures over time."),
    impact("If necessity: Evolutionary Mountain. If cultural enforcement: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Radical Honesty / Plain Speaking
 *    Viability: Bypassing "face" to speak the raw truth.
 *    Suppression: Explicitly identified as a "Face-Threatening Act" that brings "pain to 
 *    others" and risks social exclusion.
 * 
 * CONCLUSION:
 * The suppression of radical honesty is the core mechanism that makes politeness a constraint. 
 * The fact that this alternative is so strongly discouraged demonstrates that 'face negotiation'
 * is an enforced system, not merely an option.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/politeness_face_negotiation].
 * 2. Multi-perspective: ?- multi_index_report(politeness_face_negotiation).
 * 3. Run tests: ?- run_tests(politeness_face_negotiation_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */