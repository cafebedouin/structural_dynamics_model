% ============================================================================
% CONSTRAINT STORY: poetic_verse_and_past
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "Dead Poet’s Society: Past, Future & Forgiveness" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_poetic_verse_and_past, []).

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
 * constraint_id: poetic_verse_and_past
 * human_readable: The Inescapable Tale (Keating’s Verse vs. Chiang’s Past)
 * domain: social/philosophical/cultural
 * temporal_scope: Biographical to Historical
 * spatial_scope: Institutional (The School) to Global (The Human Race)
 * 
 * SUMMARY:
 * This constraint defines human existence as a "powerful play" where individuals are 
 * both players and audience, compelled to contribute a "verse". While 
 * institutional forces seek to measure and limit life to "noble pursuits" like medicine 
 * and law, the underlying constraint is the past itself, which cannot be erased but only 
 * processed through atonement and forgiveness.
 * 
 * KEY AGENTS:
 * - John Keating (Individual Moderate): The catalyst who views life as a "Rope" for self-transformation.
 * - The School Administration (Institutional): Enforces a rigid path of "noble pursuits".
 * - The Students (Individual Powerless): Caught in the "Snare" of institutional expectations.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(poetic_verse_and_past, 0, 10).
narrative_ontology:constraint_claim(poetic_verse_and_past, tangled_rope).

% Base extractiveness: 0.75.
% The constraint extracts "hearts and souls" and, in Neil's 
% case, the totality of life. Institutions extract identity to sustain 
% "medicine, law, and business".
domain_priors:base_extractiveness(poetic_verse_and_past, 0.75).

% Suppression score: 0.6.
% The "Pritchard" method of measuring poetry 
% suppresses idiosyncratic "savoring" of language. Parental authority 
% actively punishes alternatives to the med/law path.
domain_priors:suppression_score(poetic_verse_and_past, 0.6).

% Enforcement: Requires active enforcement (Institutional boards, rigid parenting).
domain_priors:requires_active_enforcement(poetic_verse_and_past).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(poetic_verse_and_past, institutional_order).
constraint_victim(poetic_verse_and_past, individual_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SUBJECTED STUDENT (NEIL) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (A student whose casualties are his "heart and soul")
   WHEN: immediate (Short-term battle of expulsion and desk-standing)
   WHERE: trapped (Paternal will leaves "no escape from our roles")
   
   WHY THIS CLASSIFICATION:
   For Neil, the constraint of his father’s path is a 'Snare'. He cannot "peel 
   off" from the lineage and eventually finds that cutting a new path leads to 
   terminal disaster, strangling his individual identity and aspirations.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    poetic_verse_and_past,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SCHOOL ADMINISTRATION - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Enforces a rigid path of "noble pursuits")
   WHEN: historical (Maintaining the school's legacy and reputation)
   WHERE: constrained (Bound by tradition and parental expectations)
   
   WHY THIS CLASSIFICATION:
   For the school administration, the curriculum and rules are a 'Rope' for
   maintaining order and academic standards. It's a tool to coordinate the
   student body towards "noble pursuits" like medicine and law, upholding the
   institution's values and ensuring its survival.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    poetic_verse_and_past,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ROMANTIC TEACHER (KEATING) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (A teacher "beginning his career" with the power to inspire)
   WHEN: biographical (Changing lives through savoring words)
   WHERE: mobile (Believing ideas can "change the world")
   
   WHY THIS CLASSIFICATION:
   To Keating, poetry and life are a 'Rope'—a tool for coordination that allows 
   students to "think for yourselves again" and contribute their own verse. It's a
   means to break free from the constraints of conformity and embrace individuality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    poetic_verse_and_past,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(poetic_verse_and_past_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(poetic_verse_and_past, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(poetic_verse_and_past, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(poetic_verse_and_past, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(poetic_verse_and_past_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The School Administration' as the
 *    institutional agent. For them, the established curriculum is a 'Rope'
 *    for maintaining order and institutional legacy.
 *
 * 2. MANDATROPHY STATUS: High extraction (0.75) is 'RESOLVED' by recognizing
 *    the stark perspectival conflict. The institutional 'Rope' of conformity
 *    becomes a lethal 'Snare' for individuals who cannot conform (Neil).
 *
 * 3. CLASSIFICATION RATIONALE:
 *    - Neil (Snare): Trapped by paternal and institutional expectations.
 *    - School (Rope): Upholding tradition and order.
 *    - Keating (Rope): Inspiring individuality and self-expression.
 * 
 * 4. CORE INSIGHT: The story illustrates a tragic 'Tangled Rope' where the
 *    institutional 'Rope' of order and tradition directly creates a 'Snare'
 *    for individual expression, leading to catastrophic consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    keating_life_transformation,
    "Does the students' gesture of standing on desks truly transform Keating’s life, or is it a brief 'Rope' of validation that ultimately snaps into a 'Snare' of professional ruin?",
    resolution_mechanism("Biographical tracking of Keating's career post-expulsion; analysis of his long-term impact on former students."),
    impact("If transformative: It remains a 'Rope' of contribution. If destructive: It is a 'Snare' of casualty."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: The J. Evans Pritchard Method
 *    Viability: Quantifiable, "academic" measurement of poetry that aligns with institutional goals.
 *    Suppression: Explicitly "ripped out" and shunted by Keating as a threat to authentic expression and identity.
 *
 * CONCLUSION:
 * Keating's rejection of the "Pritchard Snare" creates a 'Rope' of agency for
 * his students. However, this 'Rope' is in direct conflict with the institutional
 * 'Rope' of conformity, ultimately leading to Neil's tragic entrapment between
 * two irreconcilable paths.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/poetic_verse_and_past].
 * 2. Multi-perspective: ?- multi_index_report(poetic_verse_and_past).
 * 3. Run tests: ?- run_tests(poetic_verse_and_past_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */