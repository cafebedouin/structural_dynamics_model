% ============================================================================
% CONSTRAINT STORY: poetic_verse_and_past
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
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
 * * constraint_id: poetic_verse_and_past
 * human_readable: The Inescapable Tale (Keating’s Verse vs. Chiang’s Past)
 * domain: social/philosophical
 * temporal_scope: Biographical to Historical
 * spatial_scope: Institutional (The School) to Global (The Human Race)
 * * SUMMARY:
 * This constraint defines human existence as a "powerful play" where individuals are 
 * both players and audience, compelled to contribute a "verse". While 
 * institutional forces seek to measure and limit life to "noble pursuits" like medicine 
 * and law, the underlying constraint is the past itself, which cannot be erased but only 
 * processed through atonement and forgiveness.
 * * KEY AGENTS:
 * - John Keating: The catalyst who views life as a "Rope" for Savannah transformation.
 * - The Students (e.g., Todd, Neil): Individual subjects caught in the "Noose" of 
 * institutional expectations and personal choice.
 * - The Father (Mr. Perry): An institutional agent who enforces a rigid path, 
 * treating life as an unchangeable Mountain of duty.
 * - The Aged Observer (Chiang/Keating late-life): The analytical agent who realizes 
 * the past is a Mountain that erases windows of opportunity.
 * * NARRATIVE ARC:
 * The constraint moves from the "possibilities" of youth (drunk on the future) to the 
 * "glacial shifts" of age where windows close and only forgiveness remains. 
 * Neil Perry’s suicide marks the ultimate extraction point of the constraint, where 
 * a "Noose" of parental will meets the "Mountain" of a failed path.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(poetic_verse_interval, 0, 10).
narrative_ontology:constraint_claim([poetic_verse_and_past], [existential_governance]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.75). The constraint extracts "hearts and souls" and, in Neil's 
% case, the totality of life. Institutions extract identity to sustain 
% "medicine, law, and business".
domain_priors:base_extractiveness(poetic_verse_and_past, 0.75).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate-High (0.6). The "Pritchard" method of measuring poetry 
% suppresses idiosyncratic "savoring" of language. Parental authority 
% actively punishes alternatives to the med/law path.
domain_priors:suppression_score(poetic_verse_and_past, 0.6).

% Enforcement requirements
% Requires active enforcement (Institutional boards, rigid parenting, and desk-standing 
% as a sign of resistance).
domain_priors:requires_active_enforcement(poetic_verse_and_past).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(poetic_verse_and_past, extractiveness, 0.75).
narrative_ontology:constraint_metric(poetic_verse_and_past, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(poetic_verse_and_past, institutional_order). % The school and society "sustained" by medicine/law
constraint_victim(poetic_verse_and_past, neil_perry). % Suicide as terminal extraction
constraint_victim(poetic_verse_and_past, individual_identity). % The "casualties" of the battle

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ROMANTIC TEACHER (Keating) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A teacher "beginning his career" with the power to inspire)
   WHEN: biographical (Changing lives through सावoring words)
   WHERE: mobile (Believing ideas can "change the world")
   SCOPE: global (Contributing to the "Powerful Play" of the human race)
   
   WHY THIS CLASSIFICATION:
   To Keating, poetry and life are a "Rope"—a tool for coordination that allows 
   students to "think for yourselves again" and contribute their own verse.
   
   NARRATIVE EVIDENCE:
   "That the powerful play goes on and you may contribute a verse".
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SUBJECTED STUDENT (Neil) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A student whose casualties are his "heart and soul")
   WHEN: immediate (Short-term battle of expulsion and desk-standing)
   WHERE: trapped (Paternal will leaves "no escape from our roles")
   SCOPE: local (The domestic and school sphere)
   
   WHY THIS CLASSIFICATION:
   For Neil, the constraint of his father’s path is a "Noose." He cannot "peel 
   off" from the lineage and eventually finds that cutting a new path leads to 
   terminal disaster.
   
   NARRATIVE EVIDENCE:
   "Sometimes cutting a new path leads to disaster... CASUALTIES could be 
   your hearts and souls".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    poetic_verse_and_past,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE AGED OBSERVER (Chiang) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Viewing life as a "tale Allah tells")
   WHEN: civilizational/historical (Beyond the biographical drift)
   WHERE: analytical (Observer stance: "We are the audience as well as the players")
   SCOPE: global (The human condition)
   
   WHY THIS CLASSIFICATION:
   To the observer of the "past," life is a "Mountain." Nothing erases it; 
   windows close, and we are left only with the unchangeable tale.
   
   NARRATIVE EVIDENCE:
   "Nothing erases the past... There is no escape from our roles".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    poetic_verse_and_past,
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

:- begin_tests(poetic_verse_and_past_tests).

test(multi_perspective_verse) :-
    % Keating sees Rope (Contribution)
    constraint_indexing:constraint_classification(poetic_verse_and_past, rope, context(individual_moderate, _, _, _)),
    % Neil sees Noose (Liquidation)
    constraint_indexing:constraint_classification(poetic_verse_and_past, noose, context(individual_powerless, _, _, _)),
    % Chiang sees Mountain (Immutability)
    constraint_indexing:constraint_classification(poetic_verse_and_past, mountain, context(analytical, _, _, _)).

test(power_extractiveness_verse) :-
    % The powerless student experiences terminal extraction (Noose)
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    % The institutional observer sees a necessary sustainment
    ContextInstitutional = context(institutional, generational, mobile, global),
    constraint_indexing:extractiveness_for_agent(poetic_verse_and_past, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(poetic_verse_and_past, ContextInstitutional, E2),
    E1 > E2.

test(time_immutability_past) :-
    % Over biographical time, the "verse" is a Rope (changeable)
    constraint_indexing:effective_immutability(biographical, mobile, rope),
    % Over historical time, the past is a Mountain (unchangeable)
    constraint_indexing:effective_immutability(historical, analytical, mountain).

:- end_tests(poetic_verse_and_past_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: The narrative centers on a "battle" where "casualties" are literal 
 * (Neil's suicide) and figurative (the death of passion/soul). This 
 * qualifies as high-asymmetry extraction of life-potential for institutional stability.
 * * 2. SUPPRESSION SCORE (0.6):
 * Reasoning: Institutions like the "Pritchard method" suppress individual expression 
 * in favor of quantifiable academic measurement.
 * * 3. MANDATROPHY RESOLUTION:
 * Status: RESOLVED MANDATROPHY. The system is a Rope for Keating (the catalyst) 
 * and for intra-church coordination among students, but a Noose for Neil, whose 
 * paternal "Mountain" is built upon Neil's liquidation.
 * * 4. AMBIGUITIES:
 * - The text asks if Mr. Hopkins was "an advanced student" or simply resistant. 
 * I have categorized him under the "Rope" of Intra-Church coordination.
 * * 5. CONFIDENCE:
 * High: Classifications of Keating vs. Chiang (Rope vs. Mountain).
 * Medium: Extractiveness (varies by agent, though terminal for Neil).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    keating_life_transformation,
    "Does the students' gesture of standing on desks truly transform Keating’s life, or is it a brief Rope that snaps into a Noose of expulsion?",
    resolution_mechanism("Biographical tracking of Keating's career post-expulsion"),
    impact("If transformative: It remains a Rope of contribution. If destructive: It is a Noose of casualty."),
    confidence_without_resolution(medium)
).

omega_variable(
    perry_redemption_intent,
    "Does Mr. Perry ever recognize his role and find 'small measure of forgiveness,' or is his bitterness an immutable Mountain?",
    resolution_mechanism("Longitudinal psychiatric audit of parental grief in high-conformity cultures"),
    impact("If redemption: Forgiveness is a Rope. If bitterness: It is a terminal Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The J. Evans Pritchard Method
 * Viability: Quantifiable, "academic" measurement of poetry.
 * Suppression: Explicitly "ripped out" and shunted by Keating as a threat to identity.
 * Evidence: "Armies of academics going forward measuring poetry. No!".
 * * ALTERNATIVE 2: Quiet Conformity
 * Viability: Studying Mr. Pritchard to "go quietly about our business".
 * Suppression: Rejected by the students in the final "O Captain! My Captain!" scene.
 * * CONCLUSION:
 * The presence of Alternative 1 (The Pritchard Noose) makes Keating’s Verse a 
 * "Rope" of agency. However, the systemic "Mountain" of the father's will 
 * ultimately liquidates the alternative of Neil’s acting career.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_poetic_verse_and_past].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
