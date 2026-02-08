% ============================================================================
% CONSTRAINT STORY: quellcrist_falconer_justice
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Richard K. Morgan, "Altered Carbon" (Quellcrist Falconer quote)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(quellcrist_falconer_justice, []).

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
 * * constraint_id: quellcrist_falconer_justice
 * human_readable: The Machinery of Justice (Quellist)
 * domain: political/social
 * temporal_scope: Future (Altered Carbon Universe)
 * spatial_scope: Global/Systemic
 * * SUMMARY:
 * This constraint represents the "Machinery of Justice" as described by Quellcrist Falconer: 
 * a cold, institutional apparatus that serves the "players" (creatures of power) while 
 * systematically liquidating, displacing, and torturing the "little people". 
 * It functions by reframing systemic harm as "just business" or "politics" to suppress 
 * personal accountability.
 * * KEY AGENTS:
 * - The Power Player: Institutional actors who slide under Justice with a "wink and a grin".
 * - The Little People: Powerless subjects who suffer "torture and brutal execution".
 * - The Insurrectionary (Falconer): An analytical observer who advocates for "clawing" justice 
 * through personal, dangerous action.
 * * NARRATIVE ARC:
 * The Machinery of Justice operates on "hardware and soft-" that belongs to the powerful. 
 * It is presented as an immutable "way of the world" (Mountain) to discourage resistance. 
 * Only by becoming "dangerous" (Rope/Insurrection) can a little person transition to a 
 * "player" with whom the machine will make deals.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(quellcrist_falconer_justice, 0, 10).
narrative_ontology:constraint_claim([quellcrist_falconer_justice], [institutional_oppression]).

% Base extractiveness score (0.9 = High)
% Rationale: The system extracts life, freedom, and dignity through "displacement, 
% torture and brutal execution".
domain_priors:base_extractiveness(quellcrist_falconer_justice, 0.9).

% Suppression score (0.8 = High)
% Rationale: Alternatives are suppressed by the "ultimate insult" that systemic 
% harm is "nothing personal" and merely "the way of the world".
domain_priors:suppression_score(quellcrist_falconer_justice, 0.8).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(quellcrist_falconer_justice, extractiveness, 0.9).
narrative_ontology:constraint_metric(quellcrist_falconer_justice, suppression_requirement, 0.8).

% Enforcement: Requires active enforcement
% Rationale: It is maintained by "hardware and soft-" belonging to the power players.
domain_priors:requires_active_enforcement(quellcrist_falconer_justice).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(quellcrist_falconer_justice, power_players).
constraint_victim(quellcrist_falconer_justice, little_people).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE POWER PLAYER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power; hardware and software owners)
   WHEN: historical (Managing the "way of the world" across generations)
   WHERE: arbitrage (Can slide under justice with a "wink and a grin")
   SCOPE: global (Systemic control)
   
   WHY THIS CLASSIFICATION:
   For the powerful, the Machinery of Justice is a "Rope"—a functional coordination 
   mechanism to manage populations, execute policy ("just business"), and 
   "liquidate" obstacles.
   
   NARRATIVE EVIDENCE:
   "Only the little people suffer... the creatures of power slide from under it... 
   it is theirs, hardware and soft-".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    quellcrist_falconer_justice,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE LITTLE PEOPLE - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Subjects who "suffer at the hands of Justice")
   WHEN: immediate (Short-term survival/liquidation)
   WHERE: trapped (Bounded by the "cold" machine)
   SCOPE: local (Immediate harm/displacement)
   
   WHY THIS CLASSIFICATION:
   The little people experience the system as a "Snare." It is a cold, slow 
   apparatus that systematically "creams" their displacement and torture 
   as a necessary "way of the world".
   
   NARRATIVE EVIDENCE:
   "Only the little people suffer... displacement, your torture and brutal 
   execution... it’s a tough life and that it’s nothing personal".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    quellcrist_falconer_justice,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INSURRECTIONARY (Falconer) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the "Machinery" who rejects its neutrality)
   WHEN: civilizational (Identifying the "only difference" in their eyes)
   WHERE: analytical (Unmasking the "hardware and soft-")
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   Falconer views the institutional machine as a "Mountain"—an immutable 
   obstacle that "will not serve you." It is a fixed physical and 
   informational reality ("hardware and soft-") that must be bypassed 
   through personal clawing.
   
   NARRATIVE EVIDENCE:
   "The Machinery of Justice will not serve you here... it is slow and cold, 
   and it is theirs... hardware and soft-".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    quellcrist_falconer_justice,
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

:- begin_tests(quellcrist_falconer_justice_tests).

test(multi_perspective_justice) :-
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, Rope, context(institutional, historical, arbitrage, global)),
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, Snare, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, Mountain, context(analytical, civilizational, analytical, global)),
    Rope \= Snare,
    Snare \= Mountain.

test(mandatrophy_extraction) :-
    domain_priors:base_extractiveness(quellcrist_falconer_justice, E),
    E > 0.7.

test(suppression_via_business) :-
    domain_priors:suppression_score(quellcrist_falconer_justice, S),
    S > 0.7.

:- end_tests(quellcrist_falconer_justice_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.9):
 * The text explicitly describes "liquidation," "torture," and "brutal execution" 
 * of the powerless. This represents the extreme limit of extraction.
 * * 2. SUPPRESSION SCORE (0.8):
 * The system suppresses alternatives by claiming its violence is 
 * "just business" or "politics," stripping the victims of the ability 
 * to even take their own suffering personally.
 * * 3. MANDATROPHY RESOLUTION:
 * The "Rope" (the ability for Players to make deals) is directly 
 * indexed to the "Snare" applied to the Little People. The status 
 * is [RESOLVED MANDATROPHY] because the model proves that the 
 * "Justice" machine exists only to serve one class by harvesting another.
 * * 4. PERSPECTIVE SELECTION:
 * - Power Player (Rope): They slide under it; it is their tool.
 * - Little People (Snare): They suffer under it; it is their executioner.
 * - Falconer (Mountain): She sees it as a fixed, cold obstacle that cannot 
 * be used for good; it must be "clawed".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personal_political_threshold,
    "At what threshold of 'dangerous' behavior does a 'little person' effectively 
     transform into a 'player' in the eyes of the machine?",
    resolution_mechanism("Audit of deal-making patterns vs. casualty rates in personal/insurrectionary actions"),
    impact("If low: Insurrection is a Rope. If high: Insurrection is a suicide-Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    quellcrist_falconer_justice_extraction_intent,
    "Is the cold, slow nature of the machine a functional necessity for galactic governance 
     or a predatory choice for efficient liquidation?",
    resolution_mechanism("Comparison of 'Hardware/Software' allocation for deal-making vs. execution"),
    impact("If necessity: Mountain. If predatory choice: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Institutional Reform
 * Viability: Historically the path for the "Culture" or stable societies.
 * Suppression: Rejected by Falconer because the machine is "theirs, hardware and soft-".
 * * ALTERNATIVE 2: Personal Justice ("Clawing it from them")
 * Viability: The path of being "considered dangerous".
 * Evidence: Marks the "only difference... between players and little people".
 * * CONCLUSION:
 * The text argues that Personal Justice is the only viable Rope, while Institutional 
 * Justice is an inescapable Snare for those who do not make themselves dangerous.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [quellcrist_falconer_justice].
% Multi-perspective: ?- multi_index_report(quellcrist_falconer_justice).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
