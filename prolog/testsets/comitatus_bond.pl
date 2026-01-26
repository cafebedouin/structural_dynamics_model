% ============================================================================
% CONSTRAINT STORY: comitatus_bond
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Beowulf (Epic Poem)
% ============================================================================

:- module(constraint_comitatus_bond, []).

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
 * * constraint_id: comitatus_bond
 * human_readable: The Germanic Comitatus Code
 * domain: social/political
 * temporal_scope: Migration Period / Early Middle Ages (c. 5th-6th Century)
 * spatial_scope: Northern Europe (Denmark/Sweden)
 * * SUMMARY:
 * The comitatus is the foundational socio-political constraint governing the relationship 
 * between a lord (gold-friend) and his thanes. It mandates absolute loyalty, the 
 * distribution of spoils for service, and the blood-feud obligation. To break it 
 * [cite_start]is to become "wrackful" (exiled), a fate often described as worse than death. [cite: 1]
 * * KEY AGENTS:
 * - [cite_start]Hrothgar: The "Ring-giver," an institutional figure who maintains the bond through generosity. [cite: 1]
 * - [cite_start]Beowulf: The idealized Thane, whose power is moderate but grows to institutional as a king. [cite: 1]
 * - [cite_start]Wiglaf: The loyal youth who illustrates the bond's binding nature during Beowulf's final fight. [cite: 1]
 * - [cite_start]The Cowardly Thanes: Those who flee, illustrating the "Snare" of social exile. [cite: 1]
 * * NARRATIVE ARC:
 * The poem opens with the establishment of Hrothgar's hall, Heorot, the physical manifestation 
 * of the comitatus. The constraint drives Beowulf's journey to help Hrothgar (repaying a 
 * debt of his father) and culminates in Wiglaf's refusal to abandon Beowulf against the 
 * [cite_start]dragon, even when all others have fled. [cite: 1]
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(comitatus_era, 0, 10).
narrative_ontology:constraint_claim(comitatus_bond, rope).

% Base extractiveness score
% Rationale: Moderate. While the lord extracts life-risking labor, he 
% provides protection, land, and gold. [cite_start]It is a functional coordination. [cite: 1]
domain_priors:base_extractiveness(comitatus_bond, 0.4).

% Suppression score
% Rationale: High. Alternatives to the bond are practically invisible. 
% To be without a lord is to be a "wanderer" or a "wraith." [cite: 1]
domain_priors:suppression_score(comitatus_bond, 0.8).

% Enforcement requirements
% Requires active enforcement (gold-giving) and cultural transmission. [cite: 1]
domain_priors:requires_active_enforcement(comitatus_bond).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(comitatus_bond, extractiveness, 0.4).
narrative_ontology:constraint_metric(comitatus_bond, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(comitatus_bond, tribal_stability).
constraint_victim(comitatus_bond, [unferth, cowardly_thanes]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: WIGLAF - Mountain
   --------------------------------------------------------------------------
   
   [cite_start]WHO: individual_powerless - A young, unproven thane. [cite: 1]
   [cite_start]WHEN: biographical - The code defines his entire identity and future. [cite: 1]
   [cite_start]WHERE: trapped - Desertion means social death; there is no "exit." [cite: 1]
   [cite_start]SCOPE: local - Focused on the immediate duty in the dragon's lair. [cite: 1]
   
   WHY THIS CLASSIFICATION:
   For Wiglaf, the code is an unchangeable law. He cannot conceive of 
   survival outside the bond. [cite_start]It is the "Mountain" that dictates his reality. [cite: 1]
   
   NARRATIVE EVIDENCE:
   [cite_start]"I would rather the fire should enfold my body / with my gold-friend." [cite: 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    comitatus_bond,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        constraint_beneficiary(comitatus_bond, tribal_stability),
        constraint_victim(comitatus_bond, []),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(comitatus_bond, S),
    S > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: HROTHGAR - Rope
   --------------------------------------------------------------------------
   
   [cite_start]WHO: institutional - The King who uses the code to build Heorot. [cite: 1]
   [cite_start]WHEN: generational - Concerned with the succession and tribal survival. [cite: 1]
   [cite_start]WHERE: arbitrage - He can pay off feuds (as he did for Beowulf's father). [cite: 1]
   [cite_start]SCOPE: national - The scope of the Spear-Danes. [cite: 1]
   
   WHY THIS CLASSIFICATION:
   For the King, the code is a "Rope"—a functional tool for coordination. 
   [cite_start]He knows it requires maintenance (gifts/mead) to keep the tribe together. [cite: 1]
   
   NARRATIVE EVIDENCE:
   "He be-thought him... to bid men build a hall... and there-in all to deal 
   [cite_start]out to young and old." [cite: 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    comitatus_bond,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        constraint_beneficiary(comitatus_bond, tribal_stability),
        constraint_victim(comitatus_bond, []),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXILED THANE - Snare
   --------------------------------------------------------------------------
   
   [cite_start]WHO: individual_powerless - The lordless man. [cite: 1]
   [cite_start]WHEN: immediate - The pain of current isolation. [cite: 1]
   [cite_start]WHERE: trapped - Nowhere to go but the "whale-road." [cite: 1]
   [cite_start]SCOPE: continental - Wandering across many lands. [cite: 1]
   
   WHY THIS CLASSIFICATION:
   For the one who breaks the code or loses his lord, the comitatus becomes 
   a "Snare." It is a coercive system that now chokes his existence, as 
   [cite_start]everyone else is bound by the "Rope" he has lost. [cite: 1]
   
   NARRATIVE EVIDENCE:
   "He shall waken... and see before him the fallow waves... but the heart 
   [cite_start]is the heavier." [cite: 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    comitatus_bond,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(comitatus_bond, tribal_stability),
        constraint_victim(comitatus_bond, cowardly_thanes),
        spatial_scope(continental)
    )
) :-
    domain_priors:base_extractiveness(comitatus_bond, E),
    E > 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(comitatus_bond_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(comitatus_bond, Type1, context(agent_power(individual_powerless), _, trapped, _, _, _)),
    constraint_indexing:constraint_classification(comitatus_bond, Type2, context(agent_power(institutional), _, arbitrage, _, _, _)),
    Type1 \= Type2.

test(exile_as_noose) :-
    % Verify that for a victim (the coward), the classification is Snare
    constraint_indexing:constraint_classification(comitatus_bond, snare, context(_, _, trapped, _, cowardly_thanes, _)).

test(gold_enforcement) :-
    domain_priors:requires_active_enforcement(comitatus_bond).

:- end_tests(comitatus_bond_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: The Comitatus code is the ultimate "Rope" for the 
 * survival of the tribe, but it is experienced as a "Mountain" (inevitability) 
 * [cite_start]by the noble and a "Snare" (shame/exile) by the failure. [cite: 1]
 * 2. EXTRACTIVENESS: Set at 0.4 because while it asks for the ultimate sacrifice 
 * (death), it offers the ultimate reward (immortality in song and lordly 
 * [cite_start]provision). [cite: 1]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wyrd_nature,
    'Is \'Wyrd\' (Fate) a literal Mountain constraint or a retrospective Rope used to explain failure?',
    resolution_mechanism('Textual analysis of whether Beowulf attributes success to God or Fate consistently.'),
    impact('If Mountain: The comitatus is secondary to Fate. If Rope: The comitatus is the primary driver.'),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Christian Individualism
 * Suppression: Actively suppressed by the heroic narrative, though 
 * blending occurs. The idea of the individual soul's salvation vs 
 * [cite_start]tribal glory is a nascent tension. [cite: 1]
 * * CONCLUSION:
 * The lack of viable alternatives beyond tribal membership makes this a 
 * [cite_start]"Rope" for the group but a "Mountain" for the individual. [cite: 1]
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% To use: ?- [constraints/comitatus_bond].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
