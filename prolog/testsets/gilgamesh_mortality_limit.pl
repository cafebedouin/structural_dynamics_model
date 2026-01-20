% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: gilgamesh_mortality_limit
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Epic of Gilgamish (Stephen Langdon / University Museum)
% ============================================================================

:- module(constraint_gilgamesh_mortality, []).

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
 * * constraint_id: gilgamesh_mortality_limit
 * human_readable: The Allotment of Mortality
 * domain: philosophical/religious/social
 * temporal_scope: Ancient Mesopotamia (~2100 BCE)
 * spatial_scope: Uruk / The Waters of Death
 * * SUMMARY:
 * This constraint models mortality as the ultimate physical limit defined in the 
 * Epic. In the text, Gilgamesh—a king of immense power—attempts to treat his 
 * mortality as a "Noose" he can untie through a "Mandate of Immortality". 
 * Ultimately, he discovers that death is a Mountain (a physical law set by the 
 * gods) that extracts the "margin" of human life regardless of rank or strength.
 * * KEY AGENTS:
 * - Gilgamesh (The Seeker): Individual powerful; a king who views his mortality 
 * as an extractive Noose after the death of Enkidu.
 * - Utnapishtim (The Immortal): Institutional/Analytical; an observer who 
 * exists outside the human constraint and defines the "Rope" of survival.
 * - The Gods (Enlil/Anu): Institutional; the architects who allotted death 
 * to mankind while keeping life for themselves.
 * * NARRATIVE ARC:
 * Initially, Gilgamesh views his kingship as a Rope (coordination for fame). 
 * Upon Enkidu's death, his existence becomes a Noose, "choking" his desire 
 * to rule in favor of a frantic search for an escape. He eventually hits the 
 * Mountain (the failed sleep test/the lost plant), transitioning to a 
 * realization that the city walls of Uruk are the only enduring "Scaffold" 
 * available to man.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(gilgamesh_epic_cycle, 0, 10).
narrative_ontology:constraint_claim(gilgamesh_mortality_limit, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 1.0. Death is the ultimate extraction; it takes the entire 
% "margin" of the subject's biological and social presence.
domain_priors:base_extractiveness(gilgamesh_mortality_limit, 1.0).

% Suppression score (0.0-1.0)
% Rationale: 0.9. The "Immortal" alternative is strictly suppressed by 
% biological limits and divine decree (Enlil’s anger).
domain_priors:suppression_score(gilgamesh_mortality_limit, 0.9).

% Enforcement requirements
% Emerges naturally from biological reality (Mountain) but is framed as 
% actively enforced by the divine council (Noose).
domain_priors:emerges_naturally(gilgamesh_mortality_limit).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, extractiveness, 1.0).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, suppression_requirement, 0.9).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(gilgamesh_mortality_limit, divine_monopoly_on_eternal_life).
constraint_beneficiary(gilgamesh_mortality_limit, ecological_population_balance).
constraint_victim(gilgamesh_mortality_limit, gilgamesh).
constraint_victim(gilgamesh_mortality_limit, human_ambition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: GILGAMESH (POST-ENKIDU) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerful - A king whose power provides no leverage 
         against the "Sleep" or the "End".
   WHEN: immediate - Tactical grief; the realization that "I too shall die."
   WHERE: trapped - Searching through the Cedar Forest and the Waters of Death.
   SCOPE: local - His personal existential dread.
   
   WHY THIS CLASSIFICATION:
   For Gilgamesh, mortality is a Noose. It extracts his joy and his 
   ability to act as King, "choking" his spirit until he can move the 
   weight by finding Utnapishtim. He views death not as a law, but as 
   an extractive enemy that stole Enkidu.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    gilgamesh_mortality_limit,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(gilgamesh_mortality_limit, E),
    E > 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GODS / UTNAPISHTIM - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional/analytical - Those who set the "allotment" of man.
   WHEN: generational/historical - Viewing humanity as a collective flow.
   WHERE: mobile - Utnapishtim resides "at the mouth of the rivers."
   SCOPE: global - The management of the human race.
   
   WHY THIS CLASSIFICATION:
   From the institutional perspective of the gods, mortality is a Rope. 
   It is a functional coordination mechanism to ensure that the earth 
   is not overpopulated (The Flood myth context) and that the distinction 
   between divine and mortal is maintained. It organizes the world's 
   "noise" into a predictable "rhythm" of life and death.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    gilgamesh_mortality_limit,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(gilgamesh_mortality_limit, E),
    E < 1.1, % Institutional distance creates the illusion of coordination.
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE RETURNED GILGAMESH - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - He returns to Uruk as a man who has failed 
         the test of the "Plant of Heartbeat".
   WHEN: biographical - Acceptance of the life-cycle.
   WHERE: trapped - Within the walls of Uruk.
   SCOPE: local - His final kingdom.
   
   WHY THIS CLASSIFICATION:
   By the end of the Epic, Gilgamesh views mortality as a Mountain. 
   It is an unchangeable, unyielding fact of the world. He stops 
   trying to "un-choke" the Noose and instead starts building the 
   walls of Uruk—a cultural Scaffold that survives the individual's 
   collision with the Mountain.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gilgamesh_mortality_limit,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:emerges_naturally(gilgamesh_mortality_limit),
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(gilgamesh_mortality_tests).

test(the_allotment_delta) :-
    % Institutional view (Rope) vs Mortal view (Mountain)
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, T1, context(institutional, historical, mobile, global)),
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, T2, context(individual_powerless, biographical, trapped, local)),
    T1 \= T2.

test(absolute_extraction) :-
    % Death always extracts the biological margin.
    domain_priors:base_extractiveness(gilgamesh_mortality_limit, 1.0).

:- end_tests(gilgamesh_mortality_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. MANDATROPHY ANALYSIS: Gilgamesh’s quest is a "Mandate of Immortality" 
 * that atrophies his duty to his people. He extracts the "margin" of his 
 * energy and his city’s resources to chase a non-viable alternative.
 * 2. THE MOUNTAIN: The "Failed Sleep Test" is the primary diagnostic of 
 * the Mountain. If you cannot even control the "little death" of sleep, 
 * you cannot negotiate with the "Great Death."
 * 3. THE WALLS AS SCAFFOLD: The conclusion shifts the focus from the 
 * individual (who dies) to the city (which remains), a classic Scaffold 
 * against the Mountain of time.
 */

omega_variable(
    the_name_immortality_proxy,
    "Does 'Fame' (The Name-Scaffold) actually resolve the Noose of mortality, 
     or is it a secondary extraction that keeps the hero working for a 
     mandate they will never see?",
    resolution_mechanism("Evaluation of Gilgamesh's legacy vs. his actual 
    biological experience in the text"),
    impact("If Legacy satisfies: The Scaffold works. If he still fears 
            death: It is a deceptive Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Eternal Life (The "Utnapishtim" Scaffold)
 * Viability: Proven viable for one individual through divine intervention.
 * Suppression: Actively suppressed by the Gods who "sealed the allotment" 
 * for all other humans.
 * * ALTERNATIVE 2: The Plant of Heartbeat
 * Viability: Physical medicine to restore youth.
 * Suppression: Stolen by the Serpent—a "random" environmental event that 
 * re-asserts the Mountain.
 * * CONCLUSION:
 * The existence of a single immortal (Utnapishtim) proves that the Noose 
 * is a choice made by the "Institutional" gods, which appears as a 
 * Mountain to the "Individual Powerless" man.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [gilgamesh_mortality_limit].
% 2. Analyze: ?- multi_index_report(gilgamesh_mortality_limit).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
