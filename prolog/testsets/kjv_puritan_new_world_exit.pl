% ============================================================================
% CONSTRAINT STORY: puritan_new_world_pivot
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 1.5 Pro
% Source: The Great Migration (1620-1640) and the evolution of American Liturgy
% ============================================================================

:- module(puritan_new_world_pivot, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% The Structural Anchor for system extraction
narrative_ontology:interval(puritan_new_world_pivot, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: puritan_new_world_pivot
 * human_readable: The Puritan Textual Re-Indexing
 * domain: political/religious/geographic
 * temporal_scope: 1620 - 1690 CE
 * spatial_scope: Atlantic / New England Colonies
 * * SUMMARY:
 * This story tracks the movement of the KJV across a geographic "Exit Option." 
 * In England, the KJV was a tool of the Star Chamber used to suppress Puritan 
 * identity (Noose). Upon arrival in the New World, the Puritans—now holding 
 * institutional power—adopted the KJV as a functional coordination tool 
 * (Rope) to maintain social order and literacy in a "wilderness" environment.
 * * KEY AGENTS:
 * - The Dissenter (England): Viewing the KJV as an emblem of state tyranny.
 * - The Magistrate (New England): Using the KJV to provide a legal and moral baseline.
 * - The Indentured Servant: Experiencing the text as a fixed social "Mountain."
 * * NARRATIVE ARC:
 * The "Exit" (migration) removes the coercive element of the King’s enforcement, 
 * allowing the community to re-evaluate the constraint as a beneficial 
 * coordination mechanism for their new society.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: Low (0.2) 
% In the New World context, the text provides literacy and legal structure 
% with less asymmetric "royal" benefit.
domain_priors:base_extractiveness(puritan_new_world_pivot, 0.2).

% Suppression: Moderate (0.5)
% Alternatives (Geneva Bible) still exist but the KJV becomes the "Rope" 
% of least resistance for commerce and communication with the homeland.
domain_priors:suppression_score(puritan_new_world_pivot, 0.5).

% Enforcement: Emerges naturally (via social contract/covenant)
domain_priors:emerges_naturally(puritan_new_world_pivot).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NEW ENGLAND MAGISTRATE - ROPE
   --------------------------------------------------------------------------
   WHO: institutional - The rule-makers of the new colony.
   WHEN: biographical - Planning the survival of the "City on a Hill."
   WHERE: mobile - They have successfully exercised an exit option.
   SCOPE: regional - The Massachusetts Bay Colony.
   
   WHY THIS CLASSIFICATION:
   For the Magistrate, the KJV is a Rope. It is no longer "The King's Bible," 
   but a standard for law and education. It facilitates literacy and 
   communal unity without the direct oversight of the Anglican bishops.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    puritan_new_world_pivot,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ENGLISH PURITAN (STAYED BEHIND) - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_moderate - Lacks the capital or desire to flee.
   WHEN: immediate - Suffering under Archbishop Laud's reforms.
   WHERE: trapped - Cannot physically leave the jurisdictional reach of the Crown.
   SCOPE: national - Under the High Commission.
   
   WHY THIS CLASSIFICATION:
   The KJV remains a Noose. It is enforced by the State to the exclusion of 
   the Geneva Bible. It represents the "yoke" of the Monarch’s authority 
   over the conscience.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    puritan_new_world_pivot,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BORN-COLONIST (2nd Generation) - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: individual_powerless - Born into the system.
   WHEN: generational - It has "always" been this way.
   WHERE: trapped - The wilderness is vast; the Bible is the only map.
   SCOPE: local - The village and the meeting house.
   
   WHY THIS CLASSIFICATION:
   For the child born in the colony, the KJV is a Mountain. They never 
   witnessed the "Geneva vs. KJV" battle. The language of the KJV is 
   simply "the language of God." It is an unchangeable feature of reality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    puritan_new_world_pivot,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(puritan_pivot_tests).

test(exit_option_reclassification) :-
    % Test that mobility (Exit) changes Noose to Rope for institutional agents
    constraint_indexing:constraint_classification(puritan_new_world_pivot, rope, context(agent_power(institutional), _, exit_options(mobile), _)).

test(generational_mountain_drift) :-
    % Test that the passage of time (generational) creates a Mountain perception
    constraint_indexing:constraint_classification(puritan_new_world_pivot, mountain, context(_, time_horizon(generational), _, _)).

test(geographic_variance) :-
    % Test that classification varies by spatial context (National vs Regional)
    constraint_indexing:constraint_classification(puritan_new_world_pivot, noose, context(_, _, _, spatial_scope(national))),
    constraint_indexing:constraint_classification(puritan_new_world_pivot, rope, context(_, _, _, spatial_scope(regional))).

:- end_tests(puritan_pivot_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 1.5 Pro
 * Date: 2026-01-16
 * * KEY DECISIONS:
 * 1. THE "EXIT" AS THE PIVOT: The core insight here is that a "Noose" 
 * requires a centralized enforcer. When the subject exits the enforcer's 
 * reach but retains the *technology* (the text), the technology is 
 * re-indexed as a "Rope" (voluntary coordination).
 * * 2. EXTRACTIVENESS SCORE (0.2): Lowered from the previous KJV file because 
 * the colonial context utilized the text for internal coordination rather 
 * than external state-level extraction.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE: The "Bay Psalm Book" / Original Covenant Documents
 * Viability: High. The Puritans were capable of writing their own 
 * textual standards.
 * Suppression: None. They chose to keep the KJV as a "Rope" to maintain 
 * a linguistic and diplomatic link to England, demonstrating its 
 * utility over its coerciveness in this new context.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
