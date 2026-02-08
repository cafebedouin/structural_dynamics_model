% ============================================================================
% CONSTRAINT STORY: kjv_puritan_new_world_exit
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: The Great Migration (1620-1640) and the evolution of American Liturgy
% ============================================================================

:- module(constraint_kjv_puritan_new_world_exit, []).

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
 * 
 * constraint_id: kjv_puritan_new_world_exit
 * human_readable: The Puritan Textual Re-Indexing (KJV in the New World)
 * domain: political/religious/geographic
 * temporal_scope: 1620 - 1690 CE
 * spatial_scope: Atlantic / New England Colonies
 * 
 * SUMMARY:
 * This story tracks the movement of the KJV across a geographic "Exit Option." 
 * In England, the KJV was a tool of the Star Chamber used to suppress Puritan 
 * identity (Snare). Upon arrival in the New World, the Puritans—now holding 
 * institutional power—adopted the KJV as a functional coordination tool 
 * (Rope) to maintain social order and literacy in a "wilderness" environment.
 * 
 * KEY AGENTS:
 * - The Dissenter (Individual Moderate): Experiences the KJV as a tool of suppression in England.
 * - The Magistrate (Institutional): Uses the KJV to provide a legal and moral baseline in New England.
 * - The Born-Colonist (Individual Powerless): Born into a society where the KJV is an immutable fact.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(kjv_puritan_new_world_exit, 0, 10).
narrative_ontology:constraint_claim(kjv_puritan_new_world_exit, rope).

% Base extractiveness: 0.2.
% In the New World context, the text provides literacy and legal structure 
% with less asymmetric "royal" benefit.
domain_priors:base_extractiveness(kjv_puritan_new_world_exit, 0.2).

% Suppression: 0.5.
% Alternatives (Geneva Bible) still exist but the KJV becomes the "Rope" 
% of least resistance for commerce and communication with the homeland.
domain_priors:suppression_score(kjv_puritan_new_world_exit, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, extractiveness, 0.2).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, suppression_requirement, 0.5).

% Enforcement: Emerges naturally (via social contract/covenant).
domain_priors:emerges_naturally(kjv_puritan_new_world_exit).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(kjv_puritan_new_world_exit, new_england_society).
constraint_victim(kjv_puritan_new_world_exit, english_puritan_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ENGLISH PURITAN (STAYED BEHIND) - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate (Lacks the capital or desire to flee)
   WHEN: immediate (Suffering under Archbishop Laud's reforms)
   WHERE: trapped (Cannot physically leave the jurisdictional reach of the Crown)
   
   WHY THIS CLASSIFICATION:
   For the Puritan who remained in England, the KJV remains a 'Snare'. It is
   enforced by the State to the exclusion of other translations, representing
   the "yoke" of the Monarch’s authority over conscience and suppressing
   religious freedom.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_puritan_new_world_exit,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NEW ENGLAND MAGISTRATE - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The rule-makers of the new colony)
   WHEN: biographical (Planning the survival of the "City on a Hill")
   WHERE: mobile (They have successfully exercised an exit option)
   
   WHY THIS CLASSIFICATION:
   For the Magistrate in New England, the KJV is a 'Rope'. It is no longer
   "The King's Bible," but a standard for law and education. It facilitates
   literacy and communal unity without the direct oversight of the Anglican
   bishops, serving as a functional coordination mechanism for the new society.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_puritan_new_world_exit,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BORN-COLONIST (2nd Generation) - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Born into the system)
   WHEN: generational (It has "always" been this way)
   WHERE: trapped (The wilderness is vast; the Bible is the only map)
   
   WHY THIS CLASSIFICATION:
   For the child born in the colony, the KJV is a 'Mountain'. They never
   witnessed the "Geneva vs. KJV" battle. The language of the KJV is
   simply "the language of God," an unchangeable feature of their reality
   and social fabric.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_puritan_new_world_exit,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(kjv_puritan_new_world_exit_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, Type3, context(agent_power(powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(kjv_puritan_new_world_exit_tests).

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
 * 1. MANDATROPHY STATUS: This constraint is classified as a 'Rope' overall
 *    because the Puritans successfully re-indexed it from a 'Snare' to a
 *    tool for social cohesion in the New World.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - English Puritan (Snare): Tool of state suppression.
 *    - New England Magistrate (Rope): Tool for social order and literacy.
 *    - Born-Colonist (Mountain): Immutably integrated into social reality.
 * 
 * 3. CORE INSIGHT: The KJV's role shifts dramatically based on context and
 *    power dynamics. What is a 'Snare' of state control in one environment
 *    becomes a 'Rope' of communal coordination in another, and an immutable
 *    'Mountain' for subsequent generations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the long-term impact of a text re-indexed from suppression to coordination.
 */

omega_variable(
    textual_reindexing_permanence,
    "Did the KJV truly lose its 'Snare' qualities in the New World, or did the Puritan re-indexing merely suppress its coercive elements, which could re-emerge under different conditions?",
    resolution_mechanism("Comparative historical analysis of textual authority and social control in different colonial contexts."),
    impact("If permanent: The re-indexing creates a lasting 'Rope'. If temporary: The 'Snare' remains latent, awaiting re-emergence."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: The Geneva Bible
 *    Viability: A preferred translation for many Puritans, featuring Calvinist marginal notes.
 *    Suppression: Though not actively suppressed by the New England magistrates, its use diminished as the KJV became the de facto standard for public life and education.
 *
 * CONCLUSION:
 * The choice to adopt the KJV in the New World, rather than promoting the
 * Geneva Bible, highlights its utility as a 'Rope' for social cohesion.
 * The 'Exit Option' from England transformed the KJV from a 'Snare' of
 * state control into a tool for communal coordination.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/kjv_puritan_new_world_exit].
 * 2. Multi-perspective: ?- multi_index_report(kjv_puritan_new_world_exit).
 * 3. Run tests: ?- run_tests(kjv_puritan_new_world_exit_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Coordination mechanism in political domain — moderate institutional framing
domain_priors:theater_ratio(kjv_puritan_new_world_exit, 0.17).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, theater_ratio, 0.17).

% --- Analytical perspective classification (missing) ---
% chi = 0.2 * 1.15 (analytical) * 1.2 (global) = 0.276
% Classification: scaffold
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
