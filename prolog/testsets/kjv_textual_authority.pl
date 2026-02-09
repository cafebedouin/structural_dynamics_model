% ============================================================================
% CONSTRAINT STORY: kjv_textual_authority
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: The 1611 King James Bible and its subsequent cultural hegemony
% ============================================================================

:- module(constraint_kjv_textual_authority, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: kjv_textual_authority
 * human_readable: The King James Textual Monopoly
 * domain: religious/linguistic/political
 * temporal_scope: 1611 CE to Present
 * spatial_scope: Anglosphere / Global
 * 
 * SUMMARY:
 * The KJV was commissioned by King James I to stabilize a fractured English 
 * church by replacing the radical Geneva Bible. It evolved from a 
 * state-enforced "Rope" (unifying the realm) into a "Mountain" for 
 * believers who view its specific 17th-century English as the literal, 
 * immutable voice of God.
 * 
 * KEY AGENTS:
 * - King James I (Institutional): Seeking political stability.
 * - The KJV-Only Layperson (Individual Powerless): Views the KJV as the immutable Word of God.
 * - The Disgusted Puritan (Individual Moderate): Forced to accept the KJV or flee.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(kjv_textual_authority, 0, 10).
narrative_ontology:constraint_claim(kjv_textual_authority, mountain).

% Base extractiveness: 0.4 (Moderate)
% It provides high utility (standardized language) but extracts 
% cultural compliance and suppressed diverse theological interpretations.
domain_priors:base_extractiveness(kjv_textual_authority, 0.4).

% Suppression: 0.7 (High)
% Historically, the Crown banned the Geneva Bible to force KJV adoption.
% In modern "KJV-Only" circles, alternative texts are viewed as demonic.
domain_priors:suppression_score(kjv_textual_authority, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(kjv_textual_authority, extractiveness, 0.4).
narrative_ontology:constraint_metric(kjv_textual_authority, suppression_requirement, 0.7).

% Enforcement: Requires active enforcement (historically by the state).
domain_priors:requires_active_enforcement(kjv_textual_authority).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(kjv_textual_authority, english_monarchy).
narrative_ontology:constraint_victim(kjv_textual_authority, puritan_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: KING JAMES I - Rope
   --------------------------------------------------------------------------
   WHO: institutional - The state as rule-maker.
   WHEN: immediate/biographical - Focused on political stability.
   WHERE: arbitrage - The King can choose translators and set the rules.
   
   WHY THIS CLASSIFICATION:
   To the State, the KJV is a 'Rope'. It is a functional coordination mechanism 
   designed to bridge the gap between High Church Anglicans and Puritans, 
   ensuring the King's supremacy over the Church. It is a tool for social engineering.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_textual_authority,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE KJV-ONLY LAYPERSON - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - The subject receiving the word.
   WHEN: generational/civilizational - The text is "eternal."
   WHERE: trapped - To leave the text is to leave the Faith.
   
   WHY THIS CLASSIFICATION:
   For this agent, the KJV is a 'Mountain'. It is not a "translation"; it is 
   the "Authorized Version." Its archaic grammar is the natural language 
   of the sacred. There are zero degrees of freedom to change it.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_textual_authority,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISGUSTED PURITAN (1620) - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate - Has strong convictions but lacks state power.
   WHEN: immediate - Watching their preferred Bible (Geneva) being banned.
   WHERE: constrained - Can flee to the "New World" at great cost.
   
   WHY THIS CLASSIFICATION:
   For the Puritan, the KJV is a 'Snare'. It is an asymmetric imposition by the 
   Crown that removes the "people's" study notes (found in the Geneva Bible) 
   to enforce ecclesiastical hierarchy. It is coercive and extractive of religious liberty.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_textual_authority,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(regional)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(kjv_textual_authority_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(kjv_textual_authority, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(kjv_textual_authority, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_textual_authority, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(kjv_textual_authority_tests).

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
 * 1. CLASSIFICATION RATIONALE:
 *    - Monarch (Rope): A tool for social engineering and political stability.
 *    - KJV-Only Layperson (Mountain): The text is seen as an immutable, divine truth.
 *    - Puritan (Snare): An oppressive imposition that curtails religious freedom.
 * 
 * 2. EXTRACTIVENESS (0.4): Chosen because while the KJV "extracts" theological
 *    variety and religious freedom, it "provides" a massive cultural and
 *    linguistic surplus. It’s not purely predatory, but its origins are coercive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the long-term impact of a religiously enforced linguistic monoculture.
 */

omega_variable(
    linguistic_diversity_impact,
    "Did the KJV's textual authority suppress the development of other English theological lexicons (Snare) or merely provide a necessary common linguistic foundation (Rope)?",
    resolution_mechanism("Comparative analysis of theological literature development in Anglophone vs. other European linguistic traditions (e.g., German, French)."),
    impact("If suppressed: The KJV was a linguistic 'Snare'. If merely provided foundation: A cultural 'Rope'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: The Geneva Bible
 *    Viability: High. It was the most popular Bible among the people and Puritans.
 *    Suppression: King James specifically banned its printing in England because
 *    its marginal notes were too "seditious" (questioning monarchical authority).
 *
 * CONCLUSION:
 * The active suppression of the highly popular Geneva Bible confirms that for
 * the 17th-century dissenter, the KJV was a 'Snare', despite it appearing as
 * a "Natural Law" ('Mountain') to their descendants. This highlights the political
 * and coercive origins of seemingly immutable cultural artifacts.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/kjv_textual_authority].
 * 2. Multi-perspective: ?- multi_index_report(kjv_textual_authority).
 * 3. Run tests: ?- run_tests(kjv_textual_authority_tests).
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
% Structural constraint in religious domain — low theater, high substance
domain_priors:theater_ratio(kjv_textual_authority, 0.12).
narrative_ontology:constraint_metric(kjv_textual_authority, theater_ratio, 0.12).

% --- Analytical perspective classification (missing) ---
% chi = 0.4 * 1.15 (analytical) * 1.2 (global) = 0.552
% Classification: tangled_rope
constraint_indexing:constraint_classification(kjv_textual_authority, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
