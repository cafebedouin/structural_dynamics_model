% ============================================================================
% CONSTRAINT STORY: kjv_textual_authority
% ============================================================================
% Generated: 2024-05-22
% Model: Gemini 1.5 Pro
% Source: The 1611 King James Bible and its subsequent cultural hegemony
% ============================================================================

:- module(kjv_textual_authority, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% The Structural Anchor for system extraction
narrative_ontology:interval(kjv_textual_authority, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: kjv_textual_authority
 * human_readable: The King James Textual Monopoly
 * domain: religious/linguistic/political
 * temporal_scope: 1611 CE to Present
 * spatial_scope: Anglosphere / Global
 * * SUMMARY:
 * The KJV was commissioned by King James I to stabilize a fractured English 
 * church by replacing the radical Geneva Bible. It evolved from a 
 * state-enforced "Rope" (unifying the realm) into a "Mountain" for 
 * believers who view its specific 17th-century English as the literal, 
 * immutable voice of God.
 * * KEY AGENTS:
 * - The Monarch/State: Seeking political stability and uniform liturgy.
 * - The Lay Believer: Using the text for spiritual survival and identity.
 * - The Modern Scholar/Linguist: Viewing the text as a historical artifact 
 * subject to correction.
 * * NARRATIVE ARC:
 * Originally a tool of statecraft to suppress dissent (Puritanism), it 
 * achieved such linguistic dominance that for centuries, alternative 
 * translations were seen not just as "different," but as "sacrilegious."
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: Moderate (0.4)
% It provides high utility (standardized language) but extracts 
% cultural compliance and suppressed diverse theological interpretations.
domain_priors:base_extractiveness(kjv_textual_authority, 0.4).

% Suppression: High (0.7)
% Historically, the Crown banned the Geneva Bible to force KJV adoption.
% In modern "KJV-Only" circles, alternative texts are viewed as demonic.
domain_priors:suppression_score(kjv_textual_authority, 0.7).

% Enforcement: Requires active enforcement (historically)
domain_priors:requires_active_enforcement(kjv_textual_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MONARCH (James I) - ROPE
   --------------------------------------------------------------------------
   WHO: institutional - The state as rule-maker.
   WHEN: immediate/biographical - Focused on immediate political stability.
   WHERE: arbitrage - The King can choose translators and set the rules.
   SCOPE: national - The English Realm.
   
   WHY THIS CLASSIFICATION:
   To the State, the KJV is a Rope. It is a functional coordination mechanism 
   designed to bridge the gap between High Church Anglicans and Puritans, 
   ensuring the King's supremacy over the Church. It is a tool to be used.
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE "KJV-ONLY" LAYPERSON - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: individual_powerless - The subject receiving the word.
   WHEN: generational/civilizational - The text is "eternal."
   WHERE: trapped - To leave the text is to leave the Faith.
   SCOPE: global - The universal truth for all souls.
   
   WHY THIS CLASSIFICATION:
   For this agent, the KJV is a Mountain. It is not a "translation"; it is 
   the "Authorized Version." Its archaic grammar is the natural language 
   of the sacred. There are zero degrees of freedom to change it.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_textual_authority,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISGUSTED PURITAN (1620) - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_moderate - Has strong convictions but lacks state power.
   WHEN: immediate - Watching their preferred Bible (Geneva) being banned.
   WHERE: constrained - Can flee to the "New World" at great cost.
   SCOPE: local/regional - Their specific congregation.
   
   WHY THIS CLASSIFICATION:
   The KJV is a Noose. It is an asymmetric imposition by the Crown that 
   removes the "people's" study notes (found in the Geneva Bible) to 
   enforce ecclesiastical hierarchy. It is coercive and extractive of 
   religious liberty.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kjv_textual_authority,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(kjv_textual_authority_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(kjv_textual_authority, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(kjv_textual_authority, Type2, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2.

test(sacralization_effect) :-
    % Test that increasing time horizon shifts perception from Rope to Mountain
    constraint_indexing:constraint_classification(kjv_textual_authority, mountain, context(_, time_horizon(civilizational), _, _)).

test(exit_cost_classification) :-
    % Test that trapped exit options + powerless agents = Mountain
    constraint_indexing:constraint_classification(kjv_textual_authority, mountain, context(agent_power(individual_powerless), _, exit_options(trapped), _)).

:- end_tests(kjv_textual_authority_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 1.5 Pro
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.4): Chosen because while the KJV "extracts" 
 * theological variety, it "provides" a massive cultural and literary 
 * surplus. It’s not purely extractive like a tax, but it is asymmetric 
 * in its origins.
 * * 2. PERSPECTIVE SELECTION: Included the 1620 Puritan perspective to show 
 * the "Noose" phase before the text became "Mountain." This illustrates 
 * the lifecycle of a constraint.
 * * 3. CLASSIFICATION RATIONALE:
 * - Monarch -> Rope: It's a tool for social engineering.
 * - Modern Believer -> Mountain: Time has erased the memory of it 
 * being a choice.
 * - Puritan -> Noose: It was an active weapon used to displace their 
 * preferred technology (the Geneva Bible).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The Geneva Bible
 * Viability: High. It was the most popular Bible among the people.
 * Suppression: King James specifically banned its printing in England 
 * because its marginal notes were too "seditious" (questioning kings).
 * * CONCLUSION:
 * The existence of a suppressed, highly viable alternative (Geneva) 
 * confirms that for the 17th-century dissenter, the KJV was a Noose, 
 * despite it appearing as a "Natural Law" (Mountain) to their descendants.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [kjv_textual_authority].
 * 2. Run tests: ?- run_tests(kjv_textual_authority_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
