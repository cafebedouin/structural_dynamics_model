% ============================================================================
% CONSTRAINT STORY: dharma_of_kurukshetra
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 2.0 (AI Thought Partner)
% Source: The Bhagavad-Gita (Edwin Arnold Translation)
% ============================================================================

:- module(dharma_of_kurukshetra, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor for System Extraction
narrative_ontology:interval(dharma_of_kurukshetra, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: dharma_of_kurukshetra
 * human_readable: The Duty of the Kshatriya (Warrior Caste)
 * domain: religious/philosophical/social
 * temporal_scope: Ancient India (The Mahabharata era)
 * spatial_scope: The sacred plain of Kurukshetra
 * * SUMMARY:
 * The "dharma" or sacred duty of the warrior caste (Kshattriya) to fight in a 
 * lawful war, even against kin. It presents an inescapable moral obligation 
 * rooted in the nature of the soul and the cosmic order.
 * * KEY AGENTS:
 * - Arjuna: The Prince/Warrior, experiencing moral distress and viewing the 
 * obligation to kill kin as a destructive "Noose" or "Hell-ward road".
 * - Krishna: The Supreme Being/Charioteer, who reframes the battle as a 
 * "Mountain" (Natural Law of the Spirit) and a "Rope" (Service to the Whole).
 * - Dhritirashtra: The blind King, representing the Institutional view of the 
 * status quo and the rigid results of past actions.
 * * NARRATIVE ARC:
 * Arjuna collapses in distress, refusing to fight. Krishna 
 * instructs him on the immortality of the soul and the necessity of selfless 
 * action (Karma Yoga), transforming Arjuna's perception of the war from 
 * personal slaughter to cosmic necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: 0.2 (Low)
% While a war extracts lives, the Gita argues that "Right Action" yields 
% no separate profit for the actor; it is for the "upholding of thy kind".
domain_priors:base_extractiveness(dharma_of_kurukshetra, 0.2).

% Suppression: 0.9 (High)
% Alternatives (like fleeing to be a beggar) are explicitly framed as 
% "shameful," "infamous," and "sinful" for a noble-born warrior.
domain_priors:suppression_score(dharma_of_kurukshetra, 0.9).

% Enforcement: Emerges naturally from the cosmic order (Dharma).
domain_priors:emerges_naturally(dharma_of_kurukshetra).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Arjuna (Initial State) - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (He feels trapped by family ties and sin)
   WHEN: immediate (The horror of the battle to break tomorrow)
   WHERE: trapped (He sees "mutual slaughter" with no good outcome)
   
   WHY THIS CLASSIFICATION:
   Arjuna sees the constraint as extractive and destructive. It "perisheth" 
   households and leads to "Hell". It is a weight that 
   paralyzes him.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dharma_of_kurukshetra,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(dharma_of_kurukshetra, S),
    S > 0.8.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Krishna (The Teacher) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The Supreme Lord/Charioteer)
   WHEN: historical (Spanning ages and "Kalpas")
   WHERE: mobile (Beyond the three qualities/modes)
   
   WHY THIS CLASSIFICATION:
   Krishna frames Dharma as a functional coordination mechanism for the world. 
   "Work is more excellent than idleness". It is a path to "highest 
   bliss" and "deliverance" when done without attachment.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dharma_of_kurukshetra,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Enlightened Sage (Muni) - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (The observer of the "Qualities")
   WHEN: civilizational (The life of the Spirit is "eternal")
   WHERE: analytical (Not constrained by results)
   
   WHY THIS CLASSIFICATION:
   To the sage, Dharma is an unchangeable Law of Nature. "Life cannot slay. 
   Life is not slain!". Actions are just the "play of visible things" 
   and "Nature's way".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dharma_of_kurukshetra,
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

:- begin_tests(dharma_of_kurukshetra_tests).

test(perspectival_shift) :-
    % Test that the same duty (Dharma) shifts from Noose to Rope via Knowledge
    constraint_indexing:constraint_classification(dharma_of_kurukshetra, noose, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(dharma_of_kurukshetra, rope, context(agent_power(institutional), _, _, _)).

test(immutability_of_spirit) :-
    % Test that at the highest time horizon, the constraint is a Mountain (Natural Law)
    constraint_indexing:constraint_classification(dharma_of_kurukshetra, mountain, context(_, time_horizon(civilizational), _, _)).

:- end_tests(dharma_of_kurukshetra_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 (Thought Partner)
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS: Set low (0.2). Krishna emphasizes that the warrior 
 * receives no "fruit" if acting rightly. The "extraction" 
 * is of the ego, not for a predatory master.
 * 2. PERSPECTIVES: Arjuna's initial state is the quintessential "Powerless" 
 * view. Krishna represents the "Institutional/Universal" order. 
 * The "Sage" represents the "Analytical" detachment.
 * 3. AMBIGUITY: Dharma is often translated as "Duty" (Rope) or "Law" (Mountain). 
 * The Gita uses the tension between these to resolve Arjuna's crisis.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Sannyas (Renunciation/Asceticism)
 * Viability: Arjuna proposes living on "beggar's bread".
 * Suppression: Krishna argues that "none shall 'scape from act by shunning 
 * action". He redefines Renunciation as the giving up of 
 * *fruit*, not the giving up of *task*.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(gita_kurukshetra, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(gita_kurukshetra, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(gita_kurukshetra, noose, agent_power(individual_powerless)).
