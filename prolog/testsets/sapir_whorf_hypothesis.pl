% ============================================================================
% CONSTRAINT STORY: sapir_whorf_hypothesis
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Edward Sapir / Benjamin Lee Whorf / Linguistic Relativity
% ============================================================================

:- module(constraint_sapir_whorf_hypothesis, []).

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
 * * constraint_id: sapir_whorf_hypothesis
 * human_readable: Sapir-Whorf Hypothesis (Linguistic Relativity)
 * domain: social/technological/political
 * temporal_scope: Permanent (Cognitive/Linguistic Architecture)
 * spatial_scope: Global (All Language-Bearing Agents)
 * * SUMMARY:
 * The Sapir-Whorf hypothesis proposes that the structure of a language affects its 
 * speakers' worldview or cognition. In its "strong" form (linguistic determinism), 
 * language limits what can be thought; in its "weak" form (linguistic relativity), 
 * it merely influences perception and memory.
 * * KEY AGENTS:
 * - The Linguist (Analytical): Observes the "Mountain" of semantic categories and 
 * how they vary across cultures.
 * - The State/Propagandist (Institutional): Uses language as a "Rope" (coordination) 
 * or "Snare" (control) to shape public thought via Newspeak or framing.
 * - The Monolingual Speaker (Individual Powerless): Trapped within their own 
 * language's concepts, experiencing them as inevitable reality.
 * * NARRATIVE ARC:
 * Language begins as a "Mountain"—the fundamental hardware of thought. 
 * For the diplomat, it is a "Rope" used to bridge differing worldviews. 
 * For the subject of a totalizing ideology, it becomes a "Snare," as the 
 * removal of specific words (e.g., 'freedom') strangles the ability to 
 * conceptualize dissent.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural anchor
narrative_ontology:interval(sapir_whorf_interval, 0, 10).
narrative_ontology:constraint_claim(sapir_whorf_hypothesis, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). Language extracts "unlabeled" thoughts. 
% It forces the mind into specific semantic ruts, effectively "taxing" 
% thoughts that fall outside the linguistic norm.
domain_priors:base_extractiveness(sapir_whorf_hypothesis, 0.5).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). Language is the ultimate suppressor; what is not 
% named is often not noticed, making alternatives conceptually invisible.
domain_priors:suppression_score(sapir_whorf_hypothesis, 0.7).

% Enforcement requirements
% Emerges naturally through socialization but can be actively enforced 
% via censorship or newspeak.
domain_priors:emerges_naturally(sapir_whorf_hypothesis).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, extractiveness, 0.5).
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(sapir_whorf_hypothesis, [ideologues, advertisers, poets]).
constraint_victim(sapir_whorf_hypothesis, [dissenters, cross_cultural_communicators]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COGNITIVE SCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the neural and linguistic substrate.
   WHEN: civilizational - Viewing language as an evolved human constant.
   WHERE: trapped - Logic and perception are filtered by the available lexicon.
   SCOPE: global - Universal to all human populations.
   
   WHY THIS CLASSIFICATION:
   To the scientist, linguistic relativity is a Mountain. It is an unchangeable 
   feature of human cognition. We are biologically wired to categorize the 
   world; the fact that those categories are linguistic is a fixed peak 
   in the landscape of the human experience.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sapir_whorf_hypothesis,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRANSLATOR / DIPLOMAT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to bridge and define meaning between groups.
   WHEN: biographical - Managing cross-cultural relations over a career.
   WHERE: arbitrage - Can move between different linguistic worldviews.
   SCOPE: continental - Managing relations between large linguistic blocs.
   
   WHY THIS CLASSIFICATION:
   For the diplomat, language is a Rope. It is a coordination mechanism. 
   By finding the right words to align different conceptual worlds, they 
   use the constraint as a tether to pull toward peace or trade. For them, 
   linguistic relativity is a tool for understanding and influence.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sapir_whorf_hypothesis,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(continental)
    )
) :-
    domain_priors:base_extractiveness(sapir_whorf_hypothesis, E),
    E < 0.7, % It's a tool when managed with arbitrage
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE TOTALITARIAN SUBJECT - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the state's controlled vocabulary.
   WHEN: immediate - Today's news and required speech.
   WHERE: trapped - Cannot speak or think outside the state's language.
   SCOPE: national - The reach of the state's linguistic control.
   
   WHY THIS CLASSIFICATION:
   For the subject in a Newspeak environment, language is a Snare. The state 
   actively removes words and concepts to "strangle" the possibility of 
   heretical thought. The extraction of semantic range turns the primary tool 
   of thought into a literal trap for the mind.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sapir_whorf_hypothesis,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(sapir_whorf_hypothesis, S),
    S > 0.6, % High suppression of conceptual alternatives
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(sapir_whorf_hypothesis_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Snare
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, rope, context(institutional, biographical, arbitrage, continental)),
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, snare, context(individual_powerless, immediate, trapped, national)).

test(power_extractiveness_cognition) :-
    % Powerless subjects feel the total extraction of thought (Snare).
    % Institutional diplomats leverage the gap for coordination (Rope).
    ContextPowerless = context(individual_powerless, immediate, trapped, national),
    ContextPowerful = context(institutional, biographical, arbitrage, continental),
    constraint_indexing:extractiveness_for_agent(sapir_whorf_hypothesis, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(sapir_whorf_hypothesis, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(sapir_whorf_hypothesis_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.5): Chose moderate because while language is essential, 
 * it "extracts" potential thoughts by channeling them into narrow categories.
 * 2. PERSPECTIVES: Selected the Scientist (Hardware), the Diplomat (Arbitrage), 
 * and the Orwellian Subject (Extraction) to highlight how language shifts 
 * from fact to tool to trap.
 * 3. NOOSE LOGIC: Specifically focuses on "Linguistic Determinism" used 
 * politically—where removing the word for a concept removes the concept.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strong_vs_weak_determinism,
    "Does language truly prevent thoughts (Mountain/Snare) or merely make 
    some thoughts easier than others (Rope)?",
    resolution_mechanism("Long-term neuro-linguistic studies on speakers 
    of languages without specific numeric or spatial markers"),
    impact("If Strong: Linguistic control is an inescapable Snare. If Weak: 
    It is a Rope that can be untied through education."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Universal Grammar (Chomskyan)
 * Viability: High. The idea that all humans share a deep, innate biological 
 * structure for language regardless of vocabulary.
 * Suppression: Moderate. Often presented as the "Anti-Whorf" view, 
 * suppressing the idea of total conceptual diversity.
 * * CONCLUSION:
 * The existence of Universal Grammar as a "Mountain" underneath the 
 * linguistic "Rope" suggests that while our *lexicon* may be a Snare, 
 * our *syntax* might be a shared global Mountain.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_sapir_whorf_hypothesis].
 * 2. Multi-perspective: ?- multi_index_report(sapir_whorf_hypothesis).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
