% ============================================================================
% CONSTRAINT STORY: silicon_lexicon_overload
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 1.5 Pro
% Source: The "Corporate-Cyberpunk" linguistic shift in the 21st Century
% ============================================================================

:- module(silicon_lexicon_overload, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% The Structural Anchor for the L-OS install
narrative_ontology:interval(silicon_lexicon_overload, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: silicon_lexicon_overload
 * human_readable: The Silicon Lexicon (Linguistic L-OS)
 * domain: technological/corporate/neurolinguistic
 * temporal_scope: Post-2010 CE (The "Disruption" Era)
 * spatial_scope: Global (Digital/Corporate)
 * * SUMMARY:
 * Tech-Speak (e.g., "bandwidth," "alignment," "hard-coded") has moved from 
 * engineering jargon to a mandatory social protocol. It acts as a "Noose" 
 * that translates human effort into machine-readable "output." If you don't 
 * speak the "Build," you are "deprecated."
 * * KEY AGENTS:
 * - The Stack-Architect (CEO): Redefining reality through "Disruption."
 * - The Ghost-in-the-Machine (Gig Worker): Forced to optimize their "bandwidth."
 * - The Glitch-Hunter (Cultural Critic): Observing the erosion of non-technical thought.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: High (0.8)
% This lexicon extracts human nuances (empathy, rest) and rebrands them 
% as "uptime" or "synergy" for corporate gain.
domain_priors:base_extractiveness(silicon_lexicon_overload, 0.8).

% Suppression: High (0.7)
% Speaking in "Analog" terms in a "Digital" workplace leads to social 
% and professional "un-indexing."
domain_priors:suppression_score(silicon_lexicon_overload, 0.7).

% Enforcement: Requires active enforcement (via Performance Reviews/Slack)
domain_priors:requires_active_enforcement(silicon_lexicon_overload).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STACK-ARCHITECT (CEO) - ROPE
   --------------------------------------------------------------------------
   WHO: institutional - The "Root Admin" of the corporate culture.
   WHEN: immediate - Navigating the next "Pivot."
   WHERE: arbitrage - Can swap "stacks" or languages at will.
   SCOPE: global - Their "Vision" spans the network.
   
   WHY THIS CLASSIFICATION:
   To the CEO, Tech-Speak is a Rope. It is a high-tensile tool used to 
   coordinate thousands of units (employees) toward a single "Output." 
   It allows them to bypass traditional cultural friction and "deploy" 
   change instantly across the organization.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    silicon_lexicon_overload,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GIG-WORKER (COURIER) - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_powerless - Bound by the algorithm.
   WHEN: biographical - Their "Runtime" is limited.
   WHERE: trapped - The "Digital Commons" is the only place to find work.
   SCOPE: local - Their immediate "Node."
   
   WHY THIS CLASSIFICATION:
   The Lexicon is a Noose. When the worker is told they lack "bandwidth" 
   for a sick day, the language is being used as a coercive weapon to 
   strip away their humanity. They are forced to use the language of their 
   own obsolescence.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    silicon_lexicon_overload,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BORN-DIGITAL (Gen Alpha) - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: individual_moderate - Navigating the only world they've ever known.
   WHEN: generational - This is the "Base Code."
   WHERE: mobile - Comfortable in the "Metaverse," but cannot imagine "Analog."
   SCOPE: global - The borderless internet.
   
   WHY THIS CLASSIFICATION:
   For the child born into the "Stack," this language is a Mountain. It 
   is not "tech-speak"; it is just "speech." They don't "decide" to 
   call a friendship a "connection"—that is simply the natural law of 
   the digital environment they inhabit.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    silicon_lexicon_overload,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(silicon_lexicon_tests).

test(algorithmic_noose) :-
    % Test that powerless agents under high extractiveness perceive a Noose
    constraint_indexing:constraint_classification(silicon_lexicon_overload, noose, context(agent_power(individual_powerless), _, _, _)).

test(generational_firmware) :-
    % Test that generational time horizons bake the language into a Mountain
    constraint_indexing:constraint_classification(silicon_lexicon_overload, mountain, context(_, time_horizon(generational), _, _)).

test(exit_option_power_gap) :-
    % Test the gap between the "Arbitrage" CEO and the "Trapped" Worker
    constraint_indexing:constraint_classification(silicon_lexicon_overload, rope, context(_, _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(silicon_lexicon_overload, noose, context(_, _, exit_options(trapped), _)).

:- end_tests(silicon_lexicon_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. THE "NOOSE" OF OPTIMIZATION: In the KJV era, the Noose was "Heresy." 
 * In the Cyberpunk era, the Noose is "Inefficiency." The language 
 * itself enforces this by removing the words for "non-productive time."
 * * 2. THE MOUNTAIN OF THE NATIVE: The most dangerous phase of a Noose 
 * is when it becomes a Mountain—when people stop realizing they are 
 * speaking like machines and start thinking they *are* machines.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE: "Human-Centric" or "Poetic" Language
 * Viability: Low in professional contexts (seen as "unprofessional" or "vague").
 * Suppression: Active. Using "poetic" language in a Jira ticket is 
 * flagged as a "blocker" or "noise."
 * * CONCLUSION:
 * The Tech-Lexicon is a "Hard-Coded" Noose because it actively de-platforms 
 * any alternative way of describing human experience.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
