% ============================================================================
% CONSTRAINT STORY: tractarian_logic_limit
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Tractatus Logico-Philosophicus (Ludwig Wittgenstein)
% ============================================================================

:- module(constraint_tractarian_logic_limit, []).

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
 * 
 * constraint_id: tractarian_logic_limit
 * human_readable: The Limits of Language (Tractatus)
 * domain: philosophical/logical
 * temporal_scope: 1921 - Present
 * spatial_scope: Universal / Abstract Logic
 * 
 * SUMMARY:
 * The world consists of facts in logical space. Language is a "picture" of those facts. 
 * Any attempt to use language outside this scope (e.g., to discuss ethics, aesthetics, 
 * or metaphysics) hits a hard limit, resulting in "nonsense."
 * 
 * KEY AGENTS:
 * - The Speaking Subject (Individual Powerless): An agent whose world is limited by their language.
 * - The Vienna Circle (Institutional): A philosophical movement that adopted the Tractatus as a core text.
 * - The Metaphysician (Individual Powerful): A traditional philosopher whose domain is declared "unspeakable."
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(tractarian_logic_limit, 0, 10).
narrative_ontology:constraint_claim(tractarian_logic_limit, mountain).

% Base extractiveness: 0.7 (High)
% Rationale: TLP extracts almost all traditional philosophical territory 
% (Ethics, Religion, Aesthetics) and reclassifies it as "nonsense."
domain_priors:base_extractiveness(tractarian_logic_limit, 0.7).

% Suppression score: 0.95 (High)
% Rationale: Metaphysical language is not merely discouraged; it is logically 
% impossible to formulate as a valid "picture" of the world under this system.
domain_priors:suppression_score(tractarian_logic_limit, 0.95).

% Enforcement: Emerges naturally from the proposed logical form of language.
domain_priors:emerges_naturally(tractarian_logic_limit).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(tractarian_logic_limit, natural_science).
constraint_victim(tractarian_logic_limit, metaphysicians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SPEAKING SUBJECT - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless - The subject whose world is "my language."
   WHEN: civilizational - A permanent boundary of human cognition.
   WHERE: trapped - "The limits of my language mean the limits of my world."
   
   WHY THIS CLASSIFICATION:
   For the agent inside language, the logical form is a 'Mountain.' It is an 
   unchangeable fact of reality. One cannot "negotiate" with logic or choose 
   to see facts differently than logic allows.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tractarian_logic_limit,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE VIENNA CIRCLE - Rope
   --------------------------------------------------------------------------
   WHO: institutional (A philosophical movement)
   WHEN: biographical (The lifespan of the movement)
   WHERE: mobile (Could choose which philosophical tenets to adopt)
   
   WHY THIS CLASSIFICATION:
   For the logical positivists of the Vienna Circle, the Tractatus was a 'Rope'.
   It was a powerful tool to "clean up" philosophy, eliminate metaphysics, and
   establish a new, rigorous standard for meaningful discourse based on science
   and logic.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tractarian_logic_limit,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ETHICIST / METAPHYSICIAN - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerful - The traditional authority on "Value."
   WHEN: immediate - Confronting the total loss of discursive legitimacy.
   WHERE: constrained - Prevented from defining "Good" or "God."
   
   WHY THIS CLASSIFICATION:
   To the metaphysician, the Tractatus is a 'Snare.' It is a coercive logical 
   maneuver that extracts their professional status by declaring their entire 
   field "unspeakable." It is an asymmetric suppression of "Value" in favor of "Fact."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tractarian_logic_limit,
    snare,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(tractarian_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(tractarian_logic_limit, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(tractarian_logic_limit, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(tractarian_logic_limit, Type3, context(agent_power(individual_powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(tractarian_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'Vienna Circle' as the institutional
 *    agent. This shows how a philosophical text can be adopted as a programmatic
 *    'Rope' to build a new intellectual movement.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Subject (Mountain): The limits of logic are inescapable.
 *    - Vienna Circle (Rope): A tool to cleanse philosophy.
 *    - Metaphysician (Snare): A weapon that delegitimizes their entire field.
 * 
 * 3. EXTRACTIVENESS (0.7): High, because the TLP's goal is to "solve all problems"
 *    by removing the right to ask most of them, effectively extracting agency
 *    from traditional philosophical inquiry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Core uncertainties about the nature of language and logic presented in the work.
 */

omega_variable(
    mystical_interaction,
    "How does the 'Mystical' (ethics, aesthetics) show itself if it is logically excluded from the world of speakable facts?",
    resolution_mechanism("Requires a recursive analysis of Wittgenstein's later transition to 'Language Games', which abandons this strict limit."),
    impact("If the Mystical cannot be shown: TLP is a perfect Mountain. If it can: TLP is a self-contradictory Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Ordinary Language Philosophy (Later Wittgenstein)
 *    Viability: High. This became Wittgenstein's own alternative, viewing language as a
 *    social practice ('Rope') rather than a fixed logical mirror ('Mountain').
 *    Suppression: In the TLP, this view is suppressed by the insistence on a single,
 *    underlying logical form for all meaningful speech.
 * 
 * CONCLUSION:
 * The TLP's existence as a 'Snare' for traditional philosophy depends entirely 
 * on its successful suppression of other models of language. When Wittgenstein 
 * himself later adopted the alternative, the 'Snare' of the TLP dissolved.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/tractarian_logic_limit].
 * 2. Multi-perspective: ?- multi_index_report(tractarian_logic_limit).
 * 3. Run tests: ?- run_tests(tractarian_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */