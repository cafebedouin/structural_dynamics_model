% ============================================================================
% CONSTRAINT STORY: silicon_lexicon_overload
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: The "Corporate-Cyberpunk" linguistic shift in the 21st Century
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_silicon_lexicon_overload, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * constraint_id: silicon_lexicon_overload
 * human_readable: The Silicon Lexicon (Corporate Tech-Speak)
 * domain: linguistic/technological/corporate
 * temporal_scope: Post-2010 CE (The "Disruption" Era)
 * spatial_scope: Global (Digital/Corporate)
 * 
 * SUMMARY:
 * Tech-speak (e.g., "bandwidth," "alignment," "hard-coded") has moved from 
 * engineering jargon to a mandatory social protocol in corporate environments.
 * It acts as a 'Snare' that translates human experience into machine-readable
 * "output." If you don't speak the "Build," you are "deprecated."
 * 
 * KEY AGENTS:
 * - The Stack-Architect (Institutional): The CEO or manager redefining reality through "Disruption."
 * - The Gig Worker (Individual Powerless): Forced to optimize their "bandwidth" and "uptime."
 * - The Born-Digital Native (Individual Moderate): Inhabits this lexicon as their native tongue.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(silicon_lexicon_overload, 0, 10).
narrative_ontology:constraint_claim(silicon_lexicon_overload, snare).

% Base extractiveness: High (0.8)
% This lexicon extracts human nuances (empathy, rest, intuition) and rebrands them 
% as "synergy" or "low bandwidth" for corporate gain.
domain_priors:base_extractiveness(silicon_lexicon_overload, 0.8).

% Suppression: High (0.7)
% Speaking in "Analog" or poetic terms in a "Digital" workplace leads to social 
% and professional "un-indexing" and is seen as inefficient.
domain_priors:suppression_score(silicon_lexicon_overload, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(silicon_lexicon_overload, extractiveness, 0.8).
narrative_ontology:constraint_metric(silicon_lexicon_overload, suppression_requirement, 0.7).

% Enforcement: Requires active enforcement via performance reviews, Slack, and meetings.
domain_priors:requires_active_enforcement(silicon_lexicon_overload).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(silicon_lexicon_overload, corporate_management).
constraint_victim(silicon_lexicon_overload, employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STACK-ARCHITECT (CEO) - Rope
   --------------------------------------------------------------------------
   WHO: institutional - The "Root Admin" of the corporate culture.
   WHEN: immediate - Navigating the next "Pivot."
   WHERE: arbitrage - Can swap "stacks" or languages at will.
   
   WHY THIS CLASSIFICATION:
   To the CEO, Tech-Speak is a 'Rope'. It is a high-tensile tool used to 
   coordinate thousands of employees toward a single "Output." It allows them
   to bypass traditional cultural friction and "deploy" change instantly.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GIG-WORKER - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Bound by the algorithm and its language.
   WHEN: biographical - Their "Runtime" as a productive worker is limited.
   WHERE: trapped - The "Digital Commons" is the only place to find work.
   
   WHY THIS CLASSIFICATION:
   The lexicon is a 'Snare'. When the worker is told they lack "bandwidth" 
   for a sick day, the language is a coercive weapon that strips away their
   humanity. They are forced to use the language of their own obsolescence.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    silicon_lexicon_overload,
    snare,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BORN-DIGITAL (GEN ALPHA) - Mountain
   --------------------------------------------------------------------------
   WHO: individual_moderate - Navigating the only world they've ever known.
   WHEN: generational - This is the "Base Code" of their social reality.
   WHERE: mobile - Comfortable in the "Metaverse," but cannot imagine "Analog" thought.
   
   WHY THIS CLASSIFICATION:
   For the child born into the "Stack," this language is a 'Mountain'. It is not 
   "tech-speak"; it is just "speech." They don't "decide" to call a friendship
   a "connection"—that is simply the natural law of the digital environment.
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
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(silicon_lexicon_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(silicon_lexicon_overload, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(silicon_lexicon_overload, Type2, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silicon_lexicon_overload, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(silicon_lexicon_tests).

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
 * 1. MANDATROPHY STATUS: The 0.8 extractiveness is 'RESOLVED' by showing the
 *    asymmetry. For the CEO, the language is a 'Rope' for efficiency. For the
 *    worker, it's a 'Snare' that re-engineers their humanity for corporate needs.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - CEO (Rope): A tool for control and coordination.
 *    - Worker (Snare): A tool of coercion and dehumanization.
 *    - Native (Mountain): The unquestioned, natural way of speaking and thinking.
 *
 * 3. CORE INSIGHT: The most dangerous phase of a 'Snare' is when it becomes a
 *    'Mountain' for the next generation—when people stop realizing they are
 *    speaking like machines and start thinking they *are* machines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    lexicon_intent,
    "Is the adoption of the Silicon Lexicon a deliberate strategy to increase productivity and control (Snare), or a natural evolution of language in a technological age (Mountain)?",
    resolution_mechanism("Linguistic analysis of corporate communications vs. organic online discourse to track the origin and spread of specific jargon."),
    impact("If deliberate: It's a tool of social engineering. If natural: It's an emergent feature of our time."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: "Human-Centric" or Poetic Language
 *    Viability: Low in most modern professional contexts, where it is often seen as "unprofessional," "inefficient," or "vague."
 *    Suppression: High. Using "poetic" or emotionally nuanced language in a Jira ticket or a performance review is actively discouraged and flagged as a "blocker" or "noise."
 *
 * CONCLUSION:
 * The Silicon Lexicon functions as a "Hard-Coded" 'Snare' because it actively
 * de-platforms alternative ways of describing human experience, enforcing a
 * machine-like view of productivity and existence.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/silicon_lexicon_overload].
 * 2. Multi-perspective: ?- multi_index_report(silicon_lexicon_overload).
 * 3. Run tests: ?- run_tests(silicon_lexicon_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */