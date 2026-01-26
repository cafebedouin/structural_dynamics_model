% ============================================================================
% CONSTRAINT STORY: dark_patterns_manipulation
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Brignull, H. (2010). Dark Patterns. / FTC Enforcement Guidelines.
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_dark_patterns_manipulation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * constraint_id: dark_patterns_manipulation
 * human_readable: Dark Patterns (Interface Coercion)
 * domain: technological/economic/psychological
 * temporal_scope: Immediate
 * spatial_scope: Global
 * 
 * SUMMARY:
 * Dark patterns exploit cognitive biases (like loss aversion or the default 
 * effect) to steer user behavior toward outcomes that benefit the platform 
 * but harm the user. They are the "shadow side" of Information Foraging Theory,
 * turning the interface from a transparent tool into a psychological Snare.
 * 
 * KEY AGENTS:
 * - The User (Individual Powerless): The "forager" whose cognitive heuristics are being weaponized.
 * - The Growth Hacker (Institutional): The designer tasked with maximizing "conversion" at any cost.
 * - The Ethical Designer (Analytical): The agent attempting to design transparent interfaces.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(dark_patterns_manipulation, 0, 10).
narrative_ontology:constraint_claim(dark_patterns_manipulation, snare).

% Base extractiveness score (0.85)
% High; they are purely designed to extract value (money, data, attention) 
% through deception rather than exchange.
domain_priors:base_extractiveness(dark_patterns_manipulation, 0.85).

% Suppression score (0.9)
% High; the "Exit Option" (e.g., the 'Cancel Subscription' button) is 
% intentionally hidden or made difficult to find.
domain_priors:suppression_score(dark_patterns_manipulation, 0.9).

% Enforcement: Requires active enforcement by the platform's design.
domain_priors:requires_active_enforcement(dark_patterns_manipulation).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(dark_patterns_manipulation, institutional).
constraint_victim(dark_patterns_manipulation, individual_powerless).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CAPTIVE CUSTOMER - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped (in a 'Roach Motel' design)
   
   WHY THIS CLASSIFICATION:
   For a user trying to delete an account, the interface is a "Snare." 
   The "Confirm Deletion" button is grayed out, hidden behind five menus, 
   or requires a physical phone call. The platform strangles their 
   exit option to keep their "active user" metric high.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dark_patterns_manipulation,
    snare,
    context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PRODUCT MANAGER / GROWTH HACKER - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: biographical (quarterly targets)
   WHERE: mobile (can A/B test different patterns)
   
   WHY THIS CLASSIFICATION:
   To the product manager, these patterns are a "Rope." They are the 
   coordination tools used to "nudge" users toward the "desired" 
   business outcome (e.g., higher subscription retention). They see it as a 
   necessary strategy for survival in a hyper-competitive attention economy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dark_patterns_manipulation,
    rope,
    context(agent_power(institutional), time_horizon(biographical), exit_options(mobile), spatial_scope(regional))
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ETHICAL DESIGNER - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   
   WHY THIS CLASSIFICATION:
   The ethical observer sees the potential for these patterns as a "Mountain" of 
   psychological vulnerability. Human cognitive biases (loss aversion, default bias) 
   are immutable. The fact that they *can* be exploited is a feature of our biological 
   nature that designers must either respect or abuse.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dark_patterns_manipulation,
    mountain,
    context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(dark_patterns_manipulation_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(dark_patterns_manipulation, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(dark_patterns_manipulation, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(dark_patterns_manipulation, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(biographical), exit_options(mobile), spatial_scope(regional)),
    constraint_indexing:extractiveness_for_agent(dark_patterns_manipulation, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(dark_patterns_manipulation, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(dark_patterns_manipulation_tests).

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
 * 1. EXTRACTIVENESS SCORE (0.85): This is high because dark patterns are, by
 *    definition, deceptive and non-reciprocal. They extract user value (data,
 *    money, attention) without providing commensurate service in return.
 *
 * 2. MANDATROPHY STATUS: The high extraction is 'RESOLVED' because the perspectives
 *    show a clear power dynamic. The institutional agent wields the 'Rope' to achieve
 *    targets, which becomes a 'Snare' for the powerless user. The 'Mountain' is the
 *    underlying, unchangeable human psychology that makes it all possible.
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    - User (Snare): Trapped in an interface designed to prevent them from leaving.
 *    - Product Manager (Rope): A tool to achieve business goals.
 *    - Ethical Designer (Mountain): The cognitive biases being exploited are a fixed aspect of human nature.
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
    dark_patterns_intent,
    "Is the user harm caused by dark patterns an intended predatory outcome or an unforeseen consequence of aggressive optimization?",
    resolution_mechanism("Internal review of A/B testing documentation and executive communications to determine if user harm was a known and accepted trade-off."),
    impact("If intended: Confirms a predatory Snare/Mandatrophy. If unforeseen: It's a recklessly handled Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Ethical / Transparent Design
 *    Viability: Highly viable. Many successful products use clear, honest user interfaces.
 *    Suppression: Suppressed by a corporate culture that prioritizes short-term growth metrics (like 'user retention' or 'conversion') over long-term user trust. The "growth hacker" mindset actively suppresses ethical considerations that might lower these numbers.
 * 
 * CONCLUSION:
 * The active suppression of transparent design in favor of manipulative patterns is the core of this constraint. It proves that the 'Snare' experienced by the user is not an accident, but a choice made by the institutional actor.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/dark_patterns_manipulation].
 * 2. Multi-perspective: ?- multi_index_report(dark_patterns_manipulation).
 * 3. Run tests: ?- run_tests(dark_patterns_manipulation_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */