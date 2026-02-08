% ============================================================================
% CONSTRAINT STORY: framing_effect
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Tversky & Kahneman (1981) / Behavioral Economics
% ============================================================================

:- module(constraint_framing_effect, []).

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
 * * constraint_id: framing_effect
 * human_readable: The Framing Effect
 * domain: social/political/psychological
 * temporal_scope: Permanent (Cognitive Architecture)
 * spatial_scope: Global (Human Decision Making)
 * * SUMMARY:
 * The framing effect is a cognitive bias where people decide on options based on 
 * whether the options are presented with positive or negative connotations; 
 * e.g. as a loss or as a gain. It represents a fundamental limit on "rational" 
 * decision-making, where the presentation of information dictates the choice.
 * * KEY AGENTS:
 * - The Behavioral Scientist: Analytical observer mapping the "Mountain" of 
 * human irrationality and cognitive shortcuts.
 * - The Campaign Strategist: Institutional agent using language as a "Rope" 
 * to coordinate public opinion and behavior through specific frames.
 * - The Subjected Voter: Individual powerless agent whose choice is "strangled" 
 * by the emotional weight of a loss-frame, leading to suboptimal outcomes.
 * * NARRATIVE ARC:
 * To navigate complexity, the brain uses heuristics that act as a "Mountain"—a 
 * fixed feature of human biology. For the architect of choice, these heuristics 
 * are a "Rope" for influence. However, for the person being nudged, the frame 
 * becomes a "Snare," as the emotional manipulation extracts their capacity 
 * for objective reasoning.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(framing_effect_interval, 0, 10).
narrative_ontology:constraint_claim(framing_effect, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). The effect extracts "objective utility" and 
% "rational agency." It redirects the energy of a decision toward the 
% beneficiary's desired outcome via psychological pressure.
domain_priors:base_extractiveness(framing_effect, 0.5).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.5). It suppresses "Neutral Information." Once a 
% frame is established (e.g., "90% lean" vs "10% fat"), the objective 
% data point is conceptually suppressed by the emotive connotations.
domain_priors:suppression_score(framing_effect, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(framing_effect, extractiveness, 0.5).
narrative_ontology:constraint_metric(framing_effect, suppression_requirement, 0.5).

% Enforcement requirements
% Emerges naturally from human evolutionary psychology and dual-process theory.
domain_priors:emerges_naturally(framing_effect).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
% Marketers and political strategists benefit from the predictable reactivity.
constraint_beneficiary(framing_effect, [marketers, political_strategists]).
% Consumers and voters suffer the loss of objective choice.
constraint_victim(framing_effect, [voters, consumers]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COGNITIVE SCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the biological/psychological substrate.
   WHEN: civilizational - Viewing the bias as an evolved human constant.
   WHERE: trapped - Logic and perception are filtered by the brain's hardware.
   SCOPE: global - Universal to the species.
   
   WHY THIS CLASSIFICATION:
   To the scientist, the framing effect is a Mountain. It is an unchangeable 
   consequence of how the human mind processes risk and value. No amount of 
   "education" removes the underlying neurological reactivity; it is a 
   fixed peak in the landscape of human nature.
   
   NARRATIVE EVIDENCE:
   Studies consistently show that even experts are subject to framing bias 
   when stakes are high (e.g., medical treatment choices).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    framing_effect,
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
   PERSPECTIVE 2: THE ADVERTISING EXECUTIVE - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design choice environments.
   WHEN: biographical - Managing campaigns and brand perception over a career.
   WHERE: arbitrage - Can shift frames to achieve desired conversion metrics.
   SCOPE: national - Large-scale consumer markets.
   
   WHY THIS CLASSIFICATION:
   For the marketer, framing is a Rope. It is a coordination mechanism used 
   to "pull" consumer behavior toward a purchase. It is a functional tool 
   that allows them to communicate value in a way that resonates 
   psychologically, ensuring the success of the business.
   
   NARRATIVE EVIDENCE:
   "We don't sell steak, we sell the sizzle." "Frame the cost as 'only 
   cents a day' to make it accessible."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    framing_effect,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(framing_effect, E),
    E < 0.7, 
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CONFUSED CONSUMER - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to the market's choice architecture.
   WHEN: immediate - Today's buying decision or vote.
   WHERE: constrained - High cost/effort to find and verify neutral data.
   SCOPE: local - Immediate decision environment.
   
   WHY THIS CLASSIFICATION:
   For the individual, a deceptive frame is a Snare. They are presented 
   with choices that feel intuitive but lead to objectively worse outcomes 
   (e.g., choosing a higher-cost insurance plan because it's framed as 
   "protection"). The frame strangles their rational autonomy, extracting 
   their wealth or health.
   
   NARRATIVE EVIDENCE:
   "I felt I had to choose the 'extended warranty' because the salesperson 
   framed it as a way to avoid 'catastrophic loss,' even though it was a 
   bad deal."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    framing_effect,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(framing_effect, E),
    E > 0.4, 
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(framing_effect_tests).

test(multi_perspective_variance) :-
    % Scientist sees Mountain, Marketer sees Rope, Consumer sees Snare
    constraint_indexing:constraint_classification(framing_effect, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(framing_effect, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(framing_effect, snare, context(powerless, immediate, constrained, local)).

test(power_extractiveness_bias) :-
    % Powerless individuals feel the total extraction of their rationality (Snare).
    % Institutional actors use the bias to coordinate behavior (Rope).
    ContextPowerless = context(powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(framing_effect, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(framing_effect, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_framing) :-
    % Over historical/civilizational time, it is a fixed feature (Mountain).
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(framing_effect_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.5):
 * Reasoning: Framing extracts "Agency." By manipulating the 
 * connotations of a choice, the beneficiary extracts the 
 * subject's time, money, or loyalty without their full awareness.
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Scientist (Law), the Marketer (Utility), and the 
 * Voter/Consumer (Victim) to highlight how a cognitive "fact" 
 * becomes a structural "trap" in social systems.
 * * 3. CLASSIFICATION RATIONALE:
 * Analytical → Mountain: It is a law of human cognitive architecture.
 * Institutional → Rope: It is a primary tool for "Choice Architecture."
 * Powerless → Snare: It is a psychological trap that strangles autonomy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debiasing_efficacy,
    "Can 'debiasing' training effectively 'untie' the Snare (Rope), or is 
    the effect fundamentally a Mountain for all human agents?",
    resolution_mechanism("Long-term studies of expert decision-makers 
    trained in cognitive bias mitigation vs. control groups"),
    impact("If Rope: Consumers can be protected via education. If Mountain: 
    Institutional regulation (Ropes) is the only solution."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Neutral Data Presentation (Raw Statistics)
 * Viability: High. Presenting data without emotive language (e.g., a 
 * simple table) allows for more objective analysis.
 * Suppression: High. Marketers and politicians actively reject neutral 
 * frames because they are less effective at driving behavior.
 * * CONCLUSION:
 * The existence of suppressed neutral alternatives (Raw Data) confirms that 
 * for the individual, the Framing Effect is often a Snare used by choice 
 * architects to prevent rational arbitrage.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_framing_effect].
 * 2. Multi-perspective: ?- multi_index_report(framing_effect).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
