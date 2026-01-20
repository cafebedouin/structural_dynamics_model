% ============================================================================
% CONSTRAINT STORY: availability_heuristic
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Tversky, A., & Kahneman, D. (1973) / Behavioral Economics
% ============================================================================

:- module(constraint_availability_heuristic, []).

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
 * * constraint_id: availability_heuristic
 * human_readable: Availability Heuristic
 * domain: social/cognitive/economic
 * temporal_scope: Permanent (Human Evolution)
 * spatial_scope: Global (Human Decision Systems)
 * * SUMMARY:
 * The availability heuristic is a mental shortcut that relies on immediate 
 * examples that come to a given person's mind when evaluating a specific 
 * topic, concept, method or decision. It operates on the principle that 
 * if something can be recalled, it must be important, or at least more 
 * important than alternative solutions which are not as readily recalled.
 * * KEY AGENTS:
 * - The Instinctive Actor: Relies on "gut feelings" shaped by recent or 
 * vivid memories.
 * - The Media Architect: Shapes the "availability" of information through 
 * selective, vivid, and frequent reporting.
 * - The Risk Analyst: Attempts to use base-rate data to override the 
 * heuristic's biases.
 * * NARRATIVE ARC:
 * The heuristic functions as a "Mountain" of biological shortcutting. For 
 * the individual, it is a "Rope" providing rapid, low-energy decision-making 
 * in safe environments. However, in modern information-dense societies, it 
 * becomes a "Noose," as vivid but rare events (e.g., plane crashes) strangle 
 * rational risk assessment compared to common but dull threats (e.g., heart 
 * disease).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(availability_interval, 0, 10).
narrative_ontology:constraint_claim(availability_heuristic, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: It extracts "rational accuracy" and "long-term utility." It 
% benefits media entities and actors who leverage "vividness" to capture 
% attention and influence behavior.
domain_priors:base_extractiveness(availability_heuristic, 0.4).

% Suppression score (0.0-1.0)
% Rationale: It suppresses "statistical base rates" and "dull facts." The 
% brain naturally ignores what isn't vivid, making the alternative (data) 
% effectively invisible without high cognitive effort.
domain_priors:suppression_score(availability_heuristic, 0.7).

% Enforcement requirements
domain_priors:emerges_naturally(availability_heuristic).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(availability_heuristic, extractiveness, 0.4).
narrative_ontology:constraint_metric(availability_heuristic, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(availability_heuristic, news_media).
constraint_beneficiary(availability_heuristic, sensationalist_politicians).
constraint_victim(availability_heuristic, public_health_policy).
constraint_victim(availability_heuristic, rational_decision_makers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COGNITIVE PSYCHOLOGIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The objective observer)
   WHEN: civilizational (Observing an evolved cognitive trait)
   WHERE: trapped (Neural architecture is fixed)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the psychologist, this is a Mountain. It is an unchangeable feature 
   of the human "system 1" (fast thinking). It is a physical limit on 
   how information is retrieved and weighted in the human brain.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    availability_heuristic,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ADVERTISING EXECUTIVE - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-shaping power in the attention economy)
   WHEN: biographical (Achieving sales targets over a career)
   WHERE: arbitrage (Can move between different campaigns and media)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the executive, the heuristic is a Rope. It is a coordination mechanism 
   used to "top-load" a brand into a consumer's mind. By ensuring their 
   product is the "most available" memory through repetition and vivid 
   imagery, they pull the consumer toward a purchase decision.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    availability_heuristic,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANXIOUS CITIZEN - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A subject of the information environment)
   WHEN: immediate (Current state of fear or anxiety)
   WHERE: constrained (Cannot easily escape the "vividness" of news feeds)
   SCOPE: stand_alone
   
   WHY THIS CLASSIFICATION:
   For the citizen bombarded by vivid images of rare disasters, the heuristic 
   is a Noose. It strangles their ability to live a normal life or make 
   sane risks (like flying vs. driving). They are trapped by the salience 
   of the "last scary thing they saw," which extracts their peace of mind.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    availability_heuristic,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(availability_heuristic_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(availability_heuristic, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(availability_heuristic, T2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(availability_heuristic, T3, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(vividness_suppression_scaling) :-
    % Heuristic relies on the suppression of dull, statistical alternatives.
    domain_priors:suppression_score(availability_heuristic, Score),
    Score >= 0.6.

test(time_immutability) :-
    % Evolutionary traits are viewed as Mountains on civilizational scales.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(availability_heuristic_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION SCORE (0.7): High, because the "Ease of Recall" acts as 
 * a biological block against slower, data-driven reasoning.
 * 2. EXTRACTIVENESS (0.4): It extracts attention and "correctness." Media 
 * entities benefit from the vividness they provide (Rope), while the 
 * individual suffers distorted risk perception (Noose).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salience_threshold_shift,
    "Does the 'Vividness' threshold required to trigger the heuristic increase 
    in a hyper-visual digital society (desensitization)?",
    resolution_mechanism("Longitudinal study of response rates to disaster imagery across generations"),
    impact("If Yes: The Noose is temporary. If No: The Noose tightens as information volume grows."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Base-Rate Reasoning (System 2 Thinking)
 * Viability: High, but cognitively expensive. Requires training in 
 * statistics and active effort.
 * Suppression: Naturally suppressed by the brain's preference for 
 * low-energy heuristics.
 * * CONCLUSION:
 * The availability heuristic is a Mountain of the human condition, but it 
 * is a Noose specifically because modern institutions (media/advertising) 
 * weaponize it to suppress the Rope of statistical reality.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_availability_heuristic].
 * 2. Multi-perspective: ?- multi_index_report(availability_heuristic).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
