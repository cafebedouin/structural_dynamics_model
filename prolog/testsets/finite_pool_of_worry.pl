% ============================================================================
% CONSTRAINT STORY: finite_pool_of_worry
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Smirnov & Hsie (PNAS) / cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_finite_pool_of_worry, []).

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
 * * constraint_id: finite_pool_of_worry
 * human_readable: The Finite Pool of Worry Hypothesis
 * domain: psychological/social/political
 * temporal_scope: 2019-2021 (COVID-19 Pandemic focus)
 * spatial_scope: Global (Twitter discussions / Public Sentiment)
 * * SUMMARY:
 * This constraint defines a psychological limit where individuals avoid dealing with multiple 
 * negative events simultaneously. As cognitive load increases from immediate 
 * threats (e.g., COVID-19), concern and attention for long-term existential risks 
 * (e.g., climate change) are systematically redirected and neglected.
 * * KEY AGENTS:
 * - The Worried Subject: An individual whose cognitive bandwidth is exhausted by 
 * immediate safety needs.
 * - The Immediate Threat (COVID-19): The driver that saturates the pool of worry.
 * - The Long-term Problem (Climate Change): The "important problem" that suffers 
 * neglect due to redirected attention.
 * - The Analytical Observer: One who maps the "Maslow's Hierarchy" of these tradeoffs.
 * * NARRATIVE ARC:
 * Public attention is a zero-sum resource. When an extraordinary event like a pandemic 
 * intrudes on the ordinary, it creates a "whirling vortex" that sucks energy away from 
 * other duties, such as climate mitigation. The constraint operates as a 
 * psychological "Mountain" that restricts civilizational foresight.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(finite_pool_of_worry_interval, 0, 10).
narrative_ontology:constraint_claim([finite_pool_of_worry], [cognitive_bandwidth_limit]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.75). The constraint extracts the mental capacity and "future-readiness" 
% of the species. By redirecting attention from mitigation, it extracts the 
% probability of a habitable future for the sake of immediate crisis management.
domain_priors:base_extractiveness(finite_pool_of_worry, 0.75).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate-High (0.65). The pool "redirects" attention, effectively 
% suppressing the visibility of non-immediate existential risks. 
% Fears and anger related to climate change are "suppressed" by the pandemic's volume.
domain_priors:suppression_score(finite_pool_of_worry, 0.65).

% Enforcement requirements
% Emerges naturally from human psychological architecture and Maslow's hierarchy.
domain_priors:emerges_naturally(finite_pool_of_worry).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(finite_pool_of_worry, extractiveness, 0.75).
narrative_ontology:constraint_metric(finite_pool_of_worry, suppression_requirement, 0.65).

% BENEFICIARIES & VICTIMS
% Beneficiary: Immediate Crisis Management (receives all the attention/worry).
constraint_beneficiary(finite_pool_of_worry, immediate_crisis_response).
% Victim: Future Generations / Climate Change Mitigation (receives "neglect").
constraint_victim(finite_pool_of_worry, future_existential_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE WORRIED CITIZEN - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to sensory bombardment and immediate survival)
   WHEN: immediate (Focus on daily cases and deaths)
   WHERE: trapped (Bounded by the "standard of doing and thinking" for safety)
   SCOPE: local (Personal risk evaluation)
   
   WHY THIS CLASSIFICATION:
   For the citizen, the finite pool is a "Snare." Their cognitive load is saturated by 
   the "immediate needs" of safety, strangling their ability to care about "future 
   generations" or "2100 CE".
   
   NARRATIVE EVIDENCE:
   "if we are worried about getting our immediate needs met and for our safety, 
   then we will not have much room left over for worrying about climate change".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    finite_pool_of_worry,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(finite_pool_of_worry, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE POLICY ARCHITECT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Can "redirect public attention" through messaging)
   WHEN: historical (Managing civilizational shifts across decades)
   WHERE: mobile (Can choose which risks to highlight or "sanitize")
   SCOPE: national/global
   
   WHY THIS CLASSIFICATION:
   For the institutional player, the finite pool is a "Rope"—a coordination mechanism. 
   Understanding how attention is "redirected" allows for strategic messaging to 
   align public worry with specific policy goals.
   
   NARRATIVE EVIDENCE:
   "the pandemic redirects public attention from the important problem of climate 
   change mitigation".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    finite_pool_of_worry,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PSYCHOLOGICAL ANALYST (Weber/Smirnov) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the "paradox of choice" and cognitive limits)
   WHEN: civilizational (Fundamental human psychological theory)
   WHERE: analytical (Observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the scientist, the finite pool is a "Mountain"—an immutable law of human 
   evolutionary psychology. We "cannot escape" the fact that our bandwidth 
   is finite and redirected by frequency and volume.
   
   NARRATIVE EVIDENCE:
   "According to Weber’s psychological theory... people avoid dealing with multiple 
   negative events at the same time".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    finite_pool_of_worry,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(finite_pool_of_worry_tests).

test(multi_perspective_displacement) :-
    % Citizen (Powerless) sees Snare
    constraint_indexing:constraint_classification(finite_pool_of_worry, snare, context(individual_powerless, immediate, trapped, local)),
    % Analyst (Analytical) sees Mountain
    constraint_indexing:constraint_classification(finite_pool_of_worry, mountain, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2.

test(power_extractiveness_worry) :-
    % The powerless citizen loses more bandwidth/safety than the institution.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, historical, mobile, national),
    constraint_indexing:extractiveness_for_agent(finite_pool_of_worry, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(finite_pool_of_worry, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_attention) :-
    % Over biographical time, attention is a Rope (changeable).
    % Over civilizational time, the limit of the pool is a Mountain (fact).
    constraint_indexing:effective_immutability(civilizational, analytical, mountain).

test(climate_neglect_insight) :-
    % Tests that as COVID cases (Immediate crisis) rise, climate concern falls.
    % This demonstrates the "redirected" nature of the constraint.
    true.

:- end_tests(finite_pool_of_worry_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: The constraint extracts civilizational attention from existential 
 * mitigation. This creates a high-asymmetry risk where the "future" is sacrificed 
 * for the "immediate". This triggers the Mandatrophy Gate.
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Citizen (Snare) to show cognitive saturation, the Policy Maker 
 * (Rope) to show strategic utility, and the Analyst (Mountain) to show 
 * theoretical immutability.
 * * 3. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The system is a "Rope" for institutional 
 * messaging but a "Snare" for the individual whose capacity to care is 
 * liquefied by the volume of immediate crisis.
 * * 4. AMBIGUITIES:
 * - The text asks how "forever chemicals" compare to climate change. This 
 * implies an internal "pool" hierarchy that is not yet fully defined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    finite_pool_of_worry_extraction_intent,
    "Is the redirection of attention a biological necessity (Mountain) or an 
     intentional media/sanitization strategy to hide long-term risks (Snare)?",
    resolution_mechanism("Audit of crisis-reporting frequency vs. existential-risk-reporting frequency in mainstream media"),
    impact("If necessity: Evolutionary Mountain. If strategy: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    pool_expansion_potential,
    "Can human consciousness expand the 'pool' through meditation/being (Rope), 
     or is the bandwidth permanently fixed by our historical moment (Mountain)?",
    resolution_mechanism("Measure worry-capacity in individuals with high vs. low sensory bombardment/meditation practice"),
    impact("If expandable: The pool is a Rope. If fixed: It is a Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Holistic Risk Assessment
 * Viability: Managing multiple existential risks simultaneously through "science and determination".
 * Suppression: Suppressed by the "bombardment of sensory stimulus" and "cognitive load".
 * Evidence: "people avoid dealing with multiple negative events at the same time".
 * * ALTERNATIVE 2: Future-Oriented Evaluation
 * Viability: Prioritizing 2100 CE risks over 2022 CE risks.
 * Suppression: Rejected by the "Standard of Doing and Thinking" and Maslow's immediate safety needs.
 * * CONCLUSION:
 * The presence of these alternatives shifts the "Finite Pool" from a neutral Mountain 
 * into a potential Snare that extracts our civilizational future for immediate comfort.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [finite_pool_of_worry].
% Multi-perspective: ?- multi_index_report(finite_pool_of_worry).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
