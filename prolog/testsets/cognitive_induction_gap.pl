% ============================================================================
% CONSTRAINT STORY: cognitive_induction_gap
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "One and One Sometimes Equals Eleven" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_cognitive_induction_gap, []).

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
 * * constraint_id: cognitive_induction_gap
 * human_readable: The Induction Gap (Cognitive Compromise)
 * domain: psychological/social
 * temporal_scope: Perennial / Immediate
 * spatial_scope: Global / Cognitive
 * * SUMMARY:
 * This constraint defines the "problem of induction" as a fundamental lack of rational basis for applying 
 * experienced circumstances to new, unexperienced situations. While humans use 
 * "intuitive leaps" to navigate this, factors such as high stress, social suasion, and investment in 
 * outcomes can compromise this ability, leading to a state where the subject is "completely unmoored" 
 * from reality.
 * * KEY AGENTS:
 * - The Decision Maker: An individual attempting an "intuitive leap" to achieve a good outcome.
 * - The Social Group: Individual and group dynamics that exert "social suasion".
 * - The Feedback Auditor: An analytical stance that tracks decision accuracy to identify cognitive compromise.
 * * NARRATIVE ARC:
 * The subject begins with a "reasonable assumption" in one context but fails when abstracting it into 
 * a new, high-stakes, or high-noise environment. The constraint acts as a "Noose" when 
 * stress and social pressure liquefy the subject's judgment, but can be a "Rope" if feedback loops 
 * are maintained.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(cognitive_induction_interval, 0, 10).
narrative_ontology:constraint_claim([cognitive_induction_gap], [cognitive_bias_trap]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.7). Cognitive compromise extracts the subject's ability to see reality 
% for the sake of group cohesion or emotional investment.
domain_priors:base_extractiveness(cognitive_induction_gap, 0.7).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate-High (0.6). "Noise" and "social suasion" suppress essential 
% signal and individual dissent, making the compromise hard to realize.
domain_priors:suppression_score(cognitive_induction_gap, 0.6).

% Enforcement: Emerges naturally from biological stress responses and group dynamics.
domain_priors:emerges_naturally(cognitive_induction_gap).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cognitive_induction_gap, group_cohesion). % Social suasion protects the group
constraint_victim(cognitive_induction_gap, individual_judgment). % Individual becomes "cognitively compromised"

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(cognitive_induction_gap, extractiveness, 0.7).
narrative_ontology:constraint_metric(cognitive_induction_gap, suppression_requirement, 0.6).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STRESSED DECISION MAKER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Slaves to high stress and "noise")
   WHEN: immediate (Must make a decision "quickly")
   WHERE: trapped (No rational basis for induction; unmoored)
   SCOPE: local (The immediate high-stakes situation)
   
   WHY THIS CLASSIFICATION:
   For the individual in a "high stress situation" with "too much information," the constraint is a "Noose." 
   Their cognitive capacity is extracted by pressure and noise, strangling their ability to make 
   effective intuitive leaps.
   
   NARRATIVE EVIDENCE:
   "When the stakes are high, it is easier to make mistakes... it is all noise and/or all signal".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    cognitive_induction_gap,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(cognitive_induction_gap, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PRACTICAL INTUITIVE - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Uses guesswork and guesswork feedback)
   WHEN: biographical (Tracking decisions and outcomes over time)
   WHERE: mobile (Adjusts assumptions based on experience)
   SCOPE: regional (Moving across different life contexts)
   
   WHY THIS CLASSIFICATION:
   For the agent who "tracks how well your decisions do," the gap is a "Rope." It is a 
   functional coordination mechanism that allows them to make intuitive leaps that 
   are "good enough" by using feedback as a navigational tool.
   
   NARRATIVE EVIDENCE:
   "Life presents us with situations where we have to make an intuitive leap that is 
   good enough... track how well your decisions do and get feedback".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_induction_gap,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LOGICIAN/ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the "problem of induction")
   WHEN: civilizational (Perennial epistemological limit)
   WHERE: analytical (Observer stance; "no rational basis")
   SCOPE: global (The nature of the universe)
   
   WHY THIS CLASSIFICATION:
   To the analyst, the induction gap is a "Mountain"—an unchangeable natural law of logic. 
   The fact that there is "no rational basis to argue from circumstances experienced to another 
   not experienced" is a zero-degree-of-freedom reality.
   
   NARRATIVE EVIDENCE:
   "At base, this is the problem of induction. There is no rational basis to argue 
   from circumstances we have experienced to another situation we have not".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    cognitive_induction_gap,
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

:- begin_tests(cognitive_induction_gap_tests).

test(multi_perspective_induction) :-
    % Powerless under stress sees Noose
    constraint_indexing:constraint_classification(cognitive_induction_gap, Type1, context(individual_powerless, immediate, trapped, local)),
    % Pragmatist with feedback sees Rope
    constraint_indexing:constraint_classification(cognitive_induction_gap, Type2, context(individual_moderate, biographical, mobile, regional)),
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(cognitive_induction_gap, Type3, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2, Type2 \= Type3.

test(power_extractiveness_induction) :-
    % Powerless (stressed) experiences higher extraction of judgment accuracy
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, regional),
    constraint_indexing:extractiveness_for_agent(cognitive_induction_gap, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(cognitive_induction_gap, ContextModerate, E2),
    E1 > E2.

test(time_immutability_induction) :-
    % Over immediate/trapped horizons, the error rate is a Mountain/Noose
    constraint_indexing:effective_immutability(immediate, trapped, mountain).

:- end_tests(cognitive_induction_gap_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.7):
 * Reasoning: High because cognitive compromise literally liquidates the individual's 
 * grasp on truth to preserve emotional comfort or social status ("hard to get someone 
 * to see something that their livelihood depends on them not seeing").
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Stressed Subject (Noose), the Practical Agent (Rope), and the Logician 
 * (Mountain) to show how an epistemological fact becomes a survival tool or a 
 * lethal trap depending on stress and feedback.
 * * 3. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. The "extraction" of judgment is 
 * justified by the "Rope" of intuitive leaps for those who maintain feedback loops.
 * * 4. AMBIGUITIES:
 * - The text mentions "social suasion" but doesn't detail if it's intentional or 
 * emergent. Resolved as "emergent naturally" but having high extractive potential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    induction_logic_validity,
    "Is there a yet-undiscovered 'Rope' of rational induction, or is the 
     Mountain of logical isolation truly absolute?",
    resolution_mechanism("Development of a formal system that solves the problem of induction - currently impossible"),
    impact("If Rope exists: Cognitive compromise is a remediable Noose. If Mountain: It is a biological limit."),
    confidence_without_resolution(medium)
).

omega_variable(
    feedback_threshold_efficacy,
    "At what specific 'wrong more often than average' threshold does an agent 
     permanently lose the ability to self-audit (Terminal Noose)?",
    resolution_mechanism("Longitudinal tracking of decision accuracy in high-stress environments"),
    impact("If low threshold: The Noose is lethal. If high: The Rope remains accessible."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Random Chance
 * Viability: Purely stochastic decision making.
 * Suppression: Rejected by the text because "intuitive leaps" are better 
 * than random chance when functioning well.
 * * ALTERNATIVE 2: Scientific/Data-Driven Rigor
 * Viability: Paring down information to what is essential through rigorous method.
 * Suppression: Suppressed by "high stress" and "need to make a decision quickly".
 * * CONCLUSION:
 * The existence of Alternative 2 (Rigor) makes the intuitive failure a "Noose." 
 * For those who cannot access the rigor, the leap is a necessary Rope that 
 * can easily slip into a Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [cognitive_induction_gap].
% Multi-perspective: ?- constraint_indexing:multi_index_report(cognitive_induction_gap).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
