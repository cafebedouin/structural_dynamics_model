% ============================================================================
% CONSTRAINT STORY: bedouin_sedentary_transition
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The Bedouin Way of Life in Rub’ al Khali"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_bedouin_sedentary_transition, []).

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
 * * constraint_id: bedouin_sedentary_transition
 * human_readable: The Bedouin Sedentary Transition
 * domain: social/economic/technological
 * temporal_scope: 1938 - 2030+ (Oil discovery to Vision 2030)
 * spatial_scope: Regional (Rub’ al Khali, Saudi Arabia)
 * * SUMMARY:
 * This constraint represents the shift of the nomadic Bedouin tribes from a resilient, millennium-old pastoral 
 * lifestyle in the hyper-arid Empty Quarter to a dependency-based existence in settled 'markaz' population centers 
 * fueled by oil wealth. It highlights the systemic risk inherent in urban settlements where 
 * specialization increases fragility and "going soft" results in the loss of survival skills.
 * * KEY AGENTS:
 * - Traditional Nomadic Bedouin: Characterized by freedom, hardship, and abstinence as tools for survival.
 * - Settled Bedouin (Markaz Dwellers): Recipients of "trickle down" oil money, housing, and medical care, now 
 * "hampered by possessions" and dependency.
 * - The Saudi State: Institutional provider using oil wealth to consolidate population and manage development.
 * - The Existential Analyst: Observer who identifies sedentary life as a "Black Swan" risk and "fishing net" 
 * for systemic collapse.
 * * NARRATIVE ARC:
 * Following the discovery of oil in 1938, Bedouin tribes were integrated into sedentary markaz settlements. 
 * While providing immediate comfort, this transition created a "Noose" of dependency on a finite resource (oil) 
 * and an interconnected global network, effectively suppressing the traditional nomadic "Rope" of 
 * resilience that had survived for millennia.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bedouin_transition_interval, 0, 10).
narrative_ontology:constraint_claim([bedouin_sedentary_transition], [systemic_dependency]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.75). The sedentary system extracts traditional survival skills and resilience 
% in exchange for temporary comfort, creating a deep asymmetric dependency on oil state budgets.
domain_priors:base_extractiveness(bedouin_sedentary_transition, 0.75).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate-High (0.6). Traditional nomadic strategies are undergoing "extinction" 
% and are shunted in favor of markaz settlements and town-based business.
domain_priors:suppression_score(bedouin_sedentary_transition, 0.6).

% Enforcement requirements
% Requires active enforcement (State budget management and infrastructure maintenance).
domain_priors:requires_active_enforcement(bedouin_sedentary_transition).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(bedouin_sedentary_transition, extractiveness, 0.75).
narrative_ontology:constraint_metric(bedouin_sedentary_transition, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(bedouin_sedentary_transition, state_centralization).
constraint_victim(bedouin_sedentary_transition, bedouin_traditional_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: TRADITIONAL NOMAD (THESIGER ERA) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Significant agency over personal survival)
   WHEN: biographical (Lifelong nomadic satisfaction)
   WHERE: mobile (Constantly on the move looking for green patches)
   SCOPE: local (Immediate desert environment)
   
   WHY THIS CLASSIFICATION:
   For the traditional Bedouin, the desert was a "Rope"—a functional coordination mechanism 
   where hardship and abstinence were tools to achieve a "freedom unattainable in civilization".
   
   NARRATIVE EVIDENCE:
   "I had found a freedom unattainable in civilization; a life unhampered by possessions... 
   the satisfaction which comes from hardship".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    bedouin_sedentary_transition,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MODERN SETTLED BEDOUIN - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Dependent on state oil money and Bangladeshi herders)
   WHEN: immediate (Day-to-day life in towns)
   WHERE: trapped (Dependent on a $237B state budget requiring $89/barrel oil)
   SCOPE: national (Integrated into Saudi society)
   
   WHY THIS CLASSIFICATION:
   For the modern settled Bedu, the transition is a "Noose." They have "gone soft" and 
   "lost their edge," becoming trapped in settlements where survival is no longer 
   under their control but dependent on finite oil prices.
   
   NARRATIVE EVIDENCE:
   "the price we paid was the price men have always paid for achieving a paradise in this 
   life—we went soft, we lost our edge".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    bedouin_sedentary_transition,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXISTENTIAL ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of civilizational collapse patterns)
   WHEN: civilizational (12,000-year historical record)
   WHERE: analytical (Observer stance)
   SCOPE: global (Systemic risk of interconnected mega-cities)
   
   WHY THIS CLASSIFICATION:
   To the analyst, the systemic risk of urban life is a "Mountain"—an immutable law 
   of complex systems where increased integration leads inevitably to increased 
   fragility and eventual collapse.
   
   NARRATIVE EVIDENCE:
   "As society became more complex, it also became more fragile... These parts are 
   no longer redundant. They are part of an integrated whole".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bedouin_sedentary_transition,
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

:- begin_tests(bedouin_sedentary_transition_tests).

test(multi_perspective_transition) :-
    % Traditional (Moderate) sees Rope
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, rope, context(individual_moderate, _, _, _)),
    % Settled (Powerless) sees Noose
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, noose, context(individual_powerless, _, _, _)),
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, mountain, context(analytical, _, _, _)).

test(extraction_scaling) :-
    % Powerless (settled) experience more extraction of resilience than the analyst observer.
    ContextPowerless = context(individual_powerless, immediate, trapped, national),
    ContextAnalyst = context(analytical, civilizational, analytical, global),
    constraint_indexing:extractiveness_for_agent(bedouin_sedentary_transition, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(bedouin_sedentary_transition, ContextAnalyst, E2),
    E1 > E2.

:- end_tests(bedouin_sedentary_transition_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS MADE BY MODEL:
 * * 1. BASE EXTRACTIVENESS (0.75):
 * Reasoning: The narrative emphasizes that sedentary life is a "paradise" that "extracts" 
 * the ability to survive. The Bedu no longer herd their own animals (hiring others) 
 * and depend on a state budget tied to oil prices.
 * * 2. PERSPECTIVE SELECTION:
 * Model chose Traditional Bedu (Rope/Agency), Settled Bedu (Noose/Dependency), and 
 * Analyst (Mountain/Systemic Law) to capture the full trajectory of the text.
 * * 3. CLASSIFICATION RATIONALE:
 * Settled Bedu -> Noose: They are "trapped" by housing and medical care that will "dry up" 
 * when oil revenue declines.
 * Analyst -> Mountain: The history of Sumeria and the Bronze Age suggests collapse is an 
 * inherent natural law of sedentary complexity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bedouin_resilience_persistence,
    "Can the settled Bedouin re-learn the traditional skills necessary for survival if the oil revenue collapses?",
    resolution_mechanism("Observation of Bedouin response to post-oil economic declines (e.g., Vision 2030 outcomes)"),
    impact("If yes: The transition was a temporary Rope. If no: It is a terminal Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    sedentary_collapse_inevitability,
    "Is systemic collapse a physical necessity (Mountain) for all urban civilizations, or a result of specific choices?",
    resolution_mechanism("Analysis of civilization lifespans vs. resource management techniques"),
    impact("If necessity: Mountain. If choice: Rope/Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Modern Oases (Warka Water)
 * Viability: Incorporating new technologies to form resilient modern oases.
 * Suppression: Currently shunted in favor of markaz settlements dependent on international markets.
 * * ALTERNATIVE 2: Continued Nomadism
 * Viability: The traditional millennium-old resilient strategy.
 * Suppression: Effectively made extinct by the "paradise" of oil money and towns.
 * * CONCLUSION:
 * The existence of resilient alternatives (nomadism) makes the sedentary shift a "Noose" 
 * because the alternative was actively abandoned for comfort.
 */



/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into main system: ?- [constraint_bedouin_sedentary_transition].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
