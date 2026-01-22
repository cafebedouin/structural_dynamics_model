% ============================================================================
% CONSTRAINT STORY: cow_field_poop
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Out in the Cow Field of Life" by cafebedouin.org
% ============================================================================

:- module(constraint_cow_field_poop, []).

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
 * * constraint_id: cow_field_poop
 * human_readable: The Cow Field Hazard (Pragmatic Avoidance)
 * domain: social/psychological
 * temporal_scope: Perennial
 * spatial_scope: Local (The immediate path) to Global (The field of life)
 * * SUMMARY:
 * This constraint defines the presence of "poop" (negative events, social hazards, or 
 * nonsense) as an inherent and diverse feature of existence. It 
 * distinguishes between the analytical act of cataloging these hazards and the 
 * pragmatic necessity of avoiding them.
 * * KEY AGENTS:
 * - The Practical Walker (Agent): The individual attempting to navigate the field 
 * without "stepping in one".
 * - The Cataloger (Analytical): An observer who describes every detail of the 
 * "poop" but may miss the functional point of navigation.
 * - The Victim (The Stepper): One who has already encountered the hazard, 
 * suffering immediate extraction of dignity or cleanliness.
 * * NARRATIVE ARC:
 * Life is presented as a field saturated with diverse hazards (fresh, wet, old, 
 * round). The "main thing" is not the intellectual mastery of the 
 * hazard's taxonomy, but the operational coordination of one's own feet to 
 * ensure a clean passage.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(cow_field_poop, 0, 10).

% Updated to valid v3.1 ontology pillar: [mountain]
narrative_ontology:constraint_claim(cow_field_poop, mountain).

% Base extractiveness: Moderate (0.4)
% Rationale: Encountering a hazard extracts time, emotional energy, and resources (cleaning). 
% Cataloging without avoiding also extracts cognitive bandwidth.
domain_priors:base_extractiveness(cow_field_poop, 0.4).

% Suppression: Moderate (0.3)
% Rationale: The "extraordinary" detail of cataloging can suppress the "ordinary" 
% necessity of simple navigation.
domain_priors:suppression_score(cow_field_poop, 0.3).

% Enforcement: Emerges naturally from the nature of the "field".
domain_priors:emerges_naturally(cow_field_poop).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cow_field_poop, effective_navigators). % Those who know how not to step.
constraint_victim(cow_field_poop, the_unwary_walker).         % Those who step in it.

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(cow_field_poop, extractiveness, 0.4).
narrative_ontology:constraint_metric(cow_field_poop, suppression_requirement, 0.3).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRACTICAL WALKER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has agency to choose where to step)
   WHEN: immediate (The next step in the field)
   WHERE: mobile (Can see hazards and adjust path)
   SCOPE: local (The immediate environment)
   
   WHY THIS CLASSIFICATION:
   For the navigator, the hazards are a "Rope"—a functional coordination mechanism. 
   Knowledge of the "poop" is used as a tool to successfully coordinate movement 
   through the field.
   
   NARRATIVE EVIDENCE:
   "The main thing is knowing, how not to step in one".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    cow_field_poop,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE VICTIM (THE STEPPER) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Slaves to the "poop" they just stepped in)
   WHEN: immediate (The moment of impact)
   WHERE: trapped (Bounded by the mess and its consequences)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the person who has stepped in it, the hazard is a "Noose." It has 
   tightened around their foot, extracting their progress and comfort, and 
   requiring immediate "liquidation" of the mess to proceed.
   
   NARRATIVE EVIDENCE:
   "how not to step in one" [implies the negative state of having stepped in one].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cow_field_poop,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- 
    domain_priors:base_extractiveness(cow_field_poop, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CATALOGER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Ancient and wise observer of "every detail")
   WHEN: historical (Viewing the field as a permanent collection of objects)
   WHERE: analytical (Observer stance; "catalog them all")
   SCOPE: global (The "cow field of life")
   
   WHY THIS CLASSIFICATION:
   To the analytical observer, the hazards are a "Mountain"—an unchangeable 
   natural law of the terrain. The "poop" is a fixed, albeit diverse, 
   feature of the environment that exists to be described and understood.
   
   NARRATIVE EVIDENCE:
   "Even if you catalog them all, and describe every detail".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cow_field_poop,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cow_field_poop_tests).

test(multi_perspective_navigation) :-
    % Walker sees Rope
    constraint_indexing:constraint_classification(cow_field_poop, Type1, context(individual_moderate, immediate, mobile, local)),
    % Stepper sees Noose
    constraint_indexing:constraint_classification(cow_field_poop, Type2, context(individual_powerless, immediate, trapped, local)),
    % Cataloger sees Mountain
    constraint_indexing:constraint_classification(cow_field_poop, Type3, context(analytical, historical, analytical, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_navigation) :-
    % The victim (powerless) experiences higher extraction of cleanliness/time
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, immediate, mobile, local),
    constraint_indexing:extractiveness_for_agent(cow_field_poop, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(cow_field_poop, ContextModerate, E2),
    E1 > E2.

:- end_tests(cow_field_poop_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: Stepping in "poop" is an extraction of time and state. However, the 
 * text focuses on the *knowledge* of avoidance, which mitigates the extraction 
 * for those who learn.
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the Cataloger (Mountain) with the Practical Walker (Rope) to 
 * reflect the text's primary pedagogical point: knowing vs. doing.
 * * 3. CLASSIFICATION RATIONALE:
 * Practical Walker -> Rope: Coordination tool for safe passage.
 * Cataloger -> Mountain: Fixed feature of life's "field."
 * * 4. AMBIGUITIES:
 * The nature of the "poop" is metaphorical. I have mapped it as "environmental 
 * hazards" or "social noise."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    poop_catalog_utility,
    "Does 'describing every detail' of life's hazards eventually provide a Rope 
     for better avoidance, or is it a terminal Mountain of academic clutter?",
    resolution_mechanism("Audit of navigation success rates in catalogers vs. non-cataloging practitioners"),
    impact("If beneficial: Cataloging is a Rope. If clutter: It is a Noose for time-extraction."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Cleaning the Field (Sanitization)
 * Viability: Removing the hazards entirely.
 * Suppression: Shunted as impossible; the poem assumes the field *contains* * the poop and the only agency is avoidance.
 * * ALTERNATIVE 2: Pure Ignorance
 * Viability: Walking without looking.
 * Suppression: Leads directly to the "Noose" of stepping in one.
 * * CONCLUSION:
 * The absence of Alternative 1 (Sanitization) makes "avoidance" (Rope) the 
 * only logical path in a field that is structurally a "Mountain."
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [cow_field_poop].
% Multi-perspective: ?- constraint_indexing:multi_index_report(cow_field_poop).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
