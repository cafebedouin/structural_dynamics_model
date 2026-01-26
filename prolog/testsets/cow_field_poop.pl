% ============================================================================
% CONSTRAINT STORY: cow_field_poop
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
 * 
 * constraint_id: cow_field_poop
 * human_readable: The Cow Field Hazard (Pragmatic Avoidance)
 * domain: social/psychological/environmental
 * temporal_scope: Perennial
 * spatial_scope: Local (The immediate path) to Global (The field of life)
 * 
 * SUMMARY:
 * This constraint defines the presence of "poop" (negative events, social hazards, or 
 * nonsense) as an inherent and diverse feature of existence. It 
 * distinguishes between the analytical act of cataloging these hazards and the 
 * pragmatic necessity of avoiding them.
 * 
 * KEY AGENTS:
 * - The Practical Walker (Individual Moderate): The individual attempting to navigate the field.
 * - Public Works Department / Urban Planner (Institutional): Manages environmental hazards through infrastructure.
 * - The Victim (Individual Powerless): One who has already encountered the hazard.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(cow_field_poop, 0, 10).
narrative_ontology:constraint_claim(cow_field_poop, mountain).

% Base extractiveness: 0.4.
% Encountering a hazard extracts time, emotional energy, and resources (cleaning). 
% Cataloging without avoiding also extracts cognitive bandwidth.
domain_priors:base_extractiveness(cow_field_poop, 0.4).

% Suppression: 0.3.
% The "extraordinary" detail of cataloging can suppress the "ordinary" 
% necessity of simple navigation.
domain_priors:suppression_score(cow_field_poop, 0.3).

% Enforcement: Emerges naturally from the nature of the "field".
domain_priors:emerges_naturally(cow_field_poop).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cow_field_poop, effective_navigators).
constraint_victim(cow_field_poop, the_unwary_walker).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRACTICAL WALKER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has agency to choose where to step)
   WHEN: immediate (The next step in the field)
   WHERE: mobile (Can see hazards and adjust path)
   
   WHY THIS CLASSIFICATION:
   For the navigator, the hazards are a 'Rope'—a functional coordination mechanism. 
   Knowledge of the "poop" is used as a tool to successfully coordinate movement 
   through the field, ensuring a clean passage.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: PUBLIC WORKS DEPARTMENT / URBAN PLANNER - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (Manages environmental hazards through infrastructure)
   WHEN: historical (Long-term planning for public health and safety)
   WHERE: analytical (Develops systems to mitigate inherent hazards)
   
   WHY THIS CLCLASSIFICATION:
   For a Public Works Department or Urban Planner, "cow field poop" represents
   a 'Mountain'—an inherent environmental hazard or urban planning challenge
   that must be managed through infrastructure and public policy, not simply
   avoided by individuals. It's a fixed reality that requires systemic solutions.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cow_field_poop,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE VICTIM (THE STEPPER) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Suffers immediate extraction of dignity or cleanliness)
   WHEN: immediate (The moment of impact)
   WHERE: trapped (Bounded by the mess and its consequences)
   
   WHY THIS CLASSIFICATION:
   For the person who has stepped in it, the hazard is a 'Snare'. It has 
   tightened around their foot, extracting their progress and comfort, and 
   requiring immediate "liquidation" of the mess to proceed. This effectively
   strangles their dignity and mobility.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cow_field_poop,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cow_field_poop_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(cow_field_poop, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(cow_field_poop, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cow_field_poop, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(cow_field_poop_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'Public Works Department / Urban Planner'
 *    as the institutional agent. For them, environmental hazards are a 'Mountain'
 *    requiring systemic management.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Practical Walker (Rope): A tool for navigation and avoidance.
 *    - Public Works (Mountain): Inherent environmental hazard requiring systemic solutions.
 *    - Victim (Snare): Trapped by the immediate consequences of the hazard.
 * 
 * 3. CORE INSIGHT: Life's "cow field" presents a fundamental 'Mountain' of
 *    unavoidable hazards. While individuals can use a 'Rope' of pragmatic
 *    avoidance, and institutions employ systemic management strategies, the
 *    failure to navigate these hazards can quickly turn into a dignifying 'Snare'
 *    for the unwary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the utility of detailed hazard cataloging versus pragmatic avoidance.
 */

omega_variable(
    poop_catalog_utility,
    "Does 'describing every detail' of life's hazards eventually provide a 'Rope' for better avoidance and systemic mitigation, or is it a terminal 'Mountain' of academic clutter, distracting from pragmatic action?",
    resolution_mechanism("Audit of navigation success rates in catalogers vs. non-cataloging practitioners; effectiveness of public awareness campaigns versus infrastructure investments in hazard reduction."),
    impact("If beneficial: Cataloging is a 'Rope'. If clutter: It is a 'Snare' for time-extraction and a distraction from real solutions."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Cleaning the Field (Systemic Sanitization)
 *    Viability: Removing the hazards entirely through large-scale environmental engineering or social reform.
 *    Suppression: Often shunted as impossible or prohibitively expensive; the parable assumes the field *contains* the poop, and the only individual agency is avoidance.
 *
 * CONCLUSION:
 * The "cow field" highlights the 'Mountain' of inherent hazards in existence.
 * While individuals develop 'Ropes' of pragmatic avoidance, and institutions
 * seek systemic 'Ropes' (infrastructure), the ultimate solution of 'Cleaning
 * the Field' is often suppressed by its perceived impossibility, leaving
 * many to face the 'Snare' of direct hazard.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/cow_field_poop].
 * 2. Multi-perspective: ?- multi_index_report(cow_field_poop).
 * 3. Run tests: ?- run_tests(cow_field_poop_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */