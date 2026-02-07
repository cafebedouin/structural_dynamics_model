% ============================================================================
% CONSTRAINT STORY: individual_revolution_autonomy
% ============================================================================
% Generated: 2025-05-20
% Model: Gemini 2.0 Flash
% Source: "Revolution For One" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_individual_revolution_autonomy, []).

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
 * * constraint_id: individual_revolution_autonomy
 * human_readable: The One-Man Revolution
 * domain: political/social
 * temporal_scope: Contemporary / Historical
 * spatial_scope: Global / Individual
 * * SUMMARY:
 * This constraint contrasts the "futile" pursuit of mass socio-political revolution with the "fertile" 
 * pursuit of individual autonomy and alternative culture. It posits that large-scale 
 * institutional change is a slow, generational process often co-opted by elites to maintain the 
 * status quo.
 * * KEY AGENTS:
 * - The Elite/Institutional Actor: Uses top-down control to consolidate power and maintain the status quo.
 * - The Disengaged Citizen: Experiences the "illusion as reality," often feeling a limited ability to change society.
 * - The Conscious Individual (Amish/One-Man Rebel): Focuses on the "only revolution that is coming"—internal change and alternative values.
 * * NARRATIVE ARC:
 * The individual is initially "trapped" by dominant narratives and the perceived necessity of mass 
 * movements. By recognizing the limits of top-down control and the "futility" of general 
 * revolution, the agent pivots to a "One-Man Revolution," redefining identity and interaction 
 * outside established power structures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Structural Anchor for extraction
narrative_ontology:interval(individual_revolution_autonomy, 0, 10).
narrative_ontology:constraint_claim(individual_revolution_autonomy, snare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISENGAGED CITIZEN - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Feels limited influence and disengagement from institutions.
   WHEN: immediate - Focused on current electoral cycles and immediate issues.
   WHERE: trapped - Bound by dominant narratives and the perceived "binary" of marriage/loneliness or status quo.
   SCOPE: national - Concerned with federal elections and national policy.
   
   WHY THIS CLASSIFICATION:
   For the disengaged individual, the system is a Snare because they perceive their ability to change 
   society as "limited" and are often subject to top-down control efforts like the "War on Drugs".
   
   NARRATIVE EVIDENCE:
   "Most people understand that their ability to change society is limited... This likely accounts 
   for much of the disengagement".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    individual_revolution_autonomy,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(individual_revolution_autonomy, E),
    E > 0.7.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ELITE CONTROLLER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Exerts top-down control to consolidate and maintain power.
   WHEN: historical - Operates on timescales of centuries and deep institutional inertia.
   WHERE: mobile - Capable of creating enough change to maintain power.
   SCOPE: global - Managing conditions in a global economy or automation-displaced humanity.
   
   WHY THIS CLASSIFICATION:
   For the elite, the system is a Rope; it is a "functional coordination mechanism" for 
   maintaining the status quo and their own influence.
   
   NARRATIVE EVIDENCE:
   "top down control always inspires significant resistance... they are primarily concerned about 
   maintaining the status quo, creating enough change to maintain their power".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    individual_revolution_autonomy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ONE-MAN REVOLUTIONARY (AMISH-LIKE) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Refuses participation in mass culture to focus on immediate sphere.
   WHEN: biographical - Focuses on individual life transformation and community values.
   WHERE: mobile - Refuses to use dominant narratives as the reference point.
   SCOPE: local - Immediate social sphere and alternative culture.
   
   WHY THIS CLASSIFICATION:
   For the autonomous individual, the "One-man revolution" is a Rope; it is a fertile form of 
   action that allows them to "redefine identity" and create alternative values.
   
   NARRATIVE EVIDENCE:
   "The revolution of single individuals and of small communities to reject mass culture... is the 
   only revolution that is always possible".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    individual_revolution_autonomy,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE HISTORIAN - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observing long-term social patterns)
   WHEN: historical (Viewing cycles of revolution and control)
   WHERE: analytical (Detached from the immediate political struggle)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the historian, the tension between elite control and individual autonomy
   is a "Mountain." It is a fundamental, recurring pattern in human societies,
   a dynamic that plays out over centuries with predictable outcomes. The
   "futility" of mass revolution is a data point in this larger, unchangeable landscape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    individual_revolution_autonomy,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(individual_revolution_autonomy_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that the concept of revolution is viewed differently across agents.
 */
test(multi_perspective_variance) :-
    % Disengaged Citizen (Snare)
    constraint_indexing:constraint_classification(
        individual_revolution_autonomy,
        Type1,
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(national))
    ),
    % Elite Controller (Rope)
    constraint_indexing:constraint_classification(
        individual_revolution_autonomy,
        Type2,
        context(agent_power(institutional), time_horizon(historical), exit_options(mobile), spatial_scope(global))
    ),
    % One-Man Revolutionary (Rope)
    constraint_indexing:constraint_classification(
        individual_revolution_autonomy,
        Type3,
        context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
    ),
    % Historian (Mountain)
    constraint_indexing:constraint_classification(
        individual_revolution_autonomy,
        Type4,
        context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type4. % Rope and Mountain are different

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that powerless citizens experience higher extraction than the elites who control the system.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(national)),
    ContextPowerful = context(agent_power(institutional), time_horizon(historical), exit_options(mobile), spatial_scope(global)),
    constraint_indexing:extractiveness_for_agent(individual_revolution_autonomy, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(individual_revolution_autonomy, ContextPowerful, Score2),
    Score1 > Score2.  % Powerless experience more extraction

/**
 * TEST 3: Domain-specific insight - The "One-Man Revolution"
 * Demonstrates that individual autonomy provides a 'rope' to escape the 'snare' of mass politics.
 */
test(one_man_revolution_insight) :-
    constraint_indexing:constraint_classification(individual_revolution_autonomy, ClassificationCitizen, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_revolution_autonomy, ClassificationRebel, context(agent_power(individual_moderate), _, _, _)),
    ClassificationCitizen = snare,
    ClassificationRebel = rope.

:- end_tests(individual_revolution_autonomy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. EXTRACTIVENESS SCORE (0.75): The text explicitly states that top-down projects are "elite projects" 
 * that "benefit only a small subset of society". 
 * 
 * 2. SUPPRESSION SCORE (0.8): The source highlights that "dominant ideas" are internalized even by those 
 * in opposition, making alternatives invisible or illusory.
 * 
 * 3. PERSPECTIVE SELECTION:
 *    Chose the powerless citizen (Snare), the elite controller (Rope), the autonomous individual (Rope), 
 *    and the historian (Mountain) to show the full spectrum of engagement with social change.
 * 
 * 4. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        individual_revolution_autonomy_extraction_intent,
 *        "Is the high extractiveness of top-down control a functional necessity for social stability (Mountain) or purely predatory (Snare)?",
 *        resolution_mechanism("Audit of elite resource allocation vs. social survival thresholds during crises"),
 *        impact("If necessity: Mountain. If predatory: Snare/Mandatrophy."),
 *        confidence_without_resolution(medium)
 *    ).
 * 
 *    omega_variable(
 *        revolutionary_conditions_ripeness,
 *        "Is it impossible to know in advance if conditions are ripe for revolution (Mountain), or can a 'One-Man' shift catalyze mass divergence?",
 *        resolution_mechanism("Trace whether individual identity shifts (Amish-style) reach critical mass to trigger systemic unraveling"),
 *        impact("If Mountain: Mass revolution remains a futile illusion. If catalyzeable: One-man shifts are Scaffolds."),
 *        confidence_without_resolution(low)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Electoral Politics / Reform
 *    Viability: Facilitates the perpetuation of a "slightly more enlightened status quo".
 *    Suppression: Often characterized as an "illusion" that fails to address the root problem.
 *    Evidence: Participation levels are low (~60%), and change happens over "decades" if at all.
 * 
 * CONCLUSION:
 * The existence of electoral politics as a perceived but "futile" alternative reinforces 
 * the "One-Man Revolution" as the only functional Rope for individual autonomy.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/individual_revolution_autonomy].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(individual_revolution_autonomy).
 * 
 * 3. Run tests:
 *    ?- run_tests(individual_revolution_autonomy_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(individual_revolution_autonomy).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(individual_revolution_autonomy, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
