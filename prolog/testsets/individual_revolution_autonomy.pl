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

% Base extractiveness: High (0.75)
% Rationale: Top-down control projects are elite projects primarily concerned with maintaining 
% the status quo and consolidating power for a small subset of society.
domain_priors:base_extractiveness(individual_revolution_autonomy, 0.75).

% Suppression: High (0.8)
% Rationale: Dominant ideas of the dominant class are pervasive, shaping the illusions individuals 
% have about themselves even when they take an antagonistic form.
domain_priors:suppression_score(individual_revolution_autonomy, 0.8).

% Enforcement: Emerges naturally through the inertia of social networks and informal structures.
domain_priors:emerges_naturally(individual_revolution_autonomy).

% BENEFICIARIES & VICTIMS
% Beneficiary: The political elite and those consolidated in power.
constraint_beneficiary(individual_revolution_autonomy, political_elite).
% Victim: The everyday individual and those seeking genuine systemic change through mass movements.
constraint_victim(individual_revolution_autonomy, everyday_individual).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(individual_revolution_autonomy, extractiveness, 0.75).
narrative_ontology:constraint_metric(individual_revolution_autonomy, suppression_requirement, 0.8).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISENGAGED CITIZEN - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Feels limited influence and disengagement from institutions.
   WHEN: immediate - Focused on current electoral cycles and immediate issues.
   WHERE: trapped - Bound by dominant narratives and the perceived "binary" of marriage/loneliness or status quo.
   SCOPE: national - Concerned with federal elections and national policy.
   
   WHY THIS CLASSIFICATION:
   For the disengaged individual, the system is a Noose because they perceive their ability to change 
   society as "limited" and are often subject to top-down control efforts like the "War on Drugs".
   
   NARRATIVE EVIDENCE:
   "Most people understand that their ability to change society is limited... This likely accounts 
   for much of the disengagement".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    individual_revolution_autonomy,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(individual_revolution_autonomy, E),
    E > 0.7,
    !.

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
) :-
    !.

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
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(individual_revolution_autonomy_tests).

test(multi_perspective_variance) :-
    % Test that different perspectives yield different classifications
    constraint_indexing:constraint_classification(
        individual_revolution_autonomy,
        Type1,
        context(individual_powerless, immediate, trapped, national)
    ),
    constraint_indexing:constraint_classification(
        individual_revolution_autonomy,
        Type2,
        context(individual_moderate, biographical, mobile, local)
    ),
    % In this narrative, the powerless sees a Noose, the moderate sees a Rope
    Type1 = noose,
    Type2 = rope.

test(power_extractiveness_scaling) :-
    % Powerless (trapped in mass culture) experience more extraction than elites
    domain_priors:base_extractiveness(individual_revolution_autonomy, Score),
    Score > 0.5.

test(time_immutability) :-
    % Mass social movements (historical) appear unchangeable to the immediate actor
    constraint_indexing:effective_immutability(historical, analytical, mountain).

:- end_tests(individual_revolution_autonomy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2024-05-20
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: The text explicitly states that top-down projects are "elite projects" 
 * that "benefit only a small subset of society". 
 * Evidence: "Almost always, the goal of top-down projects is to consolidate... power".
 * * 2. SUPPRESSION SCORE (0.8):
 * Reasoning: The source highlights that "dominant ideas" are internalized even by those 
 * in opposition, making alternatives invisible or illusory.
 * * 3. PERSPECTIVE SELECTION:
 * Chose the powerless citizen (Noose) to show the trap of mass politics, and the 
 * autonomous individual (Rope) to show the fertile alternative path.
 * * 4. AMBIGUITIES:
 * - The text mentions "The successful conditions of a general revolution... cannot be 
 * manufactured". This creates an Omega regarding the "ripeness" of history.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    individual_revolution_autonomy_extraction_intent,
    "Is the high extractiveness of top-down control a functional necessity for social stability (Mountain) or purely predatory (Noose)?",
    resolution_mechanism("Audit of elite resource allocation vs. social survival thresholds during crises"),
    impact("If necessity: Mountain. If predatory: Noose/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    revolutionary_conditions_ripeness,
    "Is it impossible to know in advance if conditions are ripe for revolution (Mountain), or can a 'One-Man' shift catalyze mass divergence?",
    resolution_mechanism("Trace whether individual identity shifts (Amish-style) reach critical mass to trigger systemic unraveling"),
    impact("If Mountain: Mass revolution remains a futile illusion. If catalyzeable: One-man shifts are Scaffolds."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Electoral Politics / Reform
 * Viability: Facilitates the perpetuation of a "slightly more enlightened status quo".
 * Suppression: Often characterized as an "illusion" that fails to address the root problem.
 * Evidence: Participation levels are low (~60%), and change happens over "decades" if at all.
 * * CONCLUSION:
 * The existence of electoral politics as a perceived but "futile" alternative reinforces 
 * the "One-Man Revolution" as the only functional Rope for individual autonomy.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/individual_revolution_autonomy].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(individual_revolution_autonomy).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
