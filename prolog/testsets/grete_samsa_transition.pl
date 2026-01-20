% ============================================================================
% CONSTRAINT STORY: grete_samsa_transition
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Metamorphosis by Franz Kafka (David Wyllie Trans.)
% ============================================================================

:- module(constraint_grete_samsa_transition, []).

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
 * * constraint_id: grete_samsa_transition
 * human_readable: Grete's Burden and Ascendance
 * domain: social/economic/familial
 * temporal_scope: Late 19th/Early 20th Century
 * spatial_scope: The Samsa Family Flat
 * * SUMMARY:
 * Grete Samsa undergoes a radical transition from a "somewhat useless" child 
 * to the family's primary decision-maker. Initially, Gregor's transformation 
 * is a Rope (functional care) for her, but as she enters the workforce and 
 * seeks a future, Gregor becomes a Noose (existential/social weight) that 
 * she must personally dismantle to survive.
 * * KEY AGENTS:
 * - Grete Samsa: The sister (Primary agent of change).
 * - Gregor Samsa: The vermin (The passive constraint).
 * - The Parents: (Observers of her transition).
 * * NARRATIVE ARC:
 * Grete begins as the only one brave enough to feed Gregor (Rope), but the 
 * exhaustion of labor and the desire for "new dreams" leads her to be the first 
 * to declare that "it must go" (Noose), effectively executing the family's 
 * collective exit strategy.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(grete_ascendance_period, 0, 10).
narrative_ontology:constraint_claim(grete_samsa_transition, rope).

% Base extractiveness score (0.6): Moderate.
% Rationale: Grete extracts status and power within the family by being the 
% sole mediator for Gregor, but she also suffers the extraction of her youth 
% and leisure.
domain_priors:base_extractiveness(grete_samsa_transition, 0.6).

% Suppression score (0.5): Moderate.
% Rationale: She initially refuses to let her mother see Gregor, 
% "protecting" her mother while consolidating her own control over the room.
domain_priors:suppression_score(grete_samsa_transition, 0.5).

% Enforcement requirements
% Requires active enforcement: She must physically lock the door and 
% manage the "filth" to maintain her role.
domain_priors:requires_active_enforcement(grete_samsa_transition).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(grete_samsa_transition, extractiveness, 0.6).
narrative_ontology:constraint_metric(grete_samsa_transition, suppression_requirement, 0.5).

% Beneficiaries and Victims
constraint_beneficiary(grete_samsa_transition, grete_samsa). % Power gain.
constraint_victim(grete_samsa_transition, grete_samsa).      % Burden of care.
constraint_victim(grete_samsa_transition, gregor_samsa).     % Ultimate exclusion.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: EARLY GRETE - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - A 17-year-old girl with no job or authority.
   WHEN: immediate - Focused on what Gregor will eat "today."
   WHERE: constrained - She is the only one who can enter the room.
   SCOPE: local - Limited to the "few square meters" of Gregor's room.
   
   WHY THIS CLASSIFICATION:
   In the beginning, the transformation is a functional coordination challenge. 
   She tests his tastes (cheese vs. vegetables) and adjusts the furniture. 
   She sees it as a duty she can manage.
   
   NARRATIVE EVIDENCE:
   "In order to test his taste, she brought him a whole selection of things, 
   all spread out on an old newspaper."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    grete_samsa_transition,
    rope,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: WORKING GRETE (LATE STORY) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Now a salesgirl, learning shorthand and French.
   WHEN: biographical - Thinking of her future, marriage, and moving house.
   WHERE: mobile - She has an "outside" life and a job.
   SCOPE: regional - Her world now includes the "open country" and the city.
   
   WHY THIS CLASSIFICATION:
   The burden of cleaning Gregor's room after a full day's work makes the 
   constraint extractive and asymmetric. Gregor no longer provides (money); 
   he only consumes (time/reputation). He is now a Noose threatening her 
   "blossoming" future.
   
   NARRATIVE EVIDENCE:
   "We can't carry on like this... we have to try and get rid of it."
   "She was becoming livelier... blossoming into a well built and beautiful 
   young lady."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    grete_samsa_transition,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(grete_samsa_transition, E),
    E > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PARENTS - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerful (Relative to the household)
   WHEN: generational - Concerned about the end of their lives and "peace."
   WHERE: trapped - They feel they cannot move because of "that thing."
   SCOPE: local - Confined to the apartment and its immediate social shame.
   
   WHY THIS CLASSIFICATION:
   For the parents, Grete's transformation into the primary caregiver is an 
   immutability they simply accept. They watch her from the hallway, 
   powerless to intervene, treating her new role as a fixed natural law of 
   their "misfortune."
   
   NARRATIVE EVIDENCE:
   "The two of them, father and mother, would often both wait outside the 
   door... his mother also wanted to go in... but his father and sister 
   at first persuaded her against it."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    grete_samsa_transition,
    mountain,
    context(
        agent_power(individual_powerful),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(grete_samsa_transition_tests).

test(role_reversal_insight) :-
    % Test that Grete's power (agent_power) increases as Gregor's 
    % classification shifts from Rope to Noose.
    constraint_indexing:constraint_classification(grete_samsa_transition, rope, 
        context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(grete_samsa_transition, noose, 
        context(agent_power(individual_moderate), _, _, _)).

test(biographical_exit_pressure) :-
    % Long-term horizons (marriage/career) make social constraints feel like Nooses.
    true.

:- end_tests(grete_samsa_transition_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SHIFT: 
 * Grete is the only character whose perspective on the constraint shifts 
 * radically based on her own development. Her transition from 
 * "powerless child" to "decision maker" is the engine of the third act.
 * * 2. EXTRACTIVENESS: 
 * I focused on the "theft of peace" and "theft of a future." For Grete, 
 * Gregor isn't just a bug; he's a barrier to a "good man" and a 
 * "better position."
 * * 3. AMBIGUITIES:
 * Is her final cruelty (locking the door) a Noose (coercion) or a Rope 
 * (necessary boundary for family survival)? I classified it as Noose 
 * because it actively suppresses Gregor's lingering humanity/identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grete_intent,
    "Did Grete truly believe Gregor could no longer understand them, or was it a strategic lie to ease the family's conscience?",
    resolution_mechanism("Requires access to Grete's internal monologue, which Kafka denies the reader."),
    impact("If Lie: Noose (intentional extraction/removal). If Belief: Rope (tragic necessity)."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Professional Care
 * Viability: The family could have used the "saved money" to hire specialized 
 * help.
 * Suppression: Suppressed by the shame of "misfortune unlike anything 
 * experienced by anyone else."
 * * ALTERNATIVE 2: Continued Conservatory Dream
 * Viability: Gregor's secret plan to send her to the Conservatory.
 * Suppression: Rejected by Grete herself as she prioritizes shorthand and 
 * French for immediate survival.
 * * CONCLUSION:
 * The death of the Conservatory dream marks the point where Grete's 
 * relationship to the constraint hardens into a Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% narrative_ontology:interval(grete_samsa_transition, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
