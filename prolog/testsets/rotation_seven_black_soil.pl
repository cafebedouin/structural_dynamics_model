% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: black_soil_toxicity
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 2.0 Flash
% Source: "Rotation Seven" narrative
% ============================================================================

:- module(black_soil_toxicity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor for indexing
narrative_ontology:interval(black_soil_toxicity, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: black_soil_toxicity
 * human_readable: Toxic Biological Effects of R7 Black Soil
 * domain: biological_environmental
 * temporal_scope: Biographical (years of accumulation)
 * spatial_scope: Rotation Seven Station Greenhouse
 * * SUMMARY:
 * The black soil in Section 7-B contains toxins that lead to kidney failure 
 * and death. While the station's protocols (Nooses) manage how children 
 * interact with the soil, the soil's actual biological effect on the human 
 * body is an unchangeable physical reality.
 * * KEY AGENTS:
 * - Anna (Powerless): Exposed to the soil; her body is the site of the constraint.
 * - Supervisor Kwan (Institutional): Cannot "manage" the toxin away; can only 
 * manage the victims.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */

% Base extractiveness: 0.95 (Extreme).
% It extracts the vital organs (kidneys) and eventually the entire life of 
% the agent.
domain_priors:base_extractiveness(black_soil_toxicity, 0.95).

% Suppression: 0.1 (Low).
% It is not a rule being "hidden"; it is a physical fact that manifests in 
% dark urine and yellow skin.
domain_priors:suppression_score(black_soil_toxicity, 0.1).

% Active Enforcement: NO.
% No one "enforces" the kidney failure. It is a biological consequence that 
% happens automatically upon exposure.
domain_priors:requires_active_enforcement(black_soil_toxicity) :- fail.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE: ALL AGENTS - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless / institutional
   WHEN: immediate / biographical
   EXIT: trapped
   
   WHY: Toxicity is a Mountain because there are zero degrees of freedom. 
   Anna cannot "negotiate" with her enzyme levels, and Kwan cannot 
   reprogram the toxin's chemical signature. It is the 
   unchangeable ground upon which the station's tragedies are built.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    black_soil_toxicity,
    mountain,
    context(
        agent_power(_),
        time_horizon(_),
        exit_options(trapped),
        spatial_scope(_)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: HIGH COMMAND (STRATEGIC) - Rope
   --------------------------------------------------------------------------
   WHO: agent_power(institutional) - High Command managing population flow.
   WHEN: biographical - Planning the multi-year abandonment of Sector 7.
   WHERE: arbitrage - Managing resource balance between Sectors 7 and 1.
   SCOPE: continental - The survival of the entire colony architecture.
   
   WHY THIS CLASSIFICATION:
   High Command uses the "Black Soil" crisis as a Rope. It is a coordination 
   tool that justifies the "Mandatrophic" necessity of urban migration. By 
   framing the depletion as absolute, they pull the populace into a more 
   legible, efficient, and taxable urban core in Sector 1.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_black_soil,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(continental)
    )
) :-
    domain_priors:requires_active_enforcement(rotation_seven_black_soil),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: SECTOR 7 AGRARIANS (RESIDENTS) - Noose
   --------------------------------------------------------------------------
   WHO: agent_power(individual_powerless) - Those whose margin has collapsed.
   WHEN: immediate - The current harvest failure and impending hunger.
   WHERE: trapped - Bound by the soil they can no longer farm.
   SCOPE: local - Their home, community, and immediate survival.
   
   WHY THIS CLASSIFICATION:
   For the residents, the black soil is a Noose. The depletion is not a 
   "strategic opportunity" but an extractive trap that has consumed their 
   autonomy. With a suppression of alternative food sources (0.8) and 
   existential stakes (0.9), they have no exit but the state's migration 
   mandate.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_black_soil,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    !.

% Explicit priors reflecting the high extraction of the "Black Soil" crisis.
domain_priors:base_extractiveness(rotation_seven_black_soil, 0.8).
domain_priors:suppression_score(rotation_seven_black_soil, 0.8).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(black_soil_tests).

test(universal_mountain_status) :-
    % Unlike the Kubo Noose, this is a Mountain for the powerful (Kwan) 
    % and the powerless (Anna).
    constraint_indexing:constraint_classification(black_soil_toxicity, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(black_soil_toxicity, mountain, context(agent_power(individual_powerless), _, _, _)).

test(zero_enforcement_constraint) :-
    % A true Mountain does not require a guard to make it work.
    \+ domain_priors:requires_active_enforcement(black_soil_toxicity).

:- end_tests(black_soil_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================= */

/**
 * LLM GENERATION NOTES:
 * 1. CLASSIFICATION AS MOUNTAIN:
 * This is a "True Mountain." While the *rules* about who touches the soil 
 * are Nooses, the *toxin itself* is the physical ceiling of the 
 * environment. It lacks the "Type 1 False Mountain" signature because 
 * it requires zero active enforcement to kill.
 * 2. EXTRACTIVENESS:
 * Set to 0.95 because it extracts the physical substrate of agency 
 * (the biological body).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * THE ONLY ALTERNATIVE:
 * The only "exit" from this Mountain is physical: leaving the station 
 * or finding a cure. Since "Rotation Seven" implies no one ever returns 
 * and dialysis "can't be done here," the exit option is 'trapped'.
 */

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(rotation_seven_black_soil, mountain, agent_power(analytical)).
