% ============================================================================
% CONSTRAINT STORY: asce_7_22_seismic_design
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: ASCE/SEI 7-22: Minimum Design Loads and Associated Criteria
% ============================================================================

:- module(constraint_asce_7_22_seismic, []).

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
 * * constraint_id: asce_7_22_seismic_design
 * human_readable: ASCE 7-22 Seismic Design Requirements
 * domain: technological/legal
 * temporal_scope: 2022-Present
 * spatial_scope: National (United States)
 * * SUMMARY:
 * ASCE 7-22 dictates the minimum loads, including seismic, that buildings 
 * must be designed to withstand. It introduces updated multi-period 
 * design response spectra to more accurately capture the behavior of structures 
 * on various soil types.
 * * KEY AGENTS:
 * - Junior Structural Engineer: Powerless subject who must follow the code 
 * to obtain a permit.
 * - Building Official (AHJ): Institutional enforcer of the code requirements.
 * - Code Committee (ASCE 7): Powerful institutional body that defines 
 * the "laws of engineering".
 * * NARRATIVE ARC:
 * To the engineer, the code is a Mountain: an unchangeable set of physical 
 * and legal requirements. To the public, it is a Rope: a coordination 
 * mechanism providing a baseline of safety. If the code becomes 
 * overly conservative due to lobbying, it risks becoming a Noose for 
 * affordable development.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(asce_cycle_22, 0, 10).
narrative_ontology:constraint_claim(asce_7_22_seismic_design, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.2. While compliance costs money, the "benefit" is public safety. 
% Minimal extraction of capital compared to safety provided.
domain_priors:base_extractiveness(asce_7_22_seismic_design, 0.2).

% Suppression score (0.0-1.0)
% Rationale: 0.7. Alternatives (Performance-Based Design) exist but are 
% highly scrutinized and costly to implement compared to prescriptive rules.
domain_priors:suppression_score(asce_7_22_seismic_design, 0.7).

% Enforcement requirements
domain_priors:requires_active_enforcement(asce_7_22_seismic_design).

% Metrics required for Executive Summary
narrative_ontology:constraint_metric(asce_7_22_seismic_design, extractiveness, 0.2).
narrative_ontology:constraint_metric(asce_7_22_seismic_design, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(asce_7_22_seismic_design, public_safety).
constraint_beneficiary(asce_7_22_seismic_design, insurance_industry).
constraint_victim(asce_7_22_seismic_design, property_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: JUNIOR STRUCTURAL ENGINEER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Lacks authority to change the design spectra.
   WHEN: immediate - Focused on meeting the deadline for the current project.
   WHERE: trapped - Bound by state-adopted building codes and professional liability.
   SCOPE: local - Project-specific site and building.
   
   WHY THIS CLASSIFICATION:
   The engineer cannot negotiate with the ground motion parameters. The values 
   provided by the ASCE 7 Hazard Tool are as fixed as the gravity of Earth 
   itself for the duration of the design process.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    asce_7_22_seismic_design,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(asce_7_22_seismic_design),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: ASCE 7 COMMITTEE - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power to adjust factors and methods.
   WHEN: generational - Thinking in 6-year code cycles and 50-year building lives.
   WHERE: mobile - Can adjust the code based on new seismological data.
   SCOPE: national - Sets the standard for the entire United States.
   
   WHY THIS CLASSIFICATION:
   For the committee, the code is a functional coordination mechanism. It 
   aligns the scientific understanding of hazards with the practical 
   needs of construction to maintain a reliable "rope" of safety.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    asce_7_22_seismic_design,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(asce_7_22_seismic_design, E),
    E < 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: MARGINAL HOUSING DEVELOPER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has some agency but limited capital.
   WHEN: biographical - Business cycle of 5-10 years.
   WHERE: constrained - High cost of land and materials.
   SCOPE: regional - Local real estate market.
   
   WHY THIS CLASSIFICATION:
   Increased seismic requirements in ASCE 7-22, particularly for soft soils, 
   increase structural costs. For a developer on the margin, these 
   unyielding requirements can "choke" the viability of a project, preventing 
   new housing from being built.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    asce_7_22_seismic_design,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :-
    domain_priors:suppression_score(asce_7_22_seismic_design, S),
    S > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(asce_7_22_seismic_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, Type1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, Type2, context(institutional, generational, mobile, national)),
    constraint_indexing:constraint_classification(asce_7_22_seismic_design, Type3, context(individual_moderate, biographical, constrained, regional)),
    Type1 \= Type2,
    Type2 \= Type3.

test(safety_scaling) :-
    % Test that "extractiveness" is low because the goal is safety, not profit
    domain_priors:base_extractiveness(asce_7_22_seismic_design, E),
    E < 0.3.

:- end_tests(asce_7_22_seismic_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.2): Standard engineering codes are rarely 
 * extractive in the sense of transferring wealth unfairly; they are 
 * high-friction safety mechanisms.
 * * 2. PERSPECTIVE SELECTION: Chose the Developer as a "Noose" perspective 
 * to highlight that safety constraints have economic externalities.
 */

omega_variable(
    seismic_model_accuracy,
    "Do the new multi-period spectra accurately reflect actual earthquake performance?",
    resolution_mechanism("Evaluation of structural performance in a M7.0+ event under the new code"),
    impact("If Mountain: The code is a true reflection of physics. If Rope: It is a conservative guess."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Performance-Based Seismic Design (PBSD)
 * Viability: Highly viable for tall buildings; allows for specific modeling 
 * rather than prescriptive code compliance.
 * Suppression: Suppressed by high expertise requirements and AHJ reluctance 
 * to approve non-standard designs.
 * * CONCLUSION:
 * The presence of PBSD prevents the code from being a total Mountain, 
 * keeping it in the Rope/Noose transition for sophisticated agents.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [asce_7_22_seismic_design].
% 2. Analysis: ?- multi_index_report(asce_7_22_seismic_design).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
