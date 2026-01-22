% ============================================================================
% CONSTRAINT STORY: ai_driven_surveillance_sensor_layer
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: AI-Driven Surveillance and Indexical Perception
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_ai_surveillance, []).

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
 * * constraint_id: ai_driven_surveillance_sensor_layer
 * human_readable: AI-Driven Real-Time Surveillance
 * domain: technological/security/political
 * temporal_scope: 2020-2026 (Global rollout phase)
 * spatial_scope: Global (Urban centers/Smart Cities)
 * * SUMMARY:
 * This constraint represents the "sensor layer" of modern governance—facial recognition, 
 * gait analysis, and automated behavioral detection. It serves as the data 
 * supply for the Social Credit Architecture, turning public space into a readable ledger.
 * * KEY AGENTS:
 * - The Monitored Subject: Individual whose biometrics are captured in public.
 * - The Security Architect: Agency or vendor using AI to "secure" public order.
 * - The Systemic Outlier: Individuals with "high-risk" behavioral flags.
 * * NARRATIVE ARC:
 * Public space is re-engineered from a neutral "Mountain" of anonymity into a 
 * "Rope" of safety for the compliant and a "Noose" of real-time tracking for 
 * those flagged by the algorithm.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(ai_surveillance_v3, 0, 10).

% The system is claimed as a ROPE by its institutional proponents.
narrative_ontology:constraint_claim(ai_driven_surveillance_sensor_layer, rope).

% Base extractiveness score (0.75)
% Rationale: High extraction of cognitive/behavioral autonomy; continuous 
% monitoring extracts the "psychological safety" of privacy.
domain_priors:base_extractiveness(ai_driven_surveillance_sensor_layer, 0.75).

% Suppression score (0.8)
% Rationale: Anonymity in public space is actively "suppressed" by technical ubiquity.
domain_priors:suppression_score(ai_driven_surveillance_sensor_layer, 0.8).

% Enforcement: Emerges from technical deployment and ubiquitous data linkage.
domain_priors:requires_active_enforcement(ai_driven_surveillance_sensor_layer).

% Metrics for DR-Audit
narrative_ontology:constraint_metric(ai_driven_surveillance_sensor_layer, extractiveness, 0.75).
narrative_ontology:constraint_metric(ai_driven_surveillance_sensor_layer, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(ai_driven_surveillance_sensor_layer, [security_agencies, ai_vendors, urban_planners]).
constraint_victim(ai_driven_surveillance_sensor_layer, [privacy_advocates, ethnic_minorities, political_activists]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: The Monitored Activist - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (No control over the "Black Box" algorithms)
   WHEN: immediate (Real-time tracking and rapid response)
   WHERE: trapped (Cameras cover every exit and transit hub)
   SCOPE: local (The physical street/square)
   
   WHY THIS CLASSIFICATION:
   For the activist, AI surveillance is a Noose. It is an extractive tool that 
   uses biometric signatures to preemptively suppress dissent and eliminate the 
   possibility of anonymous association.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_driven_surveillance_sensor_layer,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(ai_driven_surveillance_sensor_layer, E),
    E > 0.7, % High extraction + trapped = Noose
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: The Security Architect - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making and tool-deploying authority)
   WHEN: historical (Designing long-term "Safe City" frameworks)
   WHERE: arbitrage (Can calibrate sensitivity and target parameters)
   SCOPE: national (Cross-jurisdictional data integration)
   
   WHY THIS CLASSIFICATION:
   For the architect, the sensor layer is a Rope. It is a coordination device 
   designed to reduce crime, optimize traffic flow, and ensure that the "social 
   ledger" is accurate and up-to-date.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_driven_surveillance_sensor_layer,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Passive Citizen - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (General public, non-dissenting)
   WHEN: biographical (The "new normal" of urban life)
   WHERE: constrained (Accepting cameras as part of the infrastructure)
   SCOPE: regional (City-wide standard)
   
   WHY THIS CLASSIFICATION:
   For the average citizen, the cameras become a Mountain. Like the weather or 
   the geography of the city, surveillance is perceived as an unchangeable 
   and neutral part of the environment that "has always been there".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_driven_surveillance_sensor_layer,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (Mandatrophy Resolution)
   ========================================================================== */

:- begin_tests(ai_surveillance_tests).

test(multi_perspective_variance) :-
    % Verify that high extraction (0.75) is resolved through indexical shift
    constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, Type1, 
        context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, Type2, 
        context(individual_moderate, biographical, constrained, regional)),
    Type1 = noose,
    Type2 = mountain.

test(mandatrophy_validation) :-
    % Verify base extractiveness meets the 0.75 threshold
    domain_priors:base_extractiveness(ai_driven_surveillance_sensor_layer, E),
    E == 0.75.

:- end_tests(ai_surveillance_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * [RESOLVED MANDATROPHY]: The extraction score of 0.75 represents the 
 * psychological tax of the "Panopticon effect". For the activist, 
 * this is a NOOSE because it preemptively suppresses dissent. 
 * For the passive citizen, it is a MOUNTAIN because the surveillance 
 * becomes a normalized, unchangeable background to urban life.
 * The high extraction is intentional to trigger indexical resolution.
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.75): High because the "extraction" is cognitive. It 
 * forces a state of constant self-monitoring (the Panopticon effect).
 * 2. PERSPECTIVE: Chose the "Passive Citizen" as Mountain to illustrate how 
 * tech-ubiquity breeds a sense of natural law (Normalization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - MANDATROPHY RESOLUTION
   ========================================================================== */

/**
 * OMEGA IDENTIFICATION
 * * Since base_extractiveness > 0.7, this Omega resolves the "Mandatrophy Gate"
 * by asking whether the high extraction is a predatory choice or a necessity.
 */

omega_variable(
    ai_surveillance_extraction_intent,
    "Is the 0.75 extraction of privacy a functional necessity for public safety or a predatory tool for political suppression?",
    resolution_mechanism("Audit of surveillance data usage: percentage of crime prevention vs political profiling"),
    impact("If necessity: Institutional Mountain. If predatory: Systemic Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    algorithmic_opacity,
    "Will biometric flagging ever be subject to individual appeal?",
    resolution_mechanism("Legislative review of 'right to explanation' in AI mandates"),
    impact("If No: Permanent Noose. If Yes: Potential Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Decentralized Proof-of-Personhood
 * Viability: Using zero-knowledge proofs to prove "I am human" without 
 * revealing "I am [Identity]".
 * Suppression: High. Security agencies reject anonymity because it breaks 
 * the linkage required for the Social Credit Noose.
 * * CONCLUSION:
 * The presence of suppressed alternatives (anonymity/SSI) shifts the 
 * classification toward NOOSE for those seeking exit.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE:
 * ?- [ai_driven_surveillance_sensor_layer].
 * ?- run_tests(ai_surveillance_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
