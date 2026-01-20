% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: burden_of_proof_engineering_safety
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Report of the Presidential Commission on the Space Shuttle Challenger Accident (Rogers Commission)
% ============================================================================

:- module(constraint_burden_of_proof_engineering, []).

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
 * * constraint_id: burden_of_proof_engineering_safety
 * human_readable: Reversal of the Engineering Burden of Proof
 * domain: technological/institutional
 * temporal_scope: 1986 (Challenger Launch Decision)
 * spatial_scope: National (NASA Marshall Space Flight Center)
 * * SUMMARY:
 * This constraint represents a specific institutional failure where the 
 * "burden of proof" shifted from requiring proof that it was SAFE to fly 
 * to requiring proof that it was UNSAFE to fly. This technical/
 * engineering variety of the constraint is distinct from legal burdens 
 * as it operates on the threshold of physical system failure rather 
 * than social culpability.
 * * KEY AGENTS:
 * - Roger Boisjoly: Individual powerless; engineer attempting to prove un-safety under a reversed burden.
 * - Lawrence Mulloy: Institutional; NASA official who demanded "conclusive" data to stop the launch.
 * - The Rogers Commission: Analytical; identified this shift as a primary cause of the disaster.
 * * NARRATIVE ARC:
 * Traditionally, aerospace engineering operates as a Rope—a conservative 
 * coordination mechanism ensuring safety. During the Challenger launch 
 * sequence, management enforced a Noose by demanding that engineers 
 * prove a system failure would occur with 100% certainty, effectively 
 * extracting the "safety margin" to meet a launch schedule.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(challenger_launch_decision, 0, 10).
narrative_ontology:constraint_claim(burden_of_proof_engineering_safety, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.9. Reversing the burden of proof extracts the life of the 
% subjects (crew) to preserve the institutional schedule and prestige.
domain_priors:base_extractiveness(burden_of_proof_engineering_safety, 0.9).

% Suppression score (0.0-1.0)
% Rationale: 0.8. The demand for "conclusive data" suppresses valid 
% engineering intuition and historical erosion data.
domain_priors:suppression_score(burden_of_proof_engineering_safety, 0.8).

% Enforcement requirements
% Requires active enforcement: NASA management explicitly demanded that 
% Thiokol "prove it" was unsafe.
domain_priors:requires_active_enforcement(burden_of_proof_engineering_safety).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, extractiveness, 0.9).
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(burden_of_proof_engineering_safety, launch_schedule_adherence).
constraint_victim(burden_of_proof_engineering_safety, engineering_integrity).
constraint_victim(burden_of_proof_engineering_safety, crew_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THIOKOL ENGINEER (Boisjoly) - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Lacked the power to withhold launch approval 
         once management reversed the burden.
   WHEN: immediate - The final hours of the launch decision.
   WHERE: trapped - Could not provide the "quantitative" proof demanded.
   SCOPE: local - Specific seal performance at 31 degrees Fahrenheit.
   
   WHY THIS CLASSIFICATION:
   For the engineer, the reversed burden was an unchangeable Mountain. 
   Physics does not provide "binary proof" of failure before it happens; 
   it only provides probabilities. By demanding certainty, management 
   created an impossible landscape for the engineer.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_engineering_safety,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(burden_of_proof_engineering_safety),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NASA LEVEL III MANAGEMENT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power over the launch process.
   WHEN: biographical - Seeking to justify the Shuttle as an operational system.
   WHERE: mobile - Had the capacity to delay but viewed the burden shift as 
         a necessary management tool to resolve "ambiguity".
   SCOPE: national - Managing the premiere US space asset.
   
   WHY THIS CLASSIFICATION:
   Management viewed the demand for "conclusive" proof as a Rope—a necessary 
   mechanism to coordinate across agencies and prevent "emotional" 
   delays by engineers. They saw it as a functional filter for data.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_engineering_safety,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(burden_of_proof_engineering_safety, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: ROGERS COMMISSION - Noose
   --------------------------------------------------------------------------
   
   WHO: analytical - Retrospective body examining the systemic "flaw".
   WHEN: historical - Evaluating the collapse of NASA's safety culture.
   WHERE: analytical - Free from the internal "can-do" culture pressure.
   SCOPE: global - A warning for all technical industries.
   
   WHY THIS CLASSIFICATION:
   The Commission correctly identified this constraint as a Noose. It 
   actively choked out dissenting safety data and "extracted" the life 
   of the crew to serve the illusion of a reliable schedule.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_engineering_safety,
    noose,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(burden_of_proof_engineering_safety, S),
    S > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(burden_of_proof_tests).

test(multi_perspective_asymmetry) :-
    % Engineer (Mountain) vs Manager (Rope) vs Commission (Noose)
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, T2, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(high_extraction_threshold) :-
    domain_priors:base_extractiveness(burden_of_proof_engineering_safety, E),
    E > 0.8.

:- end_tests(burden_of_proof_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. DOMAIN DIFFERENTIATION: This is a **technical/engineering** burden of proof. 
 * Unlike a legal burden (e.g., "beyond reasonable doubt" in criminal law), 
 * this constraint is extractive of physical safety margins, not just social 
 * fairness.
 * * 2. EXTRACTIVENESS SCORE (0.9): The score is high because the reversal of 
 * the burden directly trade-offs human life for schedule adherence.
 * * 3. PERSPECTIVE SELECTION: Chose the engineer's view as 'Mountain' to reflect 
 * the unyielding nature of physics that they were forced to "prove" 
 * conclusively.
 */

omega_variable(
    proof_threshold_quantification,
    "Can an engineering organization ever define a 'safe' burden of proof 
     that is not ultimately a subjective value judgment?",
    resolution_mechanism("Comparative analysis of probabilistic risk assessment (PRA) standards across nuclear vs. aerospace"),
    impact("If Yes: Burden of proof is a Rope. If No: It is an unavoidable institutional Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Standard Engineering Conservative Bias
 * Viability: The historical norm at NASA during Apollo. Requires proof 
 * of SAFETY before proceed.
 * Suppression: Actively suppressed by Level III management to maintain 
 * the "operational" rather than "experimental" status of the Shuttle.
 * * CONCLUSION:
 * The presence of a safer, traditional alternative that was suppressed 
 * solidifies the Noose classification for this specific instance.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [burden_of_proof_engineering_safety].
% 2. Analyze: ?- multi_index_report(burden_of_proof_engineering_safety).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
