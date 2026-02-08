% ============================================================================
% CONSTRAINT STORY: radiologic_diagnostic_threshold
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Indexical Relativity in Radiology
% ============================================================================

:- module(constraint_radiologic_diagnostic_threshold, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: radiologic_diagnostic_threshold
 * human_readable: The Radiologic Diagnostic Threshold
 * domain: medical/technological/legal
 * temporal_scope: Contemporary clinical practice (approx. 1990-2026)
 * spatial_scope: Global (primarily Western medical systems)
 * * SUMMARY:
 * This constraint governs the classification of medical imaging findings into "benign," "incidental," or "critical." 
 * It is not a purely biological fact but an indexical classification dependent on the reader's 
 * power level, legal exposure, and technological tools.
 * * KEY AGENTS:
 * - Frontline Radiologist: High-volume reader facing legal and cognitive pressure.
 * - Hospital Administrator/Guideline Author: Institutional body defining "standard of care".
 * - AI Vendor: Entity offering coordination tools to optimize workflow.
 * - Retrospective Audit Panel: Hindsight-biased observers reclassifying past ambiguity as obvious.
 * * NARRATIVE ARC:
 * Findings oscillate between "ignore" and "urgent" as contextual priors change. Frontline 
 * workers experience the threshold as a Snare of liability, while institutions present it 
 * as a Mountain of objective safety.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(radiology_r7, 0, 10).
narrative_ontology:constraint_claim(radiologic_diagnostic_threshold, mountain).

% Base extractiveness score (0.7: Significant risk/labor shift to individual readers)
% Rationale: Defensive reading and volume pressure extract cognitive health from clinicians.
domain_priors:base_extractiveness(radiologic_diagnostic_threshold, 0.7).

% Suppression score (0.6: Moderate-high)
% Rationale: Alternatives like "watchful waiting" are often punished by malpractice systems.
domain_priors:suppression_score(radiologic_diagnostic_threshold, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, extractiveness, 0.7).
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, suppression_requirement, 0.6).

% Enforcement requirements
domain_priors:requires_active_enforcement(radiologic_diagnostic_threshold).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(radiologic_diagnostic_threshold, [malpractice_insurers, hospital_admins, ai_vendors]).
constraint_victim(radiologic_diagnostic_threshold, [frontline_radiologists, overdiagnosed_patients]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Frontline Radiologist - Snare
   --------------------------------------------------------------------------
   WHO: powerless (subject to volume/legal pressure)
   WHEN: immediate (daily diagnostic turnaround)
   WHERE: trapped (legal/institutional framework is non-optional)
   SCOPE: local (individual cases)
   
   WHY THIS CLASSIFICATION:
   For the radiologist, the protocol functions as a Snare. High cognitive load 
   combined with fear of misses forces defensive "over-calling," turning a 
   diagnostic aid into a coercive mechanism of liability management.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    radiologic_diagnostic_threshold,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(radiologic_diagnostic_threshold, E),
    E > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Hospital Administrator / Guideline Author - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (rule-making power)
   WHEN: generational (long-term standard of care)
   WHERE: constrained (bound by "evidence-based" consensus)
   SCOPE: regional (hospital system-wide)
   
   WHY THIS CLASSIFICATION:
   From the institutional view, these thresholds are Mountains. They are presented 
   as non-optional "natural laws" of patient safety and evidence-based necessity, 
   effectively masking the underlying social and legal negotiations.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    radiologic_diagnostic_threshold,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :-
    % Perceived as unchangeable standard of care
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: AI Tool Vendor / Workflow Optimizer - Rope
   --------------------------------------------------------------------------
   WHO: powerful (significant influence over tool design)
   WHEN: biographical (product lifecycle)
   WHERE: arbitrage (can move between different hospital systems)
   SCOPE: global (product scalability)
   
   WHY THIS CLASSIFICATION:
   For vendors, these thresholds are Ropes. They are coordination devices that 
   can be optimized via structured reporting and checklists to reduce 
   variability and increase throughput.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    radiologic_diagnostic_threshold,
    rope,
    context(
        agent_power(powerful),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    % Perceived as a functional coordination mechanism
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(radiologic_diagnostic_threshold_tests).

test(multi_perspective_radiology) :-
    % Radiologist sees Snare
    constraint_indexing:constraint_classification(
        radiologic_diagnostic_threshold, Type1,
        context(powerless, immediate, trapped, local)),
    % Admin sees Mountain
    constraint_indexing:constraint_classification(
        radiologic_diagnostic_threshold, Type2,
        context(institutional, generational, constrained, regional)),
    % AI Vendor sees Rope
    constraint_indexing:constraint_classification(
        radiologic_diagnostic_threshold, Type3,
        context(powerful, biographical, arbitrage, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_radiology) :-
    % Powerless experience more extraction (liability/burnout) than Institutional
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, generational, constrained, regional),
    constraint_indexing:extractiveness_for_agent(radiologic_diagnostic_threshold, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(radiologic_diagnostic_threshold, ContextPowerful, Score2),
    Score1 > Score2.

test(hindsight_mountain_effect) :-
    % Analytical retrospective stance often sees errors as "obvious" (Mountain)
    constraint_indexing:constraint_classification(
        radiologic_diagnostic_threshold,
        mountain,
        context(analytical, historical, analytical, global)).

:- end_tests(radiologic_diagnostic_threshold_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.7): High score reflects the "liability tax" and cognitive 
 * exhaustion of frontline clinicians who bear the risk of the system's 
 * threshold shifts.
 * * 2. PERSPECTIVE SELECTION: Focused on the triad of Clinician (Snare), Admin 
 * (Mountain), and Tech Optimizer (Rope) to highlight the "perspectival stalemate" 
 * inherent in diagnostic error discussions.
 * * 3. OMEGAS:
 * omega_variable(pixel_ground_truth,
 * "Is there an objective 'correct' threshold independent of context?",
 * resolution_mechanism("Longitudinal outcome tracking vs. social consensus"),
 * impact("If Mountain: error is an absolute failure. If Rope: error is a coordination failure."),
 * confidence_without_resolution(low)
 * ).
 * * omega_variable(ai_rewiring_risk,
 * "Will AI transform the Snare into a Rope, or simply tighten the Snare by accelerating volume?",
 * resolution_mechanism("Post-implementation clinician burnout and error-rate audits"),
 * impact("Determines if AI is a tool or a coercive metric."),
 * confidence_without_resolution(medium)
 * ).
 * * 4. CONFIDENCE: High regarding the indexical nature of "error" vs "discrepancy" 
 * as documented in radiology literature.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Bayesian Probabilistic Reporting
 * Viability: Reporting findings as probability ranges rather than binary binary (ignore/urgent).
 * Suppression: High; current legal and billing systems require discrete 
 * classifications to function.
 * * ALTERNATIVE 2: Outcome-Weighted Compensation
 * Viability: Rewarding accuracy over volume.
 * Suppression: Suppressed by institutional volume-based revenue models (Snare).
 * * CONCLUSION:
 * The active suppression of probabilistic nuance in favor of "standard of care" 
 * certainty shifts this constraint from a potential Rope into a functioning Snare 
 * for the frontline practitioner.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * ?- [constraint_radiologic_diagnostic_threshold].
 * ?- constraint_indexing:multi_index_report(radiologic_diagnostic_threshold).
 * ?- run_tests(radiologic_diagnostic_threshold_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
