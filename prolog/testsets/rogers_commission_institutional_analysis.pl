% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: rogers_commission_institutional_analysis
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Report of the Presidential Commission on the Space Shuttle Challenger Accident (Rogers Commission)
% ============================================================================

:- module(constraint_rogers_commission_analysis, []).

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
 * * constraint_id: rogers_commission_institutional_analysis
 * human_readable: The Silent Safety Silo (NASA Decision-Making)
 * domain: political/technological
 * temporal_scope: 1985-1986
 * spatial_scope: National (USA)
 * * SUMMARY:
 * The Rogers Commission identified a "silent safety" constraint where critical 
 * technical data regarding O-ring erosion was filtered out before reaching 
 * top-level NASA management. This institutional silo created a 
 * false perception of safety, treating known technical flaws as "acceptable 
 * flight risks" rather than terminal threats.
 * * KEY AGENTS:
 * - Field Engineers (Thiokol): Individual powerless; possessed the data but lacked 
 * the institutional authority to stop the launch.
 * - NASA Marshall Center Management: Institutional; enforced the operational 
 * tempo and suppressed dissenting technical reports.
 * - The Commission (Analytical): The retrospective body identifying the 
 * structural "failure of communication".
 * * NARRATIVE ARC:
 * The pressure to maintain a rigorous launch schedule (the "Operational 
 * Milestone") functioned as a Snare. It extracted engineering integrity 
 * and safety margins in favor of institutional prestige, culminating in the 
 * catastrophic loss of the Challenger.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(challenger_post_mortem, 0, 10).
narrative_ontology:constraint_claim(rogers_commission_institutional_analysis, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.85. The system extracted human life and the survival of the 
% shuttle program to feed the "operational" reputation of NASA.
domain_priors:base_extractiveness(rogers_commission_institutional_analysis, 0.85).

% Suppression score (0.0-1.0)
% Rationale: 0.75. The Commission found that critical information about 
% previous O-ring erosion was not communicated to the highest levels, 
% effectively suppressing alternative risk assessments.
domain_priors:suppression_score(rogers_commission_institutional_analysis, 0.75).

% Enforcement requirements
% Requires active enforcement (Management "Launch Fever" and the marginalization 
% of the safety reporting structure).
domain_priors:requires_active_enforcement(rogers_commission_institutional_analysis).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, extractiveness, 0.85).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, suppression_requirement, 0.75).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(rogers_commission_institutional_analysis, institutional_launch_tempo).
constraint_victim(rogers_commission_institutional_analysis, engineering_truth).
constraint_victim(rogers_commission_institutional_analysis, crew_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: FIELD ENGINEER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the reporting structure and the 
         unyielding "burden of proof" required to stop a launch.
   WHEN: immediate - Focused on the specific cold-weather launch window.
   WHERE: trapped - Could only voice concerns through the very managers 
         pressuring for a launch.
   SCOPE: local - Specific joint hardware and seal dynamics.
   
   WHY THIS CLASSIFICATION:
   To the engineers like Boisjoly, the "Mountain" was the physical reality 
   of material science (O-ring resiliency). However, the institutional 
   process made their data invisible, meaning the "Mountain" was not 
   perceived by the decision-makers until the moment of impact.
   
   NARRATIVE EVIDENCE:
   "The Commission concluded that Thiokol management reversed its position... 
   at the urging of NASA and contrary to the views of its engineers".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rogers_commission_institutional_analysis,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(rogers_commission_institutional_analysis),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NASA LEVEL I/II MGMT - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Authorized to set launch criteria and safety norms.
   WHEN: biographical - Careers built on the Shuttle program's perceived reliability.
   WHERE: mobile - Had the capacity to alter the schedule but chose a 
         "can-do" coordination approach.
   SCOPE: national - Managing the premiere US space asset.
   
   WHY THIS CLASSIFICATION:
   Management viewed the O-ring erosion history as a "Rope"—a manageable 
   technical anomaly that had been "normalized" through successful flights. 
   They believed they were coordinating a complex system towards a necessary 
   milestone.
   
   NARRATIVE EVIDENCE:
   "NASA continued a pattern of flying with a known technical problem... 
   accepting the risk rather than correcting it".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rogers_commission_institutional_analysis,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(rogers_commission_institutional_analysis, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: ROGERS COMMISSION (RECAP) - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Independent observers tasked with finding the "flaw".
   WHEN: historical - Determining the long-term impact on the space program.
   WHERE: analytical - Not bound by the internal culture of NASA.
   SCOPE: global - A cautionary tale for all high-reliability organizations.
   
   WHY THIS CLASSIFICATION:
   The Commission's final judgment identifies the system as a Snare. By 
   isolating top-level management from technical dissent, the institutional 
   structure "choked off" the very safety data it was designed to ingest, 
   rendering the disaster inevitable.
   
   NARRATIVE EVIDENCE:
   "The Commission is troubled that... critical information was not shared... 
   representing a serious flaw in the decision-making process".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rogers_commission_institutional_analysis,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(rogers_commission_institutional_analysis, S),
    S > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rogers_commission_tests).

test(multi_perspective_gap_detection) :-
    % Verify that the Engineer sees a Mountain while Management sees a Rope
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, Type1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, Type2, context(institutional, biographical, mobile, national)),
    Type1 \= Type2.

test(high_extraction_audit) :-
    % Audit should trigger high alert for extraction/suppression
    domain_priors:base_extractiveness(rogers_commission_institutional_analysis, E),
    E > 0.8.

:- end_tests(rogers_commission_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION SCORE (0.75): High because the report explicitly notes 
 * that the communication system "failed" to provide the required data to 
 * those at the top. 
 * 2. CLASSIFICATION: Labeled primarily as a 'Snare' because the extraction 
 * (loss of life) and the active marginalization of safety engineers created 
 * a terminal feedback loop.
 */

omega_variable(
    institutional_memory_persistence,
    "Can a Snare-type organizational structure ever truly be reformed into a Rope without a total purge of leadership?",
    resolution_mechanism("Longitudinal study of NASA cultural shifts post-Challenger vs. the Columbia 2003 disaster"),
    impact("If Yes: Cultural reform is a Rope. If No: Institutional failure is a generational Mountain."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Independent Safety Reporting Line
 * Viability: Proposed by engineers but rejected/marginalized by Center 
 * management to avoid launch delays.
 * Suppression: Actively suppressed by requiring "proof" that it was 
 * UNSAFE to fly, rather than requiring proof that it was SAFE.
 * * CONCLUSION:
 * The reversal of the "burden of proof" is the definitive signature of an 
 * extractive Snare.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [rogers_commission_institutional_analysis].
% 2. Analyze: ?- multi_index_report(rogers_commission_institutional_analysis).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
