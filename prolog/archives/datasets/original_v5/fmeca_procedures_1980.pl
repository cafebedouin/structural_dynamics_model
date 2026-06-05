% ============================================================================
% CONSTRAINT STORY: fmeca_procedures_1980
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: MIL-STD-1629A - Failure Mode, Effects, and Criticality Analysis (1980)
% ============================================================================

:- module(constraint_fmeca_procedures_1980, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: fmeca_procedures_1980
 * human_readable: MIL-STD-1629A (FMECA Procedures)
 * domain: technological/military/legal
 * temporal_scope: 1980 - Present
 * spatial_scope: United States (DoD acquisition environment)
 * * SUMMARY:
 * This constraint establishes the mandatory procedures for performing a Failure 
 * [cite_start]Mode, Effects, and Criticality Analysis (FMECA) for DoD systems[cite: 64, 68]. 
 * It functions as a rigid documentation and analysis framework intended to 
 * systematically identify catastrophic and critical failure possibilities 
 * [cite_start]early in the design phase[cite: 31, 41, 65].
 * * KEY AGENTS:
 * - Junior Design Engineer: The individual who must complete the worksheets; 
 * often views the requirement as a bureaucratic hurdle.
 * - DoD Program Manager: The institutional agent who mandates the standard 
 * [cite_start]to ensure "peace, justice and progress" through system reliability[cite: 1, 25].
 * - System Safety Analyst: The observer who views FMECA as a logical 
 * necessity for managing the inherent "natural law" of mechanical failure.
 * * NARRATIVE ARC:
 * [cite_start]The FMECA starts as an "essential function" [cite: 31] intended to be a 
 * flexible "Rope" for coordination. However, due to "untimeliness" and "isolated 
 * [cite_start]performance"[cite: 39, 40], it often hardens into a "Snare" for contractors 
 * [cite_start]who must fulfill rigid worksheet requirements [cite: 182] after design 
 * decisions are already finalized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(fmeca_standard_era, 1980, 2026).
narrative_ontology:constraint_claim(fmeca_procedures_1980, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). While it aims for safety, it "extracts" significant 
% labor and time from contractors to satisfy procuring activity data requirements 
% [cite: 33, 45, 1056].
domain_priors:base_extractiveness(fmeca_procedures_1980, 0.4).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.5). It strictly mandates specific coding systems 
% (MIL-STD-780) [cite: 198] [cite_start]and worksheet formats[cite: 182], punishing 
% alternative risk-management styles through contract non-compliance.
domain_priors:suppression_score(fmeca_procedures_1980, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(fmeca_procedures_1980, extractiveness, 0.4).
narrative_ontology:constraint_metric(fmeca_procedures_1980, suppression_requirement, 0.5).

% Enforcement: Requires active maintenance by the procuring activity and 
% contracting officers[cite: 109, 169].
domain_priors:requires_active_enforcement(fmeca_procedures_1980).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(fmeca_procedures_1980, department_of_defense).
constraint_beneficiary(fmeca_procedures_1980, system_safety).
constraint_victim(fmeca_procedures_1980, contractors).
constraint_victim(fmeca_procedures_1980, innovative_design_flexibility).

% Metrics for Section 1 of the Executive Summary
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: JUNIOR DESIGN ENGINEER - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Must complete the work without shaping the standard.
   WHEN: immediate - Focused on meeting the current project deadline.
   [cite_start]WHERE: trapped - Bound by the "Ordering Data" in the specific contract[cite: 109, 541].
   [cite_start]SCOPE: local - Focused on a single component or assembly level[cite: 154].
   
   WHY THIS CLASSIFICATION:
   The engineer sees FMECA as a coercive "Snare" of paperwork. The "chief causes" 
   [cite_start]of criticism are its "untimeliness" [cite: 39]—it feels like a post-facto 
   [cite_start]requirement that "extracts" time without improving the design[cite: 38].
   
   NARRATIVE EVIDENCE:
   "Probably the greatest criticism of the FMECA has been its limited use 
   [cite_start]in improving designs... chief causes for this have been untimeliness"[cite: 38, 39].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fmeca_procedures_1980,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: DOD PROGRAM MANAGER - Rope
   --------------------------------------------------------------------------
   
   [cite_start]WHO: institutional - Has the power to "tailor the requirements"[cite: 34, 1012].
   WHEN: biographical - Spanning the acquisition lifecycle (years to decades).
   [cite_start]WHERE: mobile - Can select which tasks (101-105) to impose[cite: 1028].
   [cite_start]SCOPE: national - Applies to "all designated DoD systems"[cite: 68].
   
   WHY THIS CLASSIFICATION:
   For the manager, FMECA is a "Rope" for functional coordination. It's a 
   [cite_start]tool to "tailor" [cite: 34, 1043] and ensure that various reliability 
   [cite_start]tasks are coordinated to prevent "duplication of efforts"[cite: 45].
   
   NARRATIVE EVIDENCE:
   "The FMECA must contribute meaningfully to program decision... invaluable 
   [cite_start]to those who are responsible for making program decisions"[cite: 35, 36].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    fmeca_procedures_1980,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: SAFETY ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of universal failure patterns.
   [cite_start]WHEN: historical - Based on decades of engineering experience[cite: 1091, 1134].
   [cite_start]WHERE: analytical - Sees failure as an inherent property of all "items"[cite: 194, 231].
   [cite_start]SCOPE: global - Applies to the "nature of the design process itself"[cite: 32].
   
   WHY THIS CLASSIFICATION:
   The analyst sees the "potential for failure" as an unchangeable "Mountain" 
   of reality. Systematic analysis is not an option but a "logical necessity" 
   [cite_start]dictated by the laws of physics and probability[cite: 124, 150].
   
   NARRATIVE EVIDENCE:
   "FMECA is an essential function in design... [it] must be iterative to 
   [cite_start]correspond with the nature of the design process itself"[cite: 31, 32].
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    fmeca_procedures_1980,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(fmeca_procedures_1980_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(fmeca_procedures_1980, Type1, context(powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(fmeca_procedures_1980, Type2, context(institutional, biographical, mobile, national)),
    Type1 = snare,
    Type2 = rope.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, mobile, national),
    constraint_indexing:extractiveness_for_agent(fmeca_procedures_1980, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(fmeca_procedures_1980, ContextPowerful, Score2),
    Score1 > Score2.

test(tailoring_as_rope_flexibility) :-
    % Tailoring (mobile exit option) allows institutional power to view it as a Rope.
    constraint_indexing:constraint_classification(fmeca_procedures_1980, rope, context(institutional, _, mobile, _)).

:- end_tests(fmeca_procedures_1980_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.4): Chosen because while FMECA is safety-critical, 
 * [cite_start]the standard admits it is often seen as a burden[cite: 38]. It extracts 
 * [cite_start]"valuable resources" [cite: 1056] in exchange for theoretical safety.
 * * 2. SUPPRESSION SCORE (0.5): The standard enforces a very specific way of 
 * [cite_start]thinking (severity categories, coding)[cite: 267, 707]. Other methods 
 * exist but are functionally "suppressed" by the acquisition contract.
 * * 3. PERSPECTIVE SELECTION: Chose the Engineer (victim of untimeliness), 
 * Manager (agent of tailoring), and Analyst (observer of failure's nature).
 * * 4. AMBIGUITIES: Resolved the "standard vs tailoring" tension by assigning 
 * "Rope" status to those with the power to tailor and "Snare" status to 
 * those forced to comply with the untailored defaults.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fmeca_actual_impact,
    'Does FMECA actually improve design, or is its criticism as \'limited use\' inherently true?',
    resolution_mechanism('Longitudinal study of failure rates in FMECA-compliant vs non-compliant complex systems'),
    impact('If Mountain (physics-driven): safety increases. If Snare (bureaucracy-driven): only paperwork increases.'),
    confidence_without_resolution(medium)
).

omega_variable(
    tailoring_competence,
    'Is the procuring activity competent to \'tailor\' requirements appropriately, or does it default to maximum suppression?',
    resolution_mechanism('Audit of tailoring decisions across diverse DoD programs'),
    impact('If tailored: Rope. If default: Snare for all contractors.'),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Informal Designer Intuition
 * Viability: Historically how many machines were built before formal FMEA.
 * [cite_start]Suppression: Banned by Paragraph 4.1 requiring "planned and performed" analysis[cite: 169].
 * * ALTERNATIVE 2: Purely Qualitative "Brainstorming"
 * Viability: Faster, lower "extractiveness" of labor.
 * [cite_start]Suppression: Rejected by requirements for "quantitative" criticality numbers (Cm)[cite: 853, 1090].
 * * CONCLUSION:
 * The active rejection of informal/qualitative alternatives in favor of 
 * specific worksheet formats shifts the classification from a coordination 
 * Rope to an enforced Snare for those doing the work.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [fmeca_procedures_1980].
% Run tests: ?- run_tests(fmeca_procedures_1980_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
