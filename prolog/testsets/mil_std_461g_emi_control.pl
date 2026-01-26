% ============================================================================
% CONSTRAINT STORY: mil_std_461g_emi_control
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: MIL-STD-461G - Control of Electromagnetic Interference Characteristics
% ============================================================================

:- module(constraint_mil_std_461g_emi_control, []).

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
 * * constraint_id: mil_std_461g_emi_control
 * human_readable: MIL-STD-461G EMI Interface Standard
 * domain: technological/military/electronic
 * temporal_scope: 2015 - Present
 * spatial_scope: Global (DoD systems and equipment)
 * * SUMMARY:
 * MIL-STD-461G establishes interface requirements for the control of electromagnetic 
 * interference (EMI) characteristics of subsystems and equipment. 
 * It is designed to ensure that equipment functions within its designated design 
 * tolerances in a complex military electromagnetic environment.
 * * KEY AGENTS:
 * - Equipment Contractor: The entity responsible for designing and testing hardware 
 * to meet these specific, often costly, requirements.
 * - Procuring Activity (DoD Official): The institutional power that enforces 
 * compliance but has the authority to "tailor" the requirements.
 * - EMI Engineer: The analytical observer who views these requirements as a 
 * formalization of the unchangeable laws of physics (electromagnetism).
 * * NARRATIVE ARC:
 * Originally conceived as a "Rope" for functional coordination to prevent 
 * platform-level failures, the standard often functions 
 * as a "Snare" for smaller contractors due to the high barrier of entry for 
 * specialized testing equipment and expertise.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(mil_std_461g_era, 2015, 2026).
narrative_ontology:constraint_claim(mil_std_461g_emi_control, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). Compliance requires expensive specialized testing 
% infrastructure, shielded cables, and expert labor.
domain_priors:base_extractiveness(mil_std_461g_emi_control, 0.5).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.4). While tailoring is possible, alternative commercial 
% standards (e.g., FCC) are often functionally suppressed/rejected for high-stakes 
% military applications.
domain_priors:suppression_score(mil_std_461g_emi_control, 0.4).

% Enforcement: Requires active maintenance (Testing, verification, and contract oversight).
domain_priors:requires_active_enforcement(mil_std_461g_emi_control).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(mil_std_461g_emi_control, extractiveness, 0.5).
narrative_ontology:constraint_metric(mil_std_461g_emi_control, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(mil_std_461g_emi_control, dod_mission_safety).
constraint_beneficiary(mil_std_461g_emi_control, test_lab_facilities).
constraint_victim(mil_std_461g_emi_control, small_defense_contractors).
constraint_victim(mil_std_461g_emi_control, innovative_untested_cots).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: DEFENSE CONTRACTOR - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Must comply with the standard to receive payment.
   WHEN: immediate - Focused on the current contract delivery and testing cycle.
   WHERE: trapped - Bound by the specific requirements stated in the contract.
   SCOPE: local - Focused on the specific subsystem being tested.
   
   WHY THIS CLASSIFICATION:
   The contractor sees the standard as a "Snare" because it imposes asymmetric 
   costs. Failure in a single EMI test (e.g., radiated emissions) can lead to 
   costly redesigns and project delays, effectively extracting resources 
   without guaranteed payoff.
   
   NARRATIVE EVIDENCE:
   "Tightening of connectors and introduction of additional isolation... better 
   shielded cables, alternative routing, or shielding barriers".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    mil_std_461g_emi_control,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: DOD PROCURING ACTIVITY - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Has the power to "tailor" the individual requirements.
   WHEN: biographical - Planning for the multi-decade lifecycle of a platform.
   WHERE: mobile - Can adjust the severity of requirements based on the platform.
   SCOPE: national - Applies to all DoD Departments and Agencies.
   
   WHY THIS CLASSIFICATION:
   For the DoD official, the standard is a "Rope" for functional coordination. 
   It ensures that different subsystems function together in a designated 
   electromagnetic environment (EME) without interfering with each other 
  .
   
   NARRATIVE EVIDENCE:
   "The procuring activity should consider tailoring the individual requirements 
   to be more or less severe based on the design features of the intended 
   platform".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    mil_std_461g_emi_control,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: EMI SPECIALIST / SCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the laws of electromagnetism.
   WHEN: historical - Based on the immutable nature of electronic interference.
   WHERE: analytical - Not physically trapped; observing the system.
   SCOPE: global - The physics of EMI apply to all equipment everywhere.
   
   WHY THIS CLASSIFICATION:
   The scientist sees the standard as a formalization of a "Mountain." The 
   requirements for control of EMI aren't arbitrary bureaucratic rules; they 
   are necessary responses to the unchangeable laws of physics that dictate 
   how electronic components interact.
   
   NARRATIVE EVIDENCE:
   " function within their designated design tolerances when operating in 
   their intended electromagnetic environment".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    mil_std_461g_emi_control,
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

:- begin_tests(mil_std_461g_emi_control_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(mil_std_461g_emi_control, Type1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(mil_std_461g_emi_control, Type2, context(institutional, biographical, mobile, national)),
    Type1 = snare,
    Type2 = rope.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, mobile, national),
    constraint_indexing:extractiveness_for_agent(mil_std_461g_emi_control, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(mil_std_461g_emi_control, ContextPowerful, Score2),
    Score1 > Score2.

test(analytical_mountain_observation) :-
    constraint_indexing:constraint_classification(mil_std_461g_emi_control, mountain, context(analytical, historical, analytical, global)).

:- end_tests(mil_std_461g_emi_control_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.5):
 * Reasoning: compliance is famously expensive. It requires specialized 
 * shielded chambers, unique antennas, and highly trained engineers.
 * Evidence: The standard explicitly mentions methods for checking and 
 * correcting problems with shielded cables and barriers.
 * * 2. SUPPRESSION SCORE (0.4):
 * Reasoning: while the document explicitly allows tailoring, 
 * this tailoring is an internal DoD process. For a contractor, the standard 
 * effectively suppresses cheaper commercial-grade EMI alternatives.
 * * 3. PERSPECTIVE SELECTION:
 * Chose Contractor (Snare), DoD Official (Rope), and Scientist (Mountain) 
 * to demonstrate the full spectrum of indexical truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tailoring_effectiveness,
    "Does tailoring actually reduce costs for contractors, or is it used primarily to increase severity?",
    resolution_mechanism("Audit of tailoring decisions across 100 DoD acquisition programs"),
    impact("If beneficial: Standard is a true Rope. If punitive: Standard is a Snare for all."),
    confidence_without_resolution(medium)
).

omega_variable(
    lab_vs_field_fidelity,
    "Do laboratory interface requirements actually prevent interference in the chaotic real-world battlefield?",
    resolution_mechanism("Compare lab-tested reliability against battlefield performance logs over 5 years"),
    impact("If High Fidelity: Mountain. If Low Fidelity: Scaffold (Hollow)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Commercial EMI Standards (FCC Part 15 / CISPR)
 * Viability: Much lower cost; ubiquitous in consumer electronics.
 * Suppression: Rejected for military use because it lacks the severity 
 * necessary for platform-level survivability.
 * * ALTERNATIVE 2: Pure Modeling and Simulation
 * Viability: Reduces the need for physical chambers.
 * Suppression: Currently used as a supplement, but the standard still 
 * mandates physical "verification" methods.
 * * CONCLUSION:
 * The presence of tailoring and specific verification methods cements the 
 * standard as a Rope for those with power, but the active rejection of 
 * commercial standards makes it a Snare for those tasked with compliance.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_mil_std_461g_emi_control].
% Run tests: ?- run_tests(mil_std_461g_emi_control_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
