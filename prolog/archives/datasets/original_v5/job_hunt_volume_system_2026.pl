% ============================================================================
% CONSTRAINT STORY: job_hunt_volume_system_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Modern Recruitment Narrative (User Submission)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(job_hunt_volume_system_2026, []).

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
 * * constraint_id: job_hunt_volume_system_2026
 * human_readable: The Algorithmic Volume Filter
 * domain: economic/social
 * temporal_scope: 2011-2026 (The "15-year" transition)
 * spatial_scope: Global (Digital Labor Market)
 * * SUMMARY:
 * Modern job hunting has transitioned from a meritocratic "front door" process to an 
 * automated volume-management system. The system is mathematically designed 
 * to handle mass scale (600+ applications per role) rather than talent identification, 
 * resulting in a "trap" where human attention is limited to roughly 30 seconds.
 * * KEY AGENTS:
 * - The Rule-Following Applicant: (Individual Powerless) Views the process as a meritocracy.
 * - The System Architect (HR/ATS): (Institutional) Manages volume via databases and filters.
 * - The Networking "Bypasser": (Individual Moderate) Navigates the "side door".
 * * NARRATIVE ARC:
 * The shift from "apply and be seen" to "apply and be indexed" creates a system where 
 * playing by the rules leads to a loss of agency. Meritocracy is now 
 * a secondary layer to the primary constraint of database management.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(job_hunt_volume_system_2026, 0, 10).
narrative_ontology:constraint_claim([job_hunt_volume_system_2026], [algorithmic_governance]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Extremely high asymmetry. 600 units of labor (applications) 
% provided for 30 seconds of human value; 599 units are "database filler".
domain_priors:base_extractiveness(job_hunt_volume_system_2026, 0.75).

% Suppression score (0.0-1.0)
% Rationale: Meritocratic narratives ("send good CV, get job") are still 
% socially enforced despite being functionally obsolete.
domain_priors:suppression_score(job_hunt_volume_system_2026, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, suppression_requirement, 0.6).

% Enforcement: Requires active enforcement
domain_priors:requires_active_enforcement(job_hunt_volume_system_2026).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(job_hunt_volume_system_2026, corporate_hr_efficiency).
constraint_victim(job_hunt_volume_system_2026, traditional_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RULE-FOLLOWING APPLICANT - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (No control over database filtering)
   WHEN: immediate (Searching for employment now)
   WHERE: trapped (Bounded by the "Front Door" portal)
   SCOPE: national/global
   
   WHY THIS CLASSIFICATION:
   The applicant experiences the system as a "Snare" because following the rules 
   guarantees loss; the front door is a trap where their "talent" is 
   rendered invisible by the volume of 600 applications.
   
   NARRATIVE EVIDENCE:
   "the front door is a trap... if youre playing by the rules youre already losing".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    job_hunt_volume_system_2026,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSTITUTIONAL SYSTEM - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power of recruitment)
   WHEN: biographical (Hiring cycles/Corporate growth)
   WHERE: mobile (Can switch platforms/ATS providers)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the institution, the system is a "Rope"—a vital coordination tool used 
   to manage unmanageable volume. It is not designed to find talent, but to 
   handle 600 applications without overwhelming HR staff.
   
   NARRATIVE EVIDENCE:
   "the system is designed to handle volume, not find talent".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    job_hunt_volume_system_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MARKET ANALYST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of market forces)
   WHEN: historical (The last 15 years of digitization)
   WHERE: analytical (Unconstrained by individual search)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The high-volume environment is a "Mountain"—an immutable reality of the 
   digital era. The ease of applying via the internet makes 600 applications 
   per role a natural law of modern supply and demand.
   
   NARRATIVE EVIDENCE:
   "it hasn't worked like that in 15 years".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    job_hunt_volume_system_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(job_hunt_volume_system_2026_tests).

test(multi_perspective_job_hunt) :-
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, Type1, context(powerless, _, trapped, _)),
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, Type2, context(institutional, _, mobile, _)),
    Type1 \= Type2.

test(extraction_scaling) :-
    domain_priors:base_extractiveness(job_hunt_volume_system_2026, E),
    E > 0.7.

:- end_tests(job_hunt_volume_system_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS (0.75):
 * The model chose high extractiveness because the system consumes massive 
 * amounts of human hope and labor (600 applications) while providing 
 * negligible attention (30 seconds).
 * * 2. MANDATROPHY RESOLUTION:
 * The "predatory" nature of the 30-second filter is a Snare for the jobless, 
 * but a functional Rope for HR departments that would otherwise collapse 
 * under manual review of 600 CVs.
 * * 3. PERSPECTIVE SELECTION:
 * Analyzed from the view of the applicant (trapped in the portal), the 
 * institution (using the portal as a tool), and the historian (viewing 
 * the 15-year shift as an immutable market fact).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    side_door_meritocracy,
    "Is the 'side door' truly a meritocracy or simply a transfer of social capital (Snare for the unnetworked)?",
    resolution_mechanism("Comparative analysis of performance metrics between 'Side Door' hires and 'Front Door' survivors"),
    impact("If social capital only: The entire system is a double Snare. If performance-based: Side Door is a genuine Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    job_hunt_volume_system_2026_extraction_intent,
    "Is the 0.75 extraction a deliberate filtering strategy or a side effect of poor ATS interface design?",
    resolution_mechanism("Audit of HR budget allocation toward 'Talent Search' vs. 'Volume Filtering' software"),
    impact("If deliberate: Predatory Snare. If side-effect: Correctable Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Human-First Review
 * Viability: Historically the standard ("15 years ago").
 * Suppression: Economically suppressed by the sheer volume of digital applications.
 * * ALTERNATIVE 2: Referral-Only Models
 * Viability: The "Side Door" mentioned in the text.
 * Suppression: Often hidden by the performative "Front Door" mandates of 
 * equal opportunity compliance.
 * * CONCLUSION:
 * The presence of a viable Side Door alternative shifts the Front Door 
 * from a potential Rope into a definitive Snare for those without the 
 * "mobile" exit option of a professional network.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [job_hunt_volume_system_2026].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
