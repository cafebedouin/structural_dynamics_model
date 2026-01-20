% ============================================================================
% CONSTRAINT STORY: union_protection_underperformance
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Labor Relations Analysis / Collective Bargaining Agreements
% ============================================================================

:- module(constraint_union_job_protection, []).

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
 * * constraint_id: union_protection_underperformance
 * human_readable: Just-Cause Protection for Underperforming Employees
 * domain: economic/social
 * temporal_scope: Ongoing (Modern Unionized Labor)
 * spatial_scope: National/Regional (Unionized Sectors)
 * * SUMMARY:
 * This constraint refers to the "Just Cause" and due process provisions in 
 * collective bargaining agreements that make it difficult to terminate 
 * employees for sub-par performance without exhaustive documentation and 
 * remediation attempts. It is designed to prevent arbitrary firing but often 
 * results in the retention of chronic underperformers.
 * * KEY AGENTS:
 * - Underperforming Employee: Individual powerful (within the system); 
 * protected by procedural shields.
 * - Front-line Supervisor: Individual powerless; must navigate the 
 * bureaucratic burden of proof to effect change.
 * - The Union (Institutional): Rule-enforcing power; ensures procedural 
 * fidelity regardless of individual merit.
 * * NARRATIVE ARC:
 * These protections serve as a Rope for the collective (ensuring fairness). 
 * However, for the supervisor, they are an unyielding Mountain of red tape. 
 * For the organization and high-performing peers, the retention of 
 * underperformers can become a Noose, extracting morale and productivity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(labor_management_conflict_r1, 0, 10).
narrative_ontology:constraint_claim(union_protection_underperformance, rope).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.6. Moderate/High extraction; the system "extracts" wages 
% from the employer and labor from high-performing peers who must cover 
% the gap, in exchange for broad-based job security.
domain_priors:base_extractiveness(union_protection_underperformance, 0.6).

% Suppression score (0.0-1.0)
% Rationale: 0.5. Alternatives (merit-based at-will employment) are 
% visible but strictly suppressed within the unionized environment 
% through the legal force of the contract.
domain_priors:suppression_score(union_protection_underperformance, 0.5).

% Enforcement requirements
% Requires active enforcement (Grievance procedures, arbitration, shop 
% steward monitoring).
domain_priors:requires_active_enforcement(union_protection_underperformance).

% Metrics required for Executive Summary
narrative_ontology:constraint_metric(union_protection_underperformance, extractiveness, 0.6).
narrative_ontology:constraint_metric(union_protection_underperformance, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(union_protection_underperformance, underperforming_members).
constraint_beneficiary(union_protection_underperformance, labor_stability).
constraint_victim(union_protection_underperformance, organizational_efficiency).
constraint_victim(union_protection_underperformance, high_performing_coworkers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: FRONT-LINE SUPERVISOR - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Despite their title, they lack the agency to 
         dismiss an employee without months of documented failure.
   WHEN: immediate - Dealing with the daily output gap.
   WHERE: trapped - Bound by the specific language of the CBA.
   SCOPE: local - Their specific team or shift.
   
   WHY THIS CLASSIFICATION:
   To the supervisor, the protection is a Mountain. They cannot move it or 
   climb over it. The "burden of proof" is so high that the underperformer 
   appears as a permanent feature of the workplace landscape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    union_protection_underperformance,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(union_protection_underperformance),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE UNION STEWARD - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Represents the collective and enforces the rules.
   WHEN: biographical - Protecting the career longevity of all members.
   WHERE: mobile - Can negotiate grievances and settle disputes.
   SCOPE: regional/national - The broader labor movement interests.
   
   WHY THIS CLASSIFICATION:
   The steward views the protection as a Rope—a vital coordination mechanism 
   that prevents management from being arbitrary or retaliatory. It is a 
   functional standard that protects the "weak" to ensure the security 
   of the "strong."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    union_protection_underperformance,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(union_protection_underperformance, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: HIGH-PERFORMING PEER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has some agency (can leave) but is currently bound.
   WHEN: biographical - Their career growth is slowed by the "dead wood."
   WHERE: constrained - High cost of leaving if they have seniority/pensions.
   SCOPE: local - The immediate working environment.
   
   WHY THIS CLASSIFICATION:
   The high-performer sees the Noose. The protection of the underperformer 
   extracts more labor from them (to cover the gap) while the institution 
   chokes off their own advancement or morale.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    union_protection_underperformance,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(union_protection_underperformance, S),
    S > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(union_protection_tests).

test(multi_perspective_conflict) :-
    % Supervisor (Mountain) vs Steward (Rope) vs Peer (Noose)
    constraint_indexing:constraint_classification(union_protection_underperformance, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(union_protection_underperformance, T2, context(institutional, biographical, mobile, national)),
    constraint_indexing:constraint_classification(union_protection_underperformance, T3, context(individual_moderate, biographical, constrained, local)),
    T1 \= T2, T2 \= T3.

test(extraction_peer_burden) :-
    % Test if extraction score reflects the burden on the organization/peers
    domain_priors:base_extractiveness(union_protection_underperformance, E),
    E > 0.5.

:- end_tests(union_protection_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: Labeled primarily as 'Rope' because job protection 
 * is the core functional product of a union.
 * 2. PERSPECTIVE SHIFT: The "Supervisor" is modeled as powerless here, 
 * which is an important inversion. In a union environment, the legal 
 * weight of the CBA often renders the manager a subject to the rules.
 */

omega_variable(
    productivity_threshold_ambiguity,
    "Is 'underperformance' a subjective management label (Noose) or 
     an objective measurable deficit (Mountain)?",
    resolution_mechanism("Comparison of grievance outcomes where quantitative metrics exist vs. qualitative assessments"),
    impact("If Quantitative: The protection is a Rope for fairness. If Qualitative: 
            It is a Mountain of bureaucratic stalemate."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Performance-Based Peer Review
 * Viability: Some professional guilds use peer evaluation to manage 
 * underperformance while maintaining safety.
 * Suppression: Suppressed by union leadership to avoid "pitting member 
 * against member" and weakening collective solidarity.
 * * CONCLUSION:
 * The suppression of performance-based alternatives in favor of rigid 
 * proceduralism shifts the classification from a coordination Rope 
 * toward a Noose for the organization's high-performers.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [union_protection_underperformance].
% 2. Report: ?- multi_index_report(union_protection_underperformance).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
