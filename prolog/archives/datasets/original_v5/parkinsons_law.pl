% ============================================================================
% CONSTRAINT STORY: parkinsons_law
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: C. Northcote Parkinson (1955) / Management Science
% ============================================================================

:- module(constraint_parkinsons_law, []).

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
 * * constraint_id: parkinsons_law
 * human_readable: Parkinson's Law
 * domain: organizational/economic/social
 * temporal_scope: Modern Bureaucratic Era (1950s-Present)
 * spatial_scope: Global (Public and Private Bureaucracies)
 * * SUMMARY:
 * Parkinson's Law states that "work expands so as to fill the time available for 
 * its completion." It describes the inherent tendency of bureaucracies to 
 * generate work for themselves and to expand their personnel regardless of the 
 * actual amount of productive work to be done.
 * * KEY AGENTS:
 * - The Bureaucrat: Motivated to increase subordinates rather than rivals; 
 * makes work for other bureaucrats.
 * - The Efficiency Auditor: Seeks to identify and prune "manufactured" work 
 * to restore focus on core missions.
 * - The Taxpayer/Shareholder: The ultimate source of funding for the 
 * expanding administrative overhead.
 * * NARRATIVE ARC:
 * The law functions as an invisible gravity in any formal structure. To the 
 * manager, it is a "Rope" (the logic of resource acquisition). To the 
 * systemic observer, it is an immutable "Mountain" of social entropy. For the 
 * productive worker, it becomes a "Snare," as their time is strangled by 
 * meetings and administrative requirements that exist only to serve the hierarchy.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(parkinson_interval, 0, 10).
narrative_ontology:constraint_claim(parkinsons_law, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: It extracts "time" and "capital" from productive ends to maintain 
% the ego and scale of the administrative class.
domain_priors:base_extractiveness(parkinsons_law, 0.5).

% Suppression score (0.0-1.0)
% Rationale: Alternatives (lean management, radical simplification) are 
% often suppressed by the internal logic of the bureaucracy itself, which 
% views "simplification" as a threat to its budget and status.
domain_priors:suppression_score(parkinsons_law, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(parkinsons_law, extractiveness, 0.5).
narrative_ontology:constraint_metric(parkinsons_law, suppression_requirement, 0.4).

% Enforcement requirements
domain_priors:emerges_naturally(parkinsons_law).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(parkinsons_law, middle_management).
constraint_victim(parkinsons_law, productive_contributors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SYSTEMS THEORIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer of systemic entropy)
   WHEN: civilizational (A permanent feature of human social complexity)
   WHERE: trapped (Organization seems inherently to trend toward bloat)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the theorist, this is a Mountain. It is a mathematical certainty of 
   organizational physics. Without constant, violent external intervention, 
   all bureaucracies will expand their administrative ratio regardless of 
   output.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    parkinsons_law,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE DEPARTMENT HEAD - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (In charge of a sub-unit)
   WHEN: biographical (Advancing a career within the system)
   WHERE: arbitrage (Can justify budget increases using the work-expansion logic)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the manager, the law is a Rope. It is a coordination mechanism for 
   securing more resources. By ensuring their team is always "fully booked" 
   and busy, they can argue for more staff and bigger budgets, effectively 
   using the law to pull their department into higher importance.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    parkinsons_law,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CREATIVE PRODUCER - SNARE
   --------------------------------------------------------------------------
   
   WHO: powerless (A worker at the bottom of the hierarchy)
   WHEN: immediate (Today's task list)
   WHERE: constrained (Cannot easily leave or change the workflow)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the person trying to actually "produce," the law is a Snare. They find 
   their day filled with "busywork," unnecessary emails, and meetings about 
   meetings. The more time they are given, the more administrative "crud" 
   the system forces upon them, strangling their ability to do meaningful work.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    parkinsons_law,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(parkinsons_law_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(parkinsons_law, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(parkinsons_law, T2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(parkinsons_law, T3, context(agent_power(powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(power_extractiveness_scaling) :-
    % Institutional actors use the law to extract budget (low/negative cost);
    % Powerless workers suffer the loss of their time (high cost).
    domain_priors:base_extractiveness(parkinsons_law, Score),
    Score > 0.3.

test(time_immutability) :-
    % Scale of civilizational observation confirms the "law" status (Mountain).
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(parkinsons_law_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. BASE EXTRACTIVENESS (0.5):
 * The law extracts the most valuable human resource—time—for the sake 
 * of institutional mass. 
 * 2. PERSPECTIVE SELECTION:
 * The "Rope" perspective for the Manager is crucial; they aren't victims 
 * of the law, they are its primary users to secure prestige and staff.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_impact_threshold,
    "Will AI and automation break the personnel expansion loop (Rope) or simply create more complex administrative 'oversight' work (Snare)?",
    resolution_mechanism("Longitudinal tracking of administrative headcounts post-AI implementation"),
    impact("If loop breaks: Law is a Scaffold. If loop continues: Law is a permanent Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Result-Oriented Work Environments (ROWE)
 * Viability: Focuses on output rather than hours; breaks the time-expansion loop.
 * Suppression: High. Bureaucracies reject this because it makes middle-management 
 * surplus visible.
 * * CONCLUSION:
 * The fact that ROWE-style alternatives are systematically rejected in large 
 * hierarchies indicates that Parkinson's Law is often a "Snare" masquerading 
 * as a "Mountain."
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_parkinsons_law].
 * 2. Multi-perspective: ?- multi_index_report(parkinsons_law).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
