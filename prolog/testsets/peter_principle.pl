% ============================================================================
% CONSTRAINT STORY: peter_principle
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Laurence J. Peter (1969) / Management Theory
% ============================================================================

:- module(constraint_peter_principle, []).

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
 * * constraint_id: peter_principle
 * human_readable: The Peter Principle
 * domain: organizational/social/economic
 * temporal_scope: Modern Bureaucratic Era (1960s-Present)
 * spatial_scope: Global (Hierarchical Organizations)
 * * SUMMARY:
 * The Peter Principle states that "in a hierarchy, every employee tends to rise 
 * to their level of incompetence." It posits that people are promoted based 
 * on their performance in their current role rather than their fitness for 
 * the next one, eventually reaching a position where they can no longer 
 * perform effectively and thus remain there.
 * * KEY AGENTS:
 * - The Incompetent Manager: The agent who has reached their "level of 
 * incompetence" and now focuses on preserving status and following rules.
 * - The Productive Subordinate: The "competent" worker doing the actual work 
 * who is yet to be promoted to their own level of failure.
 * - The Organizational Auditor: An observer attempting to optimize placement 
 * against the natural "rising" force of the hierarchy.
 * * NARRATIVE ARC:
 * The principle acts as a "Mountain" of systemic entropy—a natural outcome 
 * of the promotion-as-reward mechanism. For the individual rising, it is a 
 * "Rope" (the ladder of success) until the final promotion, where it 
 * becomes a "Noose," trapping them in a role they hate and cannot perform.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(peter_interval, 0, 10).
narrative_ontology:constraint_claim(peter_principle, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: It extracts organizational efficiency and employee well-being. 
% It places the "cost" of incompetence on the entire system while the 
% promoted individual extracts a higher salary for lower utility.
domain_priors:base_extractiveness(peter_principle, 0.4).

% Suppression score (0.0-1.0)
% Rationale: Alternatives like "lateral growth" or "demotion without shame" 
% are heavily suppressed by cultural norms surrounding "success" and 
% "upward mobility."
domain_priors:suppression_score(peter_principle, 0.5).

% Enforcement requirements
domain_priors:emerges_naturally(peter_principle).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(peter_principle, extractiveness, 0.4).
narrative_ontology:constraint_metric(peter_principle, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(peter_principle, senior_bureaucracy).
constraint_victim(peter_principle, organizational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SOCIAL SCIENTIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer of structural patterns)
   WHEN: civilizational (A permanent feature of human hierarchies)
   WHERE: trapped (As long as hierarchies exist, this pattern persists)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the scientist, this is a Mountain. It is an emergent property of any 
   system that rewards past performance with a different future role. It 
   is a statistical certainty that over time, the "sink" of every position 
   is filled by someone who cannot do the job.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    peter_principle,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE AMBITIOUS PROFESSIONAL - ROPE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Seeking advancement)
   WHEN: biographical (Advancing through a 40-year career)
   WHERE: mobile (Can switch organizations to reset the 'Peter' cycle)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the climber, the promotion logic is a Rope. It is the visible path to 
   higher status and pay. They use their current competence to pull themselves 
   up the hierarchy, treating the principle not as a warning, but as the 
   standard mechanism of coordination and personal growth.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    peter_principle,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE TRAPPED MANAGER - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Despite the title, they are stuck in a role)
   WHEN: immediate (Daily struggle with a job they can't do)
   WHERE: constrained (Cannot demote themselves without social/financial ruin)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the person who has reached their level of incompetence, the principle 
   is a Noose. They are overwhelmed, stressed, and ineffective. They cannot 
   go back to the role they were good at because of the "promotion trap," 
   and they cannot move forward. The system strangles their job satisfaction 
   and mental health.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    peter_principle,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================= */

:- begin_tests(peter_principle_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(peter_principle, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(peter_principle, T2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(peter_principle, T3, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(extraction_paradox) :-
    % Higher power (climber) sees a Rope; lower power (trapped/ineffective) sees a Noose.
    domain_priors:base_extractiveness(peter_principle, Score),
    Score > 0.3.

test(time_immutability) :-
    % Over civilizational time, it's an invariant Mountain.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(peter_principle_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. BASE EXTRACTIVENESS (0.4): The principle extracts utility from the 
 * organization and well-being from the individual. It's a "silent tax" 
 * on hierarchical productivity.
 * 2. NOOSE CLASSIFICATION: Essential to recognize that the person at the top 
 * is often a victim of their own "success"—trapped by salary and status 
 * in a role where they feel like a fraud (Imposter Syndrome as a signal 
 * of the Peter Principle).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_lag,
    "How long can an incompetent manager hide behind 'Final Placement' before the organization detects the lack of utility?",
    resolution_mechanism("Analysis of organizational lifespan vs. average management tenure"),
    impact("If lag is high: The Peter Principle is a permanent Mountain. If low: It's a temporary Scaffold."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Dual-Track Career Paths (Individual Contributor vs Manager)
 * Viability: Allows people to "promote" in pay/status without changing roles.
 * Suppression: Moderate. Still secondary to the "leadership" narrative in 
 * most cultures.
 * * ALTERNATIVE 2: Up-or-Out (Cravath System)
 * Viability: Forces exit rather than stagnation at incompetence.
 * Suppression: High. Viewed as "brutal" and socially expensive.
 * * CONCLUSION:
 * The existence of Alternatives shifts the Peter Principle from an 
 * "Inevitable Mountain" to a "Voluntary Noose" for organizations that refuse 
 * to decouple status from management.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_peter_principle].
 * 2. Multi-perspective: ?- multi_index_report(peter_principle).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
