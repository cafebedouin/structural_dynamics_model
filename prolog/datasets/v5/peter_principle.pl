% ============================================================================
% CONSTRAINT STORY: peter_principle
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: peter_principle
 * human_readable: The Peter Principle (Promotion to Incompetence)
 * domain: organizational/social/economic
 * temporal_scope: Modern Bureaucratic Era (1960s-Present)
 * spatial_scope: Global (Hierarchical Organizations)
 * 
 * SUMMARY:
 * The Peter Principle states that "in a hierarchy, every employee tends to rise 
 * to their level of incompetence." People are promoted based on their performance
 * in their current role rather than their fitness for the next one, eventually
 * reaching a position where they can no longer perform effectively and thus remain there.
 * 
 * KEY AGENTS:
 * - The Trapped Manager (Individual Powerless): Reaches their "level of incompetence" and focuses on preserving status.
 * - The Ambitious Professional (Individual Moderate): Seeks advancement through the hierarchy.
 * - The HR Department (Institutional): Manages employee development and career progression.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(peter_principle, 0, 10).
narrative_ontology:constraint_claim(peter_principle, tangled_rope).

% Base extractiveness: 0.4.
% Extracts organizational efficiency and employee well-being. It places the "cost"
% of incompetence on the entire system while the promoted individual extracts
% a higher salary for lower utility.
domain_priors:base_extractiveness(peter_principle, 0.4).

% Suppression score: 0.5.
% Alternatives like "lateral growth" or "demotion without shame" are heavily
% suppressed by cultural norms surrounding "success" and "upward mobility."
domain_priors:suppression_score(peter_principle, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(peter_principle, extractiveness, 0.4).
narrative_ontology:constraint_metric(peter_principle, suppression_requirement, 0.5).

% Enforcement: Emerges naturally from human cognitive architecture.
domain_priors:emerges_naturally(peter_principle).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(peter_principle, senior_bureaucracy).
constraint_victim(peter_principle, organizational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE TRAPPED MANAGER - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Despite the title, stuck in an unmanageable role)
   WHEN: immediate (Daily struggle with a job they can't do)
   WHERE: constrained (Cannot demote themselves without social/financial ruin)
   
   WHY THIS CLASSIFICATION:
   For the person who has reached their level of incompetence, the principle 
   is a 'Snare'. They are overwhelmed, stressed, and ineffective. They cannot 
   go back to the role they were good at because of the "promotion trap," 
   and they cannot move forward. The system strangles their job satisfaction 
   and mental health.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    peter_principle,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE AMBITIOUS PROFESSIONAL - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Seeking advancement)
   WHEN: biographical (Advancing through a 40-year career)
   WHERE: mobile (Can switch organizations to reset the 'Peter' cycle)
   
   WHY THIS CLASSIFICATION:
   For the ambitious professional, the promotion logic is a 'Rope'. It is the 
   visible path to higher status and pay. They use their current competence 
   to pull themselves up the hierarchy, treating the principle not as a warning, 
   but as the standard mechanism of coordination and personal growth.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE HR DEPARTMENT - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manages employee development and performance)
   WHEN: biographical (Long-term employee growth)
   WHERE: arbitrage (Balances individual perception with objective metrics)
   
   WHY THIS CLASSIFICATION:
   For an HR department, the Peter Principle is a 'Tangled Rope'. It's a 'Rope'
   because understanding it helps them manage employee development and identify
   potential career bottlenecks. It's 'Tangled' because they constantly struggle
   with the organizational inertia that perpetuates promoting individuals into
   roles where they become incompetent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    peter_principle,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(peter_principle_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(peter_principle, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peter_principle, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(peter_principle, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(peter_principle_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'HR Department / Corporate Trainer'
 *    as the institutional agent. This highlights the practical challenges of
 *    managing talent development and performance in an organizational context.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Trapped Manager (Snare): Stuck in a role they can't perform.
 *    - Ambitious Professional (Rope): Uses promotions as a path to growth.
 *    - HR Department (Tangled Rope): Manages the consequences of the principle.
 * 
 * 3. CORE INSIGHT: The Peter Principle is a fundamental organizational dynamic
 *    that highlights the tension between individual career progression and
 *    organizational efficiency, often creating a 'Tangled Rope' for HR.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the practical impact of the Peter Principle on organizational longevity.
 */

omega_variable(
    competence_measurement_lag,
    "How long can an incompetent manager hide behind 'Final Placement' before the organization's overall utility is significantly degraded, leading to systemic failure?",
    resolution_mechanism("Analysis of organizational lifespan vs. average management tenure, correlated with market share or public service efficacy."),
    impact("If lag is high: The Peter Principle is a permanent 'Mountain'. If low: It's a temporary 'Scaffold' that can be easily dismantled."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Dual-Track Career Paths (Individual Contributor vs Manager)
 *    Viability: Allows employees to advance in pay and status without changing roles (e.g., senior engineer track).
 *    Suppression: Often suppressed by corporate cultures that equate "leadership" with "management," making promotion the only path to higher status.
 *
 * CONCLUSION:
 * The Peter Principle is a 'Mountain' that organizations choose to build by
 * suppressing viable alternatives like dual-track career paths. By conflating
 * promotion with management, they create a 'Snare' for competent individuals.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/peter_principle].
 * 2. Multi-perspective: ?- multi_index_report(peter_principle).
 * 3. Run tests: ?- run_tests(peter_principle_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */