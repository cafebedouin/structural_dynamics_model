% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: legacy_system_technical_debt
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Domain Analysis of Software Engineering Constraints
% ============================================================================

:- module(constraint_legacy_technical_debt, []).

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
 * * constraint_id: legacy_system_technical_debt
 * human_readable: Cumulative Technical Debt in Legacy Monoliths
 * domain: technological/economic
 * temporal_scope: Ongoing (Modern Computing)
 * spatial_scope: Global (Enterprise Infrastructure)
 * * SUMMARY:
 * Technical debt represents the implied cost of future refactoring caused by 
 * choosing an easy, limited solution now instead of a better approach that 
 * would take longer. In legacy systems, this debt ossifies into a constraint 
 * that limits the speed of innovation and increases maintenance costs 
 * exponentially.
 * * KEY AGENTS:
 * - Junior Developer: Powerless subject navigating a "spaghetti" codebase.
 * - CTO/VPE: Institutional leader balancing feature velocity vs. stability.
 * - System Architect: Analytical observer identifying structural rot.
 * * NARRATIVE ARC:
 * What begins as a strategic shortcut (Rope) to hit a deadline eventually 
 * becomes an unmovable reality (Mountain) for new hires, and finally an 
 * extractive Noose that consumes 80% of the engineering budget in maintenance 
 * while suppressing the possibility of migration.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(software_lifecycle_r7, 0, 10).
narrative_ontology:constraint_claim(legacy_system_technical_debt, noose).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.8). Technical debt "extracts" labor and capital from 
% productive new work to fuel "keeping the lights on" (maintenance).
domain_priors:base_extractiveness(legacy_system_technical_debt, 0.8).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.6). Alternatives (modernization) are visible but 
% suppressed by the high cost of exit and fear of regression.
domain_priors:suppression_score(legacy_system_technical_debt, 0.6).

% Enforcement requirements
% Emerges naturally from the entropy of large codebases and market pressures.
domain_priors:emerges_naturally(legacy_system_technical_debt).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(legacy_system_technical_debt, extractiveness, 0.8).
narrative_ontology:constraint_metric(legacy_system_technical_debt, suppression_requirement, 0.6).

% Beneficiaries and Victims
constraint_beneficiary(legacy_system_technical_debt, short_term_profit_margins).
constraint_victim(legacy_system_technical_debt, long_term_system_viability).
constraint_victim(legacy_system_technical_debt, engineering_morale).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: JUNIOR DEVELOPER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - No authority to refactor; must follow existing patterns.
   WHEN: immediate - Daily task completion and bug fixing.
   WHERE: trapped - Bound by the local logic of a 20-year-old module.
   SCOPE: local - Viewing only the code directly in front of them.
   
   WHY THIS CLASSIFICATION:
   For a new developer, the legacy codebase is "nature." They did not build 
   the wall; they simply live in its shadow. The constraint is unchangeable 
   and must be navigated like a physical mountain.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    legacy_system_technical_debt,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:emerges_naturally(legacy_system_technical_debt),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: CTO / EXECUTIVE - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power over budget and architecture.
   WHEN: biographical - Planning the 5-year roadmap for the company.
   WHERE: constrained - High cost to switch (migration), but technically possible.
   SCOPE: national/global - Managing enterprise-wide systems.
   
   WHY THIS CLASSIFICATION:
   For leadership, technical debt is a Rope—a functional (if painful) coordination 
   mechanism. It is the "price of business" that allowed them to reach their 
   current scale. They view it as a tool that can be "paid down" or managed.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    legacy_system_technical_debt,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(legacy_system_technical_debt, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: EXTERNAL AUDITOR / MODERNIZER - Noose
   --------------------------------------------------------------------------
   
   WHO: analytical - Outside observer identifying the "death spiral."
   WHEN: historical - Looking at the long-term decline of the codebase.
   WHERE: analytical - Not dependent on the legacy system's survival.
   SCOPE: global - Comparing the system against industry best practices.
   
   WHY THIS CLASSIFICATION:
   The auditor sees the Noose. Every dollar spent on maintenance is a dollar 
   stolen from the future. The system's complexity has reached a point where 
   it actively chokes off the organization's ability to respond to market shifts.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    legacy_system_technical_debt,
    noose,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(legacy_system_technical_debt, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(legacy_system_technical_debt_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(legacy_system_technical_debt, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(legacy_system_technical_debt, T2, context(institutional, biographical, constrained, national)),
    constraint_indexing:constraint_classification(legacy_system_technical_debt, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(extraction_increases_with_powerlessness) :-
    constraint_indexing:extractiveness_for_agent(legacy_system_technical_debt, context(individual_powerless, immediate, trapped, local), S1),
    constraint_indexing:extractiveness_for_agent(legacy_system_technical_debt, context(institutional, biographical, constrained, national), S2),
    S1 > S2.

:- end_tests(legacy_system_technical_debt_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.8): I chose a high score because tech debt is 
 * essentially a tax on labor. Developers "pay" it through longer hours and 
 * increased stress for the same output.
 * * 2. PERSPECTIVE SELECTION: The Junior Dev's "Mountain" view is critical. 
 * Most architectural failures occur because those with the power to fix 
 * them (CTOs) don't realize that for the workers, the constraint has 
 * already crossed the threshold into immutability.
 * * 3. OMEGA: The primary uncertainty is the "Strangler Pattern" viability.
 */

omega_variable(
    strangler_success_probability,
    "Can the system be migrated incrementally (Rope) or is the monolith's 
     coupling so deep it requires a total rewrite (Noose)?",
    resolution_mechanism("Static analysis of dependency graphs and coupling metrics"),
    impact("If Rope: Migration is viable. If Noose: The organization will likely 
            fail when the legacy system reaches terminal entropy."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Microservices/Cloud-Native Migration
 * Viability: High in theory, but requires a "pause" in feature development.
 * Suppression: Suppressed by quarterly earning pressures and short-term 
 * incentive structures for management.
 * * CONCLUSION:
 * The presence of a viable alternative (Modernization) that is actively 
 * suppressed by management incentives solidifies the 'Noose' classification 
 * for the workforce.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [constraint_legacy_technical_debt].
% 2. Report: ?- constraint_indexing:multi_index_report(legacy_system_technical_debt).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
