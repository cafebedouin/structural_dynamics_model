% ============================================================================
% CONSTRAINT STORY: ai_task_horizon_reliability
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Anthropic Economic Index Report: Economic Primitives (Jan 15, 2026)
% ============================================================================

:- module(constraint_ai_task_horizon_reliability, []).

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
 * constraint_id: ai_task_horizon_reliability
 * human_readable: The AI Task Horizon and Reliability Bottleneck
 * domain: technological/economic
 * temporal_scope: November 2025 - January 2026
 * spatial_scope: Global
 * 
 * SUMMARY:
 * This constraint defines the inverse relationship between task complexity 
 * (measured in human time-to-complete) and AI success rates. 
 * As tasks scale in duration and educational requirements, model reliability 
 * drops significantly, creating a "task horizon" that limits autonomous 
 * productivity gains.
 * 
 * KEY AGENTS:
 * - Enterprise API User (Institutional): Businesses deploying models for autonomous workflows.
 * - Professional Knowledge Worker (Individual Moderate): Subject to "net deskilling" as AI automates tasks.
 * - Individual Web User (Individual Powerless): Uses interactive feedback to augment complex tasks.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(ai_task_horizon_reliability, 0, 10).
narrative_ontology:constraint_claim(ai_task_horizon_reliability, tangled_rope).

% Base extractiveness: 0.45.
% AI removes high-skill tasks from 49% of occupations, extracting professional autonomy.
domain_priors:base_extractiveness(ai_task_horizon_reliability, 0.45).

% Suppression score: 0.60.
% Selection effects mask true failure rates 
% as users avoid presenting tasks they expect to fail.
domain_priors:suppression_score(ai_task_horizon_reliability, 0.60).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ai_task_horizon_reliability, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, suppression_requirement, 0.6).

% Enforcement: Emerges naturally from current LLM architectural limits.
domain_priors:emerges_naturally(ai_task_horizon_reliability).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(ai_task_horizon_reliability, efficiency_seeking_firms).
constraint_victim(ai_task_horizon_reliability, white_collar_professionals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: INDIVIDUAL WEB USER - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Uses interactive feedback to augment complex tasks)
   WHEN: immediate (Short-term interactions with the AI model)
   WHERE: trapped (Subject to the inherent limitations of the models they use)
   
   WHY THIS CLASSIFICATION:
   For the individual web user, the AI task horizon is a 'Mountain'. They are
   subject to the inherent limitations of the models they use and have no power
   to change the underlying architecture. They can only work around the 'Mountain'
   through iterative feedback and prompt engineering.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_task_horizon_reliability,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: ENTERPRISE API USER - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Automation-dominant deployments)
   WHEN: biographical (Long-term business strategy)
   WHERE: arbitrage (Balances automation benefits with reliability risks)
   
   WHY THIS CLASSIFICATION:
   For an enterprise user, the task horizon is a 'Tangled Rope'. It's a 'Rope'
   because it enables massive efficiency gains through automation. It's 'Tangled'
   because reliability drops significantly for complex tasks, requiring constant
   monitoring and human intervention, making it a complex trade-off between
   speed and accuracy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_task_horizon_reliability,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PROFESSIONAL KNOWLEDGE WORKER - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate (Subject to "net deskilling" in 49% of roles)
   WHEN: biographical (Adapting career paths to AI integration)
   WHERE: constrained (Forced to adopt AI to meet institutional speed targets)
   
   WHY THIS CLASSIFICATION:
   For the professional, AI is a 'Snare'. It automates high-skill tasks,
   reducing their "core knowledge" value and professional autonomy. While it
   offers speedups, it also extracts their unique expertise, potentially
   strangling their long-term career growth.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ai_task_horizon_reliability,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ai_task_horizon_reliability_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(ai_task_horizon_reliability_tests).

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
 * 1. INDIVIDUAL POWERLESS PERSPECTIVE: Added 'Individual Web User' as the
 *    individual powerless agent. For them, AI reliability is a 'Mountain' of
 *    immutable technological limits.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Individual Web User (Mountain): Immutable technological limits.
 *    - Enterprise User (Tangled Rope): Balancing automation with reliability.
 *    - Professional Worker (Snare): Deskilling and loss of professional autonomy.
 * 
 * 3. CORE INSIGHT: The AI task horizon creates a complex 'Tangled Rope'.
 *    While it offers a 'Rope' of automation for some, it functions as a 'Snare'
 *    for professional knowledge workers and a 'Mountain' of technological
 *    limitations for individual users, highlighting profound trade-offs
 *    between efficiency and human capital.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the future evolution of AI capabilities and their impact on labor.
 */

omega_variable(
    bottleneck_task_evolution,
    "Will 'bottleneck tasks' scale in difficulty as AI improves, keeping the 50% success horizon constant, or will AI eventually overcome these limitations?",
    resolution_mechanism("Longitudinal tracking of user task complexity relative to model generations; breakthroughs in AI architecture and reasoning."),
    impact("If constant: Productivity gains stall at ~1.0pp. If overcome: Gains accelerate toward 5%pp, with profound labor market disruption."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Human-Only Knowledge Work
 *    Viability: Declining rapidly due to market pressure to adopt AI for significant speedups.
 *    Suppression: Actively suppressed by the economic advantages of AI-driven efficiency.
 *
 * CONCLUSION:
 * The AI task horizon reveals a 'Tangled Rope' where the 'Rope' of automation
 * actively suppresses the alternative of human-only work. This creates a 'Snare'
 * for professionals whose skills are devalued, even as the system as a whole
 * becomes more efficient.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/ai_task_horizon_reliability].
 * 2. Multi-perspective: ?- multi_index_report(ai_task_horizon_reliability).
 * 3. Run tests: ?- run_tests(ai_task_horizon_reliability_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */