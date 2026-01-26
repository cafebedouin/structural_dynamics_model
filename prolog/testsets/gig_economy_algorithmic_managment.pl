% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: gig_economy_algorithmic_management
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: General Economic Analysis of Platform Labor / Gig Economy
% ============================================================================

:- module(constraint_gig_economy, []).

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
 * * constraint_id: gig_economy_algorithmic_management
 * human_readable: Algorithmic Management and Information Asymmetry
 * domain: economic/technological
 * temporal_scope: 2010-Present
 * spatial_scope: Global (Urban Platform Markets)
 * * SUMMARY:
 * The "Gig Economy" refers to a labor market characterized by the prevalence 
 * of short-term contracts or freelance work as opposed to permanent jobs. 
 * The core constraint is "algorithmic management," where platforms use 
 * opaque data-driven systems to assign tasks, set prices, and evaluate 
 * performance, creating a significant power imbalance.
 * * KEY AGENTS:
 * - The Driver/Courier: Individual powerless; subject to algorithmic 
 * dispatch and deactivation without traditional due process.
 * - The Platform (Uber/DoorDash): Institutional; holds the rule-making 
 * power and owns the data/market infrastructure.
 * - The Consumer: Individual moderate; benefits from low prices but is 
 * complicit in the extractive feedback loop.
 * * NARRATIVE ARC:
 * The gig economy is marketed as a Rope (flexibility and "being your own 
 * boss"). However, for workers who rely on it as their primary income, the 
 * platform's opaque terms and dynamic pricing function as a Snare. The 
 * inability to see "behind the curtain" of the algorithm makes the 
 * system appear to workers as an unyielding Mountain of market physics.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(platform_labor_epoch, 0, 10).
narrative_ontology:constraint_claim(gig_economy_algorithmic_management, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.8. High extraction; platforms capture significant surplus value 
% while shifting all operational risks (vehicle maintenance, insurance, 
% downtime) onto the individual worker.
domain_priors:base_extractiveness(gig_economy_algorithmic_management, 0.8).

% Suppression score (0.0-1.0)
% Rationale: 0.7. High suppression; the algorithm is a "black box." Workers 
% are often unaware of how their pay is calculated or why they are 
% prioritized for certain tasks, suppressing their ability to negotiate.
domain_priors:suppression_score(gig_economy_algorithmic_management, 0.7).

% Enforcement requirements
% Requires active enforcement (automated deactivation, surge pricing 
% triggers, and behavior-nudging notifications).
domain_priors:requires_active_enforcement(gig_economy_algorithmic_management).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(gig_economy_algorithmic_management, extractiveness, 0.8).
narrative_ontology:constraint_metric(gig_economy_algorithmic_management, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(gig_economy_algorithmic_management, platform_shareholders).
constraint_beneficiary(gig_economy_algorithmic_management, time_constrained_consumers).
constraint_victim(gig_economy_algorithmic_management, full_time_gig_workers).
constraint_victim(gig_economy_algorithmic_management, traditional_service_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE FULL-TIME DRIVER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Lacks bargaining power; income is 100% 
         dependent on the platform's arbitrary algorithmic shifts.
   WHEN: immediate - Focused on hitting the daily "earnings goal" to pay rent.
   WHERE: trapped - While "free" to log off, the lack of viable alternatives 
         for their specific skill/capital set creates a de facto lock-in.
   SCOPE: local - Navigating the specific streets of their city.
   
   WHY THIS CLASSIFICATION:
   For the full-time worker, the system is a Snare. The platform extracts 
   maximum effort through "gamification" and "nudges," while the payout 
   ratios often tighten over time as the platform seeks profitability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gig_economy_algorithmic_management,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(gig_economy_algorithmic_management, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE "SIDE-HUSTLE" STUDENT - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has other primary income/support; the gig 
         work is supplemental.
   WHEN: immediate - Short-term cash for specific expenses.
   WHERE: mobile - High exit options; can quit at any time without crisis.
   SCOPE: local - Neighborhood-level work.
   
   WHY THIS CLASSIFICATION:
   For the casual user, the gig economy is a Rope—a functional coordination 
   mechanism that allows them to trade spare time for cash on their own 
   terms. The "flexibility" marketed by the platform is a genuine utility 
   for them because they aren't dependent on the "Snare" of the platform's 
   total extractiveness.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gig_economy_algorithmic_management,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(gig_economy_algorithmic_management, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PLATFORM ARCHITECT - Institutional
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power; they design the incentive 
         structures and pricing tiers.
   WHEN: biographical - Focused on company growth and market dominance.
   WHERE: arbitrage - Plays different labor markets against each other.
   SCOPE: global - Scaling the platform across multiple countries.
   
   WHY THIS CLASSIFICATION:
   The architect sees a Rope. They view the algorithm as a highly 
   sophisticated coordination mechanism that solves the "efficiency 
   problem" of urban logistics. They don't see extraction; they see 
   "market optimization" and "friction reduction."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gig_economy_algorithmic_management,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SYSTEMS ANALYST) - Mountain
   --------------------------------------------------------------------------
   WHO: agent_power(analytical) - Observes the unyielding mathematical logic.
   WHEN: immediate - Algorithmic decisions are made in millisecond cycles.
   WHERE: arbitrage - Managing the data flow across global digital networks.
   SCOPE: global - The logic of optimization is a universal digital invariant.
   
   WHY THIS CLASSIFICATION:
   The "Mountain" is the objective function of the algorithm. It is the 
   unchangeable physical law of the platform's digital terrain. While 
   workers feel it as a Snare and managers use it as a Rope, the analyst 
   recognizes it as an immutable structural fact: the system must 
   optimize for efficiency or face total thermodynamic collapse. It is 
   not a "choice" but a mathematical necessity of the business model.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gig_economy_algorithmic_managment,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(gig_economy_algorithmic_managment),
    !.

% Explicit priors reflecting the rigid, unyielding nature of algorithmic logic.
domain_priors:base_extractiveness(gig_economy_algorithmic_managment, 0.4).
domain_priors:suppression_score(gig_economy_algorithmic_managment, 0.3).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(gig_economy_tests).

test(multi_perspective_dependency_gap) :-
    % Driver (Snare) vs Student (Rope)
    constraint_indexing:constraint_classification(gig_economy_algorithmic_management, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(gig_economy_algorithmic_management, T2, context(individual_moderate, immediate, mobile, local)),
    T1 \= T2.

test(high_extraction_signature) :-
    domain_priors:base_extractiveness(gig_economy_algorithmic_management, E),
    E > 0.7.

:- end_tests(gig_economy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE DEPENDENCY: The "Gig Economy" is the perfect case study for 
 * how the *exact same* set of rules (the algorithm) appears as a Rope to the 
 * person with high mobility (the student) and a Snare to the person with 
 * low mobility (the full-time driver).
 * 2. EXTRACTIVENESS: Set at 0.8 because of the systemic transfer of 
 * capital risk (depreciation of private assets) to the worker.
 */

omega_variable(
    algorithmic_transparency_impact,
    "Would making the algorithm 100% transparent resolve the Snare, or 
     is the extractiveness inherent in the payout ratios themselves?",
    resolution_mechanism("Comparative analysis of 'transparent' coop platforms vs. 'black-box' corporate platforms"),
    impact("If pay stabilizes: It was a suppression Snare. If pay stays low: 
            It is a pure economic Mountain of low-skill labor supply."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Platform Cooperatives
 * Viability: High in theory (e.g., Drivers Coop in NYC). Workers own the 
 * platform and keep the surplus.
 * Suppression: Suppressed by the lack of venture capital funding for 
 * non-extractive models and the massive marketing budgets of incumbents.
 * * CONCLUSION:
 * The active suppression of cooperative alternatives through market 
 * dominance and predatory pricing confirms the Snare classification for 
 * full-time participants.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [gig_economy_algorithmic_management].
% 2. Analyze: ?- multi_index_report(gig_economy_algorithmic_management).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, snare, agent_power(individual_powerless)).
