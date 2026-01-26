% ============================================================================
% CONSTRAINT STORY: migration_decision_threshold
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Hagen-Zanker, "Why do people migrate? A review of the theoretical 
%         literature," Maastricht Graduate School of Governance (2008).
% ============================================================================

:- module(constraint_migration_decision, []).

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
 * * constraint_id: migration_decision_threshold
 * human_readable: The Migration Decision Threshold (Cost-Benefit Equilibrium)
 * domain: economic/social
 * temporal_scope: Modern Era (Post-Industrial Migration)
 * spatial_scope: Global (Trans-national corridors)
 * * SUMMARY:
 * This constraint represents the threshold at which the expected utility of 
 * migrating exceeds the costs and risks of remaining in the origin country 
 *. According to Neo-classical micro theory, migration 
 * is an individual investment in human capital where the actor must 
 * navigate the "wage differential" Mountain.
 * * KEY AGENTS:
 * - The Potential Migrant: Individual powerless; subject to global wage 
 * disparities and legal border regimes.
 * - The Household: Collective organized; in the New Economics of Labor 
 * Migration (NELM), they use migration as a "Rope" to diversify income and 
 * mitigate local market failures.
 * - The Receiving State: Institutional; enforces the legal Mountain of 
 * visa and entry requirements.
 * * NARRATIVE ARC:
 * To the individual, the wage gap is a Mountain—an unchangeable fact of 
 * the world. To the household, migration is a 
 * Rope—a functional coordination tool to overcome the "Snare" of local 
 * market failures in insurance and credit.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(migration_theory_review, 0, 10).
narrative_ontology:constraint_claim(migration_decision_threshold, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.3. Moderate; migration "extracts" labor from the origin 
% (brain drain) but provides remittances and human capital development.
domain_priors:base_extractiveness(migration_decision_threshold, 0.3).

% Suppression score (0.0-1.0)
% Rationale: 0.4. Moderate; local economic alternatives are often 
% suppressed by "market failures" (lack of credit/insurance) in the 
% origin country.
domain_priors:suppression_score(migration_decision_threshold, 0.4).

% Enforcement requirements
% Emerges naturally from global economic inequality and the structural 
% demand for labor in developed nations.
domain_priors:emerges_naturally(migration_decision_threshold).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(migration_decision_threshold, extractiveness, 0.3).
narrative_ontology:constraint_metric(migration_decision_threshold, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(migration_decision_threshold, destination_labor_markets).
constraint_beneficiary(migration_decision_threshold, migrant_households_remittances).
constraint_victim(migration_decision_threshold, origin_country_local_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: INDIVIDUAL MIGRANT - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Must accept the wage gap and legal barriers 
         as they exist.
   WHEN: immediate - Focused on the immediate cost of travel and relocation.
   WHERE: trapped - Bound by the lack of local opportunity in the origin.
   SCOPE: local - A single life-path decision.
   
   WHY THIS CLASSIFICATION:
   For the individual, the global economic landscape is "nature." They 
   cannot "negotiate" with the wage differential between Mexico and the 
   US; they can only choose to climb the Mountain of migration.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    migration_decision_threshold,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:emerges_naturally(migration_decision_threshold),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HOUSEHOLD (NELM) - Rope
   --------------------------------------------------------------------------
   
   WHO: collective_organized - Acting as a unit to manage risk.
   WHEN: biographical - Planning for long-term household stability.
   WHERE: mobile - Sending a member to a different market to diversify.
   SCOPE: regional - Connecting origin and destination regions.
   
   WHY THIS CLASSIFICATION:
   In the New Economics of Labor Migration, the decision is a Rope. It is 
   a functional coordination mechanism to circumvent the failure of 
   local capital markets. Migration is wove into the 
   household strategy as a tool for survival.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    migration_decision_threshold,
    rope,
    context(
        agent_power(collective_organized),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(migration_decision_threshold, E),
    E < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: WORLD SYSTEMS ANALYST - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the macro-structural "core-periphery" 
         dynamic.
   WHEN: historical - Tracking global capital expansion over centuries.
   WHERE: analytical - Not bound by the individual desire to move.
   SCOPE: global - The entire world economy.
   
   WHY THIS CLASSIFICATION:
   For the World Systems theorist, the system is a Snare. Global 
   capitalist expansion "chokes" traditional local economies, 
   creating a "disrupted" population that has no choice but to 
   migrate to the core as cheap labor.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    migration_decision_threshold,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(migration_decision_threshold, S),
    S > 0.3,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(migration_decision_tests).

test(multi_perspective_conflict) :-
    % Individual (Mountain) vs Household (Rope) vs Theorist (Snare)
    constraint_indexing:constraint_classification(migration_decision_threshold, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(migration_decision_threshold, T2, context(collective_organized, biographical, mobile, regional)),
    constraint_indexing:constraint_classification(migration_decision_threshold, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(local_market_failure_impact) :-
    % Extraction/Suppression logic: High suppression of local alternatives 
    % signals a move toward Snare/Mountain.
    domain_priors:suppression_score(migration_decision_threshold, S),
    S >= 0.4.

:- end_tests(migration_decision_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: I chose 'Mountain' as the primary claim for the 
 * individual because the document highlights the "wage gap" as a primary 
 * driver in neo-classical theories.
 * 2. PERSPECTIVE: The NELM perspective is critical here—it transforms 
 * migration from an "escape" to a "coordination" tool (Rope) for the 
 * household.
 * 3. SUPPRESSION (0.4): Reflects the paper's point that local market 
 * failures (insurance/credit) suppress the ability to survive without 
 * migrating.
 */

omega_variable(
    market_failure_reversal,
    "Would the fix of local credit markets (Snare removal) transform 
     migration back into a voluntary choice (Rope)?",
    resolution_mechanism("Evaluation of migration rates in regions with 
    newly introduced micro-credit and insurance programs"),
    impact("If rates drop: The threshold was a Snare. If rates stay high: 
            It is a fundamental wage-gap Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Internal Consumption Smoothing
 * Viability: Diversifying crops or local jobs to manage risk at home.
 * Suppression: Suppressed by systemic lack of local infrastructure and 
 * credit access.
 * * ALTERNATIVE 2: Social Security/State Safety Nets
 * Viability: Government-provided insurance to replace the need for 
 * remittances.
 * * CONCLUSION:
 * The absence of these alternatives in developing nations turns the 
 * Migration Threshold into an unavoidable Mountain for the subject.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [migration_decision_threshold].
% 2. Analysis: ?- multi_index_report(migration_decision_threshold).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(rotmigration_decision_threshold, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(rotmigration_decision_threshold, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(rotmigration_decision_threshold, snare, agent_power(individual_powerless)).
