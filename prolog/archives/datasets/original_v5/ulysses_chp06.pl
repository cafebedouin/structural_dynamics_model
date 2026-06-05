% ============================================================================
% CONSTRAINT STORY: bloom_social_exclusion_dublin
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Ulysses, Chapter 6 (Hades)
% ============================================================================

:- module(constraint_bloom_social_exclusion, []).

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
 * * constraint_id: bloom_social_exclusion_dublin
 * human_readable: Social Exclusion and Antisemitic Friction
 * domain: social/political
 * temporal_scope: June 16, 1904
 * spatial_scope: Dublin (Funeral carriage to Prospect Cemetery)
 * * SUMMARY:
 * Leopold Bloom exists under a persistent constraint of social exclusion in Dublin's 
 * Catholic middle-class circles, manifesting as subtle snubs and overt 
 * marginalization. The constraint extracts Bloom's dignity 
 * to maintain tribal cohesion.
 * * KEY AGENTS:
 * - Leopold Bloom: Individual moderate navigating a "Rope" of socialAssent.
 * - The Pauper/Deceased Child: Individual powerless subject to the finality of the social order (Snare).
 * - Martin Cunningham: Institutional moderator viewing the order as law (Mountain).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(bloom_exclusion_interval, 0, 10).
narrative_ontology:constraint_claim(bloom_social_exclusion_dublin, snare).

% Metrics: Extractiveness (0.5) and Suppression (0.7)
domain_priors:base_extractiveness(bloom_social_exclusion_dublin, 0.5).
domain_priors:suppression_score(bloom_social_exclusion_dublin, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(bloom_social_exclusion_dublin, extractiveness, 0.5).
narrative_ontology:constraint_metric(bloom_social_exclusion_dublin, suppression_requirement, 0.7).
domain_priors:emerges_naturally(bloom_social_exclusion_dublin).

% Explicit metric hooks to prevent auto-imputation
% BENEFICIARIES & VICTIMS
constraint_beneficiary(bloom_social_exclusion_dublin, dublin_catholic_inner_circle).
constraint_victim(bloom_social_exclusion_dublin, bloom_leopold).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: LEOPOLD BLOOM - Rope
   --------------------------------------------------------------------------
   WHY: Bloom treats social friction as a "Rope"—a tool to be managed through 
   "soft politeness" and "prudent assent" to maintain his position. 
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_social_exclusion_dublin,
    rope,
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PAUPER / CHILD FUNERAL - Snare
   --------------------------------------------------------------------------
   WHO: powerless - "Only a pauper. Nobody owns.".
   WHY: From the bottom of the social hierarchy, the order is a "Snare" that 
   extracts life and dignity without recourse or coordination options.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_social_exclusion_dublin,
    snare,
    context(agent_power(powerless), time_horizon(immediate), exit_options(constrained), spatial_scope(local))
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: MARTIN CUNNINGHAM - Mountain
   --------------------------------------------------------------------------
   WHO: institutional - The social arbiter who "lays down the law".
   WHY: Cunningham views the tribal social order as an immutable natural law—a 
   Mountain that Bloom cannot change through mere interaction.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    bloom_social_exclusion_dublin,
    mountain,
    context(agent_power(institutional), time_horizon(historical), exit_options(trapped), spatial_scope(national))
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(bloom_social_exclusion_dublin_tests).

test(multi_perspective_conflict) :-
    constraint_indexing:constraint_classification(bloom_social_exclusion_dublin, T1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(bloom_social_exclusion_dublin, T2, context(agent_power(powerless), _, _, _)),
    T1 = rope, T2 = snare.

test(time_immutability_scaling) :-
    % Historical view (Institutional) sees the social order as a Mountain
    constraint_indexing:constraint_classification(bloom_social_exclusion_dublin, mountain, context(agent_power(institutional), time_horizon(historical), _, _)).

:- end_tests(bloom_social_exclusion_dublin_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION & OMEGAS
   ========================================================================== */

/**
 * LLM GENERATION NOTES:
 * Refactored to include the mandatory powerless perspective. This allows the 
 * audit to detect the gap between the "Mountain" of history and the 
 * "Snare" of current social reality.
 */

omega_variable(
    bloom_conscious_assimilation,
    "Is Bloom's assent a survival strategy (Rope) or a deep-seated belief (Mountain)?",
    resolution_mechanism("Internal monologue analysis of Chapter 17 (Ithaca)"),
    impact("If strategy: Bloom is an arbitrageur. If belief: He is a victim of suppression."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Active Confrontation: Rejected to avoid "mortification".
 * 2. Withdrawal: Rejected due to professional and social necessity.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(ulysses_chp06, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(ulysses_chp06, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(ulysses_chp06, snare, agent_power(powerless)).
