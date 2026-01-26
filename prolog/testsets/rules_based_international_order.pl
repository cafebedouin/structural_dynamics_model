% ============================================================================
% CONSTRAINT STORY: rules_based_international_order
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Mark Carney Davos Speech (January 20, 2026)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_rules_based_international_order, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
 * 
 * constraint_id: rules_based_international_order
 * human_readable: The Rules-Based International Order (The "Greengrocer's Ritual")
 * domain: political/economic/diplomatic
 * temporal_scope: 1945-2026 (transitioning from "Useful Fiction" to "Rupture")
 * spatial_scope: Global
 * 
 * SUMMARY:
 * The international system of institutions (WTO, UN, NATO) and norms that 
 * previously provided predictability for middle powers. It is currently 
 * undergoing a "rupture" where the pretense of mutual benefit is being 
 * abandoned by great powers in favor of transactional coercion and subordination.
 * 
 * KEY AGENTS:
 * - The Greengrocer (Individual Powerless): Any state or entity participating in the "ritual" of compliance to avoid trouble.
 * - Middle Powers (Individual Moderate): Historically beneficiaries of predictability; now facing subordination.
 * - Great Powers (Institutional): The "strong" who do what they can; treat rules as optional instruments.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(rules_based_international_order, 0, 10).
narrative_ontology:constraint_claim(rules_based_international_order, snare).

% Base extractiveness: 0.75 (High)
% Carney notes that integration is now a "source of subordination" 
% and great powers use supply chains and tariffs as weapons of coercion.
domain_priors:base_extractiveness(rules_based_international_order, 0.75).

% Suppression: 0.6 (Moderate-High)
% The "lie" was maintained through rituals of compliance; 
% alternatives (strategic autonomy) were suppressed by the "comfort" of 
% alliance assumptions.
domain_priors:suppression_score(rules_based_international_order, 0.6).

% Enforcement: Requires active maintenance (Rituals and Hegemonic power).
domain_priors:requires_active_enforcement(rules_based_international_order).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(rules_based_international_order, great_powers).
constraint_victim(rules_based_international_order, middle_powers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE GREENGROCER (PASSIVE OBSERVER) - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless (Subject to rules, cannot shape them)
   WHEN: biographical (A single lifetime of "getting along")
   WHERE: trapped (Performance is seen as the only way to "avoid trouble")
   
   WHY THIS CLASSIFICATION:
   The system appears as an immutable natural law. Compliance is a ritual 
   performed because "everyone does the same," making the order seem inevitable.
   The powerless individual is simply a subject of this unchangeable force.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rules_based_international_order,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MIDDLE POWER (CANADA) - Snare
   --------------------------------------------------------------------------
   WHO: individual_moderate (Significant agency but limited rule-shaping)
   WHEN: biographical (Current policy horizon of the Carney administration)
   WHERE: constrained (Strategic autonomy is a "pricey" insurance policy)
   
   WHY THIS CLASSIFICATION:
   For Canada, the order has shifted from a protective framework to a mechanism 
   of subordination. The costs of integration are now risks of coercion.
   "You cannot 'live within the lie' of mutual benefit through integration 
   when integration becomes the source of your subordination."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rules_based_international_order,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: GREAT POWER (HEGEMON) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-making power)
   WHEN: immediate (Transactional/Political cycle)
   WHERE: arbitrage (Can play systems against each other or go it alone)
   
   WHY THIS CLASSIFICATION:
   The rules are a functional coordination mechanism used to "provide public 
   goods" when useful, but discarded when they no longer serve "unhindered 
   pursuit of power." The rules are a 'Rope' to be used strategically.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rules_based_international_order,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rules_based_international_order_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(rules_based_international_order, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(rules_based_international_order, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(rules_based_international_order, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(rules_based_international_order_tests).

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
 * 1. MANDATROPHY STATUS: High extractiveness (0.75) is 'RESOLVED' because
 *    the Great Powers (institutional) view the order as a flexible 'Rope',
 *    while middle powers (individual moderate) experience it as a 'Snare'.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Greengrocer (Mountain): Perceived as an immutable law.
 *    - Middle Power (Snare): Subordinated and coerced.
 *    - Great Power (Rope): A strategic tool for power.
 * 
 * 3. CORE INSIGHT: The "rules-based international order" is a construct whose
 *    nature changes dramatically depending on an actor's power and ability
 *    to exploit or be exploited by the rules.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    rules_based_international_order_extraction_intent,
    "Is the 0.75 extraction a functional necessity of the 'natural logic' of international relations or a predatory choice by hegemons to maintain asymmetric power?",
    resolution_mechanism("Monitor if hegemons provide any remaining public goods vs. purely extractive transactionalism in their foreign policy. Analyze UN voting patterns and trade agreements."),
    impact("If necessity: 'Mountain'. If predatory choice: 'Snare'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: "Strategic Autonomy / Values-Based Realism"
 *    Viability: Actively pursued by Canada; involves diversifying trade
 *    (12 deals in 6 months) and doubling defense spending.
 *    Suppression: Previously suppressed by the "comfortable assumptions"
 *    of alliance membership and geography.
 *
 * CONCLUSION:
 * The current "rupture" of the rules-based order reveals that what was once
 * perceived as a stable 'Mountain' was in fact a 'Snare' for middle powers.
 * The emergence of "Strategic Autonomy" as a viable alternative changes
 * the nature of the constraint.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/rules_based_international_order].
 * 2. Multi-perspective: ?- multi_index_report(rules_based_international_order).
 * 3. Run tests: ?- run_tests(rules_based_international_order_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */