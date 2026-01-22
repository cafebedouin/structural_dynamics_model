% ============================================================================
% CONSTRAINT STORY: rules_based_international_order
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
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
 * * constraint_id: rules_based_international_order
 * human_readable: The Rules-Based International Order (The "Greengrocer's Ritual")
 * domain: political/economic
 * temporal_scope: 1945-2026 (transitioning from "Useful Fiction" to "Rupture")
 * spatial_scope: Global
 * * SUMMARY:
 * The international system of institutions (WTO, UN, NATO) and norms that 
 * previously provided predictability for middle powers. It is currently 
 * undergoing a "rupture" where the pretense of mutual benefit is being 
 * abandoned by great powers in favor of transactional coercion and subordination.
 * * KEY AGENTS:
 * - Middle Powers (e.g., Canada): Historically beneficiaries of predictability; now facing subordination.
 * - Great Powers (Hegemons): The "strong" who do what they can; treat rules as optional instruments.
 * - The Greengrocer (Metaphorical): Any state or entity participating in the "ritual" of compliance to avoid trouble.
 * * NARRATIVE ARC:
 * The constraint began as a "Rope" (coordination) that was partially false but useful. 
 * As integration became weaponized, it transformed into a "Noose" for middle powers, 
 * necessitating a shift toward "Values-Based Realism" and strategic autonomy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for structural extraction
narrative_ontology:interval(rules_based_international_order, 0, 10).

% Base extractiveness: 0.75 (High)
% Rationale: Carney notes that integration is now a "source of subordination" 
% and great powers use supply chains and tariffs as weapons of coercion.
domain_priors:base_extractiveness(rules_based_international_order, 0.75).

% Suppression: 0.6 (Moderate-High)
% Rationale: The "lie" was maintained through rituals of compliance; 
% alternatives (strategic autonomy) were suppressed by the "comfort" of 
% alliance assumptions.
domain_priors:suppression_score(rules_based_international_order, 0.6).

% Enforcement: Requires active maintenance (Rituals and Hegemonic power)
domain_priors:requires_active_enforcement(rules_based_international_order).

% BENEFICIARIES
% Great powers who can "exempt themselves when convenient" and "monetize relationships."
constraint_beneficiary(rules_based_international_order, great_powers).

% VICTIMS
% Middle powers and "the weak" who must "suffer what they must" under asymmetric enforcement.
constraint_victim(rules_based_international_order, middle_powers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: MIDDLE POWER (CANADA) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Significant agency but limited rule-shaping)
   WHEN: biographical (Current policy horizon of the Carney administration)
   WHERE: constrained (Strategic autonomy is a "pricey" insurance policy)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For Canada, the order has shifted from a protective framework to a mechanism 
   of subordination. The costs of integration are now risks of coercion.
   
   NARRATIVE EVIDENCE:
   "You cannot 'live within the lie' of mutual benefit through integration 
   when integration becomes the source of your subordination."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rules_based_international_order,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(rules_based_international_order, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: GREAT POWER (HEGEMON) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power)
   WHEN: immediate (Transactional/Political cycle)
   WHERE: arbitrage (Can play systems against each other or go it alone)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The rules are a functional coordination mechanism used to "provide public 
   goods" when useful, but discarded when they no longer serve "unhindered 
   pursuit of power."
   
   NARRATIVE EVIDENCE:
   "The strongest would exempt themselves when convenient... great powers 
   can afford, for now, to go it alone."
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
) :-
    % For the institutional power, the constraint is mutable
    true,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PASSIVE OBSERVER (GREENGROCER) - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to rules, cannot shape them)
   WHEN: biographical (A single lifetime of "getting along")
   WHERE: trapped (Performance is seen as the only way to "avoid trouble")
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   The system appears as an immutable natural law. Compliance is a ritual 
   performed because "everyone does the same," making the order seem inevitable.
   
   NARRATIVE EVIDENCE:
   "Presented as inevitable — as the natural logic of international relations 
   reasserting itself... To hope that compliance will buy safety."
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
) :-
    domain_priors:suppression_score(rules_based_international_order, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rules_based_international_order_tests).

test(multi_perspective_variance) :-
    % Canada (Moderate) sees Noose
    constraint_indexing:constraint_classification(rules_based_international_order, noose, context(agent_power(individual_moderate), _, constrained, _)),
    % Hegemon (Institutional) sees Rope
    constraint_indexing:constraint_classification(rules_based_international_order, rope, context(agent_power(institutional), _, arbitrage, _)),
    % Greengrocer (Powerless) sees Mountain
    constraint_indexing:constraint_classification(rules_based_international_order, mountain, context(agent_power(individual_powerless), _, trapped, _)).

test(power_extractiveness_scaling) :-
    % Extraction is experienced as subordination for the moderate/powerless
    % but as an instrument for the institutional.
    domain_priors:base_extractiveness(rules_based_international_order, E),
    E > 0.7.

test(the_rupture_insight) :-
    % Test that "naming the reality" (removing the sign) changes exit options 
    % from trapped to mobile/constrained, shifting Mountain -> Noose.
    constraint_indexing:constraint_classification(rules_based_international_order, mountain, context(_, _, trapped, _)),
    constraint_indexing:constraint_classification(rules_based_international_order, noose, context(_, _, constrained, _)).

:- end_tests(rules_based_international_order_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: Chosen because the source describes the current state as 
 * "source of subordination" where great powers "monetize" allies. This 
 * high score triggers Mandatrophy protocols.
 * * 2. SUPPRESSION SCORE (0.6):
 * Reasoning: Carney identifies a "tendency to go along to get along" and 
 * "living within a lie." The suppression is not just physical force but 
 * cognitive "rituals" that hide the reality of the rupture.
 * * 3. PERSPECTIVE SELECTION:
 * Chose Canada (Middle Power), Great Power (Hegemon), and the "Greengrocer" 
 * to illustrate the transition from an perceived inevitable Mountain 
 * to an extractive Noose, and finally to a tool (Rope) for the powerful.
 * * 4. AMBIGUITIES:
 * The source is clear on the "Rupture" but ambiguous on whether "Strategic 
 * Autonomy" is truly an exit or just a different type of constraint. 
 * This is handled via Omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    rules_based_international_order_extraction_intent,
    "Is the 0.75 extraction a functional necessity of the 'natural logic' of IR or a predatory choice by hegemons?",
    resolution_mechanism("Monitor if hegemons provide any remaining public goods vs. purely extractive transactionalism"),
    impact("If necessity: Mountain. If predatory choice: Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    strategic_autonomy_efficacy,
    "Does 'strategic autonomy' provide a genuine exit from great power coercion?",
    resolution_mechanism("Analyze if middle power 'variable geometry' coalitions can withstand targeted bilateral pressure from hegemons"),
    impact("If effective: Rope/Scaffold. If ineffective: Noose remains."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: "Strategic Autonomy / Values-Based Realism"
 * Viability: Actively pursued by Canada; involves diversifying trade 
 * (12 deals in 6 months) and doubling defense spending.
 * Suppression: Previously suppressed by the "comfortable assumptions" 
 * of alliance membership and geography.
 * * ALTERNATIVE 2: "Transactional Accommodation"
 * Viability: "Go along to get along" (The Greengrocer's path).
 * Suppression: Rejected by Carney as a failure that "won't buy safety."
 * * CONCLUSION:
 * The existence of suppressed alternatives (Strategic Autonomy) that are now 
 * being "named" shifts the perception from Mountain to Noose, as agents 
 * realize the "trapped" state was partially a "lie" (self-suppression).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/rules_based_international_order].
 * 2. Multi-perspective: ?- multi_index_report(rules_based_international_order).
 * 3. Run tests: ?- run_tests(rules_based_international_order_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
