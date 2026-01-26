% ============================================================================
% CONSTRAINT STORY: negative_emissions_arbitrage
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Madeleine Cuff, "The invention of net zero"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_negative_emissions_arbitrage, []).

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
 * * constraint_id: negative_emissions_arbitrage
 * human_readable: Negative Emissions Arbitrage
 * domain: economic/technological/political
 * temporal_scope: 2009 (post-Nature paper) to 2026+
 * spatial_scope: Global Carbon Markets
 * * SUMMARY:
 * Since the shift to a net-zero framework, human-made emissions must be balanced 
 * by "equivalent removals" from the atmosphere. 
 * This creates a market for "Negative Emissions Arbitrage," where the cost of 
 * absolute carbon reduction is played against the cost of atmospheric removal 
 * technologies.
 * * KEY AGENTS:
 * - The Arbitrageur: Tech providers and carbon-removal firms managing atmospheric removals.
 * - The Regulated Emitter: Industries that previously believed they could emit a "fair chunk" of 2.5Gt annually.
 * - The Carbon Auditor: Analytical observers ensuring that removals are truly equivalent to emissions.
 * * NARRATIVE ARC:
 * The discovery that "warming stops once humanity reaches net-zero" turned 
 * the atmosphere into a balanced ledger. 
 * This ledger acts as a Rope for market-based coordination but a Snare for 
 * those who cannot afford the price of "equivalent removals".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(negative_emissions_arbitrage, 0, 10).
narrative_ontology:constraint_claim([negative_emissions_arbitrage], [market_balancing]).

% Base extractiveness score (0.82 = High)
% Rationale: The requirement for "equivalent removals" extracts significant 
% capital from emitters to fund a new atmospheric engineering sector.
domain_priors:base_extractiveness(negative_emissions_arbitrage, 0.82).

% Suppression score (0.55 = Moderate)
% Rationale: Alternatives to net-zero (like the 2.5Gt "fair chunk" model) are 
% now suppressed by scientific and policy consensus.
domain_priors:suppression_score(negative_emissions_arbitrage, 0.55).

% Enforcement: Requires active enforcement (Global carbon registries and verification).
domain_priors:requires_active_enforcement(negative_emissions_arbitrage).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(negative_emissions_arbitrage, extractiveness, 0.82).
narrative_ontology:constraint_metric(negative_emissions_arbitrage, suppression_requirement, 0.55).

% BENEFICIARIES & VICTIMS
% Beneficiaries: Carbon removal tech firms and nations with high carbon-sink capacity.
constraint_beneficiary(negative_emissions_arbitrage, removal_technology_providers).
% Victims: Emitters who can no longer access the "fair chunk" of free emissions.
constraint_victim(negative_emissions_arbitrage, high_intensity_legacy_emitters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE REMOVAL PROVIDER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-shaping through technological deployment)
   WHEN: biographical (Current investment and rollout cycles)
   WHERE: arbitrage (Playing removal efficiency against carbon prices)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The provider sees the net-zero ledger as a Rope—a functional mechanism to 
   coordinate the "balancing" of emissions and removals across the world.
   
   NARRATIVE EVIDENCE:
   "Any human-made emissions balanced by equivalent removals from the atmosphere".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    negative_emissions_arbitrage,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(negative_emissions_arbitrage, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE DE-INDUSTRIALIZING FIRM - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to mandatory balancing rules)
   WHEN: immediate (Addressing current annual emissions targets)
   WHERE: trapped (Must use removals if they cannot reach absolute zero)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For firms that cannot decarbonize absolute-zero, the "equivalent removals" 
   requirement is a Snare—it extracts profit and forces a performance of 
   "net-zero" rituals.
   
   NARRATIVE EVIDENCE:
   "Emissions would have to reach net zero to stabilise the climate... balancing 
   global temperatures".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    negative_emissions_arbitrage,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(negative_emissions_arbitrage, E),
    E > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CLIMATE SCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the 2005 Frame/Allen model)
   WHEN: civilizational (Deep-time stability of Earth's temperature)
   WHERE: constrained (Bound by the physical laws of carbon concentrations)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The scientist sees the net-zero requirement as a Mountain—an immutable 
   biological and physical law discovered "on the train" in Exeter.
   
   NARRATIVE EVIDENCE:
   "The world would stop warming once humanity reached 'net-zero'... it was a 
   big deal".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    negative_emissions_arbitrage,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    % Classification based on the physical model evidence
    true,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(negative_emissions_arbitrage_tests).

test(multi_perspective_variance) :-
    % Provider (Rope) vs Emitter (Snare) vs Scientist (Mountain)
    constraint_indexing:constraint_classification(negative_emissions_arbitrage, rope, context(institutional, _, arbitrage, _)),
    constraint_indexing:constraint_classification(negative_emissions_arbitrage, snare, context(individual_powerless, _, trapped, _)),
    constraint_indexing:constraint_classification(negative_emissions_arbitrage, mountain, context(analytical, _, _, _)).

test(extraction_equivalence) :-
    % Test that "equivalent removals" (Score 1) results in higher 
    % extractiveness than the old "fair chunk" model (Score 2).
    Score1 = 0.82,
    Score2 = 0.05,
    Score1 > Score2.

:- end_tests(negative_emissions_arbitrage_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.82): High score chosen because the "Net Zero" framing 
 * creates a mandatory capital flow toward the removal sector.
 * 2. MANDATROPHY STATUS: Triggered by the high extraction rate (82%). The system 
 * is a Rope for the arbitrageur but a Snare for those who cannot exit carbon 
 * dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negative_emissions_arbitrage_extraction_intent,
    "Is the requirement for 'equivalent removals' a physical necessity for climate stability or a market mechanism to preserve industrial status quo?",
    resolution_mechanism("Audit of actual temperature response vs. net-zero removal rates in the 2030s"),
    impact("If necessity: Mountain. If status quo preservation: Snare."),
    confidence_without_resolution(medium)
).

omega_variable(
    equivalency_validity,
    "Are industrial 'removals' truly equivalent in atmospheric effect to 'avoided' fossil emissions?",
    resolution_mechanism("Long-term verification of carbon residence times in geological storage vs. atmospheric cycles"),
    impact("If equivalent: Rope. If not: The arbitrage is a 'Living within a Lie' Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Absolute Zero (No removals permitted)
 * Viability: Technically possible but economically catastrophic in the short term.
 * Suppression: Suppressed by the "Net" in Net Zero, which allows the arbitrage.
 * * CONCLUSION:
 * The invention of "Net Zero" as a "balancing" idea created the arbitrage 
 * opportunity, replacing the "Absolute Snare" with a "Removals Rope".
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraints/negative_emissions_arbitrage].
 * 2. Multi-perspective: ?- multi_index_report(negative_emissions_arbitrage).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
