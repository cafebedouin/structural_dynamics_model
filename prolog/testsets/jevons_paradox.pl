% ============================================================================
% CONSTRAINT STORY: jevons_paradox
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: William Stanley Jevons (1865) / Ecological Economics
% ============================================================================

:- module(constraint_jevons_paradox, []).

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
 * * constraint_id: jevons_paradox
 * human_readable: Jevons Paradox (The Rebound Effect)
 * domain: economic/technological/environmental
 * temporal_scope: Industrial Era (1865) to Present
 * spatial_scope: Global (Industrialized Economies)
 * * SUMMARY:
 * Jevons Paradox occurs when technological progress increases the efficiency with 
 * which a resource is used, but the rate of consumption of that resource rises 
 * because of increasing demand. This counter-intuitive result suggests that 
 * efficiency alone cannot solve resource depletion; it often accelerates it 
 * by making the resource cheaper and more accessible.
 * * KEY AGENTS:
 * - The Efficiency Engineer: Tasked with reducing resource input per unit of output.
 * - The Industrial Capitalist: Reinvests savings from efficiency into expanded production.
 * - The Global Ecosystem: The ultimate sink that must absorb the increased total throughput.
 * * NARRATIVE ARC:
 * Originally observed regarding coal use in the UK, the paradox transforms "savings" 
 * into "expansion." It functions as a "Mountain" of economic logic, a "Rope" for 
 * industrial growth, and a "Noose" for environmental sustainability as it 
 * strangles conservation efforts with their own success.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for extraction
narrative_ontology:interval(jevons_interval, 0, 10).
narrative_ontology:constraint_claim(jevons_paradox, rope).

% Base extractiveness: How asymmetric is the benefit flow?
% Rationale: 0.6 (Moderate-High). The paradox extracts "environmental stability" 
% and "resource future" to fuel current industrial expansion.
domain_priors:base_extractiveness(jevons_paradox, 0.6).

% Suppression: How much are alternatives hidden/punished?
% Rationale: 0.5 (Moderate). Alternatives like "Sufficiency" (capping total use) 
% are visible but often suppressed by the dominant narrative that efficiency 
% is the primary/only path to sustainability.
domain_priors:suppression_score(jevons_paradox, 0.5).

% Enforcement: Emerges naturally from market dynamics (supply/demand/elasticity).
domain_priors:emerges_naturally(jevons_paradox).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(jevons_paradox, [industrial_expansion, consumer_markets]).
constraint_victim(jevons_paradox, [planetary_boundaries, future_resource_availability]).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(jevons_paradox, extractiveness, 0.6).
narrative_ontology:constraint_metric(jevons_paradox, suppression_requirement, 0.5).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ECONOMIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of systemic incentives and price elasticity.
   WHEN: civilizational - Viewing resource use across the history of capitalism.
   WHERE: analytical - Not a participant, but a mapper of "unbreakable" market laws.
   SCOPE: global - Universal behavior of industrial systems.
   
   WHY THIS CLASSIFICATION:
   To the analyst, the paradox is a Mountain. It is an unchangeable feature 
    of human economic behavior: when a resource becomes cheaper through 
    efficiency, humans use more of it. It is a fixed peak in the topography 
    of industrial logic.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    jevons_paradox,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, analytical, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TECH OPTIMIST / CEO - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design systems and drive investment.
   WHEN: biographical - Achieving growth and market share over a career.
   WHERE: arbitrage - Can pivot between different technologies and markets.
   SCOPE: national - Driving the national economy.
   
   WHY THIS CLASSIFICATION:
   For the institutional builder, efficiency is a Rope. It is the primary 
    coordination mechanism for progress. By lowering costs, it pulls the 
    entire economy forward, enabling more people to access goods and 
    services that were previously too expensive. The "rebound" is simply 
    proof of success.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    jevons_paradox,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(jevons_paradox, E),
    E < 0.7, % It's a tool for growth until it reaches extreme extraction
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ENVIRONMENTAL CONSERVATIONIST - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the outcomes of a systemic trap.
   WHEN: immediate - Today's rising emissions/depletion despite "green" tech.
   WHERE: constrained - Limited by a global market that enforces expansion.
   SCOPE: local - Immediate ecosystem or atmosphere.
   
   WHY THIS CLASSIFICATION:
   For the conservationist, Jevons Paradox is a Noose. They fight for 
    efficiency as a way to "save" the planet, only to find that every 
    technological victory tightens the trap by increasing total throughput. 
    The efficiency logic strangles the possibility of genuine reduction, 
    extracting the last remnants of ecological stability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    jevons_paradox,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(jevons_paradox, E),
    E > 0.4, % Extraction of conservation utility
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(jevons_paradox_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(jevons_paradox, mountain, context(analytical, civilizational, analytical, global)),
    constraint_indexing:constraint_classification(jevons_paradox, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(jevons_paradox, noose, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_rebound) :-
    % Powerless conservationists feel the total extraction of their efforts (Noose).
    % Institutional leaders leverage the rebound for market growth (Rope).
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(jevons_paradox, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(jevons_paradox, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_market_laws) :-
    % Long-term historical view treats price elasticity as a Mountain.
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(jevons_paradox_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.6):
 * Reasoning: The paradox extracts "future resource availability" to subsidize 
 * present industrial expansion. It is a classic "bait and switch" where 
 * efficiency gains are captured by growth.
 * * 2. SUPPRESSION SCORE (0.5):
 * Reasoning: It suppresses the alternative of "Sufficiency" (absolute 
 * caps on resource use). The narrative that we can "efficiency our way 
 * out" of the problem is ubiquitous, making the idea of "using less 
 * by doing less" psychologically and politically invisible.
 * * 3. PERSPECTIVE SELECTION:
 * Chose the Analyst (Law), the Growth-Oriented Institutional (Tool), and 
 * the Powerless Conservationist (Trap) to show the indexical range.
 * * 4. AMBIGUITIES:
 * - The "Backfire" point: Jevons is only a paradox if the rebound is > 100%. 
 * I assumed the "backfire" state for this story as it represents the 
 * strongest version of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_elasticity_limit,
    "Is there a physical limit where price elasticity breaks and efficiency 
    finally leads to net reduction (Mountain), or will human demand always 
    expand into the new vacancy (Noose)?",
    resolution_mechanism("Long-term tracking of 'mature' technology markets 
    to see if consumption ever plateaus as costs approach zero"),
    impact("If Mountain: Efficiency eventually works. If Noose: Capping total 
    throughput is the only exit."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Resource Caps / Rationing (Sufficiency)
 * Viability: Theoretically the only way to ensure efficiency leads to 
 * conservation. If total coal use is capped, efficiency just makes 
 * life better within that cap.
 * Suppression: Extreme. Viewed as "anti-growth" or "authoritarian" 
 * in modern neoliberal systems.
 * Evidence: Hard limits on emissions (caps) are frequently lobbied 
 * against in favor of "intensity targets" (efficiency).
 * * CONCLUSION:
 * The existence of "Caps" as a suppressed alternative is what converts 
 * Jevons Paradox from a natural Mountain into a social Noose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/jevons_paradox].
 * 2. Multi-perspective: ?- multi_index_report(jevons_paradox).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
