% ============================================================================
% CONSTRAINT STORY: innovators_dilemma
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Clayton Christensen (1997) / Disruptive Innovation Theory
% ============================================================================

:- module(constraint_innovators_dilemma, []).

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
 * * constraint_id: innovators_dilemma
 * human_readable: The Innovator's Dilemma
 * domain: economic/technological
 * temporal_scope: Modern Industrial/Digital Era
 * spatial_scope: Global (Market Economies)
 * * SUMMARY:
 * The Innovator's Dilemma describes how successful companies can do 
 * everything "right"—listening to their best customers and investing in 
 * high-margin products—yet still lose market leadership to "disruptive" 
 * technologies that start in low-margin, niche markets. It represents a 
 * structural conflict between sustaining existing success and embracing 
 * future volatility.
 * * KEY AGENTS:
 * - The Incumbent CEO: Institutional agent bound by fiduciary duty and 
 * high-margin customer demands.
 * - The Disruptive Entrant: Individual moderate/powerful agent with 
 * high mobility and low overhead, targeting the "bottom" of the market.
 * - The Analytical Market Historian: Observer mapping the predictable 
 * collapse of dominant firms.
 * * NARRATIVE ARC:
 * Success creates a "Mountain" of institutional inertia. For the CEO, 
 * the focus on best customers is a "Rope" for coordination and profit. 
 * However, as a disruptive tech improves, that same focus becomes a 
 * "Snare," as the incumbent is unable to pivot without destroying its 
 * existing business model, leading to the extraction of its market value.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(innovators_dilemma_interval, 0, 10).
narrative_ontology:constraint_claim(innovators_dilemma, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). The dilemma extracts "future viability" from 
% successful firms. It forces an "innovation tax" where current success 
% subsidizes a future competitor's entry.
domain_priors:base_extractiveness(innovators_dilemma, 0.5).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). The "Disruptive Path" is actively suppressed 
% within the incumbent firm by resource allocation processes that 
% favor high-margin sustaining projects.
domain_priors:suppression_score(innovators_dilemma, 0.7).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(innovators_dilemma, extractiveness, 0.5).
narrative_ontology:constraint_metric(innovators_dilemma, suppression_requirement, 0.7).

% Enforcement requirements
% Emerges naturally from the rational pursuit of profit and customer satisfaction.
domain_priors:emerges_naturally(innovators_dilemma).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(innovators_dilemma, [disruptive_startups, venture_capitalists]).
constraint_victim(innovators_dilemma, [incumbent_shareholders, legacy_employees]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE BUSINESS HISTORIAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of market cycles and firm lifespans.
   WHEN: historical - Viewing the rise and fall of industries over centuries.
   WHERE: analytical - Not bound by quarterly earnings reports.
   SCOPE: global - Universal to capitalistic competition.
   
   WHY THIS CLASSIFICATION:
   To the historian, the dilemma is a Mountain. It is an unchangeable 
   feature of the economic "hardware." The pattern of established 
   leaders being toppled by cheaper, simpler technologies is a 
   fixed peak in the topography of capitalism that repeats regardless 
   of the specific industry.
   
   NARRATIVE EVIDENCE:
   Christensen's data on the disk drive and excavator industries 
   showed a nearly identical pattern of failure among leaders across 
   decades of technological shifts.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    innovators_dilemma,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, historical, analytical, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SHAREHOLDER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to demand returns and shape strategy.
   WHEN: biographical - Seeking portfolio growth over 20-40 years.
   WHERE: arbitrage - Can move capital between incumbents and startups.
   SCOPE: national - Bound by market indices and regulations.
   
   WHY THIS CLASSIFICATION:
   For the investor, the dilemma is a Rope. It is a coordination 
   mechanism for capital efficiency. By demanding that incumbents 
   focus on high margins, they "tether" the firm to immediate 
   productivity, while using their own mobility to fund the 
   "disruptors" on the side.
   
   NARRATIVE EVIDENCE:
   "We expect the company to focus on its core competencies and 
   maximize dividend yield; we will look elsewhere for high-risk 
   speculative growth."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    innovators_dilemma,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(innovators_dilemma, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INCUMBENT MANAGER - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to internal KPIs and budgets.
   WHEN: immediate - Today's quarterly goal and customer complaint.
   WHERE: trapped - Bound by the firm's specific values and cost structures.
   SCOPE: local - Immediate business unit.
   
   WHY THIS CLASSIFICATION:
   For the manager inside a successful firm, the dilemma is a Snare. 
   They see the disruptive threat, but the system "strangles" their 
   ability to respond. If they invest in the new tech, they fail 
   their current customers and kill their margins; if they don't, 
   the firm eventually dies. The successful business model extracts 
   their ability to innovate, leaving them trapped in a terminal decline.
   
   NARRATIVE EVIDENCE:
   "I proposed the new project, but it was rejected because the 
   projected margins were too low to interest our sales team."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    innovators_dilemma,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(innovators_dilemma, E),
    E > 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(innovators_dilemma_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Investor sees Rope, Manager sees Snare
    constraint_indexing:constraint_classification(innovators_dilemma, mountain, context(analytical, historical, analytical, global)),
    constraint_indexing:constraint_classification(innovators_dilemma, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(innovators_dilemma, snare, context(powerless, immediate, trapped, local)).

test(power_extractiveness_viability) :-
    % Powerless managers feel the extraction of their agency (Snare).
    % Institutional investors arbitrage the dilemma for capital gains (Rope).
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(innovators_dilemma, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(innovators_dilemma, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_market) :-
    % Civilizational view = Mountain.
    constraint_indexing:effective_immutability(historical, trapped, mountain).

:- end_tests(innovators_dilemma_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.5):
 * Reasoning: The dilemma extracts "Corporate Vitality." Successful firms 
 * are forced to subsidize their own destruction by ignoring low-end 
 * disruptions to please high-end customers.
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Historian (Pattern Law), the Investor (Strategic Tool), 
 * and the Manager (Internal Victim) to demonstrate how success 
 * becomes a structural trap.
 * * 3. CLASSIFICATION RATIONALE:
 * Analytical → Mountain: The law of creative destruction is a market invariant.
 * Institutional → Rope: It allows capital to move where it is most efficient.
 * Powerless → Snare: It prevents internal change, leading to personal/professional ruin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spin_off_efficacy,
    "Can an incumbent 'untie' the Snare (Rope) by creating an independent 
    autonomous business unit, or does the parent firm's 'Mountain' of 
    values always crush the spin-off?",
    resolution_mechanism("Long-term tracking of incumbent survival rates 
    using 'spin-off' vs 'internal-pivot' strategies during disruptive shifts"),
    impact("If Rope: The dilemma can be managed. If Mountain: Firms have 
    finite lifespans dictated by their initial value network."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Cannibalization (Eating your own lunch)
 * Viability: High. A firm can choose to launch a lower-margin product 
 * that destroys its high-margin cash cow.
 * Suppression: High. Actively suppressed by finance departments and 
 * sales teams who see it as "irrational" to kill a profitable product.
 * * CONCLUSION:
 * The existence of a suppressed "Cannibalization" alternative confirms 
 * that the Innovator's Dilemma is a Snare—the system's current success 
 * is the very thing that prevents the exit toward survival.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_innovators_dilemma].
 * 2. Multi-perspective: ?- multi_index_report(innovators_dilemma).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
