% ============================================================================
% CONSTRAINT STORY: steinmetz_valuation_asymmetry
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Gilbert King, "Charles Proteus Steinmetz, the Wizard of Schenectady"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_steinmetz_valuation_asymmetry, []).

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
 * * constraint_id: steinmetz_valuation_asymmetry
 * human_readable: The Steinmetz Chalk Mark (Knowledge Valuation)
 * domain: economic/technological
 * temporal_scope: 1910s-1920s (Industrial Era)
 * spatial_scope: National (United States / Ford Plant)
 * * SUMMARY:
 * This constraint defines the extreme asymmetry between the physical labor of a task 
 * and the specialized knowledge required to perform it correctly. 
 * Charles Steinmetz solves a "gigantic generator" problem for Henry Ford with a 
 * single chalk mark, billing $10,000 ($1 for the mark, $9,999 for "knowing where").
 * * KEY AGENTS:
 * - Charles Steinmetz: The expert (Analytical/Individual Moderate) who wields knowledge as power.
 * - Henry Ford: The industrialist (Institutional/Individual Powerful) who initially balks at paying for knowledge over labor.
 * - Ford’s Engineers: The "skeptical" practitioners (Individual Powerless) who failed to solve the problem.
 * * NARRATIVE ARC:
 * A critical industrial asset is paralyzed; Ford's internal experts are trapped by 
 * the complexity. Steinmetz is called in as a 
 * "Wizard" to provide an arbitrage between the $1 mark and the $10,000 value of 
 * operational perfection. The constraint 
 * functions as a "Snare" for the desperate client but a "Rope" for the rare expert.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% The Structural Anchor
narrative_ontology:interval(steinmetz_valuation_asymmetry, 0, 10).
narrative_ontology:constraint_claim([steinmetz_valuation_asymmetry], [economic_valuation]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.9). From the perspective of physical work ($1), the $9,999 
% fee for "knowing" represents a massive extraction of capital for a zero-marginal-cost 
% act.
domain_priors:base_extractiveness(steinmetz_valuation_asymmetry, 0.9).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.5). Alternatives (internal engineers) were visible but 
% failed, making the "wizard's" solution the only viable path to "perfection".
domain_priors:suppression_score(steinmetz_valuation_asymmetry, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, extractiveness, 0.9).
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, suppression_requirement, 0.5).

% Enforcement requirements
% Requires active enforcement via a personal itemized invoice from Steinmetz/GE.
domain_priors:requires_active_enforcement(steinmetz_valuation_asymmetry).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(steinmetz_valuation_asymmetry, rare_expertise). % Steinmetz/GE gains capital
constraint_victim(steinmetz_valuation_asymmetry, desperate_utility).   % Ford pays $10k to restore the generator

% Metrics required for Section 1 of the Executive Summary
/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SKEPTICAL ENGINEERS - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless (Failed internal staff)
   WHEN: immediate (Two straight days and nights of scribbling)
   WHERE: trapped (Could not solve the problem internally)
   SCOPE: local (The broken generator)
   
   WHY THIS CLASSIFICATION:
   For the engineers, the failure was a "Mountain." The solution was invisible 
   and unchangeable through their current techniques, requiring a "Wizard" 
   to descend and mark the "law" of the machine's malfunction.
   
   NARRATIVE EVIDENCE:
   "electrical engineers couldn’t solve some problems they were having... skeptical engineers".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    steinmetz_valuation_asymmetry,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: HENRY FORD - Snare
   --------------------------------------------------------------------------
   
   WHO: institutional (Owner of the plant)
   WHEN: biographical (The "life" of the $10,000 budget)
   WHERE: trapped (Desperate to fix the "gigantic generator")
   SCOPE: national (Industrial scale)
   
   WHY THIS CLASSIFICATION:
   For Ford, the invoice is a "Snare." He recognizes the success but "balks" at 
   the extraction of $9,999 for a "chalk mark," realizing he is paying a 
   premium for a resource (expertise) he cannot manufacture.
   
   NARRATIVE EVIDENCE:
   "Henry Ford was thrilled until he got an invoice... balked at the figure".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    steinmetz_valuation_asymmetry,
    snare,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(steinmetz_valuation_asymmetry, E),
    E > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: CHARLES STEINMETZ - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (The "Wizard" using notebooks and cots)
   WHEN: civilizational (The enduring value of the "knowing where")
   WHERE: mobile (Rejects assistance, works independently)
   SCOPE: global (The logic of expert valuation)
   
   WHY THIS CLASSIFICATION:
   For Steinmetz, the $10,000 bill is a "Rope"—a functional coordination 
   mechanism between the value of high-stakes problem solving and industrial 
   capital. It justifies the years of study required to "know where".
   
   NARRATIVE EVIDENCE:
   "Knowing where to make mark $9,999... Ford paid the bill".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    steinmetz_valuation_asymmetry,
    rope,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(steinmetz_valuation_asymmetry_tests).

test(multi_perspective_valuation) :-
    % Engineers (Powerless) see Mountain
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, mountain, context(powerless, immediate, trapped, local)),
    % Ford (Powerful) sees Snare
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, snare, context(institutional, biographical, trapped, national)),
    % Steinmetz (Analytical) sees Rope
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, rope, context(analytical, civilizational, mobile, global)),
    Type1 \= Type2, Type2 \= Type3.

test(power_extractiveness_valuation) :-
    % Ford (Powerful) experiences the $10,000 extraction, while Steinmetz (Expert) benefits.
    ContextPowerful = context(institutional, biographical, trapped, national),
    ContextExpert = context(analytical, civilizational, mobile, global),
    constraint_indexing:extractiveness_for_agent(steinmetz_valuation_asymmetry, ContextPowerful, Score1),
    constraint_indexing:extractiveness_for_agent(steinmetz_valuation_asymmetry, ContextExpert, Score2),
    Score1 > Score2.

test(time_immutability_expertise) :-
    % Short-term labor is a Mountain (fact of the mark), but long-term knowledge is a Rope.
    constraint_indexing:effective_immutability(civilizational, mobile, rope).

:- end_tests(steinmetz_valuation_asymmetry_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.9):
 * Reasoning: The story is the quintessential example of high-extraction knowledge 
 * valuation. The fee is 9,999% higher than the material labor cost ($1).
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Engineers (failure/Mountain), Ford (cost/Snare), and Steinmetz 
 * (value/Rope) to demonstrate how the same invoice changes from a tragedy 
 * to an expense to a triumph.
 * * 3. MANDATROPHY RESOLUTION:
 * [RESOLVED MANDATROPHY]. The extreme extraction ($9,999 markup) is resolved 
 * by the "knowing where" metric, which transforms the "predatory" bill into 
 * a fair Rope for the maintenance of high-value industrial systems.
 * * 4. AMBIGUITIES:
 * - The text doesn't explicitly state the downtime cost to Ford, but the 
 * "gigantic generator" and Ford's willingness to pay the $10,000 imply 
 * that the alternative (a non-working machine) was a far more lethal Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    steinmetz_valuation_intent,
    "Is the $10,000 fee a functional necessity to maintain the GE/Steinmetz knowledge 
    ecosystem (Mountain) or a predatory extraction targeting Ford's desperation (Snare)?",
    resolution_mechanism("Audit of GE's standard billing for similar expert interventions vs. competitor pricing"),
    impact("If necessity: Professional Mountain. If predatory: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Replacing the Generator
 * Viability: Extreme cost/time, likely far exceeding $10,000.
 * Suppression: Shunted by the desire for "perfection" from the current machine.
 * * ALTERNATIVE 2: Hourly Labor Billing
 * Viability: Paying Steinmetz for 48 hours of work (cots and pencils).
 * Suppression: Steinmetz explicitly rejects this by itemizing "Knowing where".
 * * CONCLUSION:
 * The existence of Alternative 2 (Hourly Billing) is what Steinmetz actively 
 * suppresses to transform his labor into an high-extractiveness Snare for Ford, 
 * which Ford eventually accepts as a Rope for his business.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/steinmetz_valuation_asymmetry].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(steinmetz_valuation_asymmetry).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
