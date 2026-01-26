% ============================================================================
% CONSTRAINT STORY: planetary_boundaries
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Stockholm Resilience Centre / Structural Dynamics Fleet
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_planetary_boundaries, []).

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
 * 
 * constraint_id: planetary_boundaries
 * human_readable: Planetary Boundaries (Earth System Limits)
 * domain: environmental/political/economic
 * temporal_scope: 2009-Present
 * spatial_scope: Global
 * 
 * SUMMARY:
 * Planetary Boundaries define the "safe operating space for humanity" across 
 * nine Earth system processes. While biophysical (Mountain), 
 * they function as a mandate for global coordination (Rope) that can cap 
 * economic margins for developing populations (Snare).
 * 
 * KEY AGENTS:
 * - Developing Populations (Individual Powerless): Experience economic margins capped by boundary adherence.
 * - Global Governance (Institutional): Seeks to coordinate policy to maintain safe operating space.
 * - Earth System Scientists (Analytical): Define and monitor the biophysical limits of the planet.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(planetary_boundaries, 0, 10).
narrative_ontology:constraint_claim(planetary_boundaries, mountain).

% Base extractiveness: 0.8. 
% High extraction of developmental potential to maintain biophysical stability.
domain_priors:base_extractiveness(planetary_boundaries, 0.8).

% Suppression score: 0.8.
% The biophysical limits strongly suppress the alternative of unlimited economic growth
% and resource consumption, especially for developing nations.
domain_priors:suppression_score(planetary_boundaries, 0.8).

% Enforcement: Requires active enforcement by global governance bodies and nation-states.
domain_priors:requires_active_enforcement(planetary_boundaries).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(planetary_boundaries, global_ecosystems).
constraint_victim(planetary_boundaries, developing_economic_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: DEVELOPING POPULATIONS - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Experience economic margins capped by boundary adherence)
   WHEN: biographical (Short-to-medium term development goals)
   WHERE: trapped (Bound by global environmental regulations)
   
   WHY THIS CLASSIFICATION:
   For developing populations, adherence to planetary boundaries can be a 'Snare'. 
   Their economic development ambitions are constrained, as carbon budgets and
   resource limits effectively cap their industrial growth, often perceived as
   "developmental extraction" by already developed nations.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    planetary_boundaries, 
    snare, 
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: GLOBAL GOVERNANCE - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Seeks to coordinate policy to maintain safe operating space)
   WHEN: generational (Planning for long-term human well-being)
   WHERE: arbitrage (Balancing national interests with planetary limits)
   
   WHY THIS CLASSIFICATION:
   For global governance bodies (e.g., UN, IPCC), planetary boundaries are a 'Rope'. 
   They provide a scientific framework for coordinating international policy and
   action, offering a tool to guide humanity towards a sustainable future by
   defining critical thresholds.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    planetary_boundaries, 
    rope, 
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: EARTH SYSTEM SCIENTISTS - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Define and monitor the biophysical limits of the planet)
   WHEN: civilizational (Observing long-term Earth system processes)
   WHERE: analytical (Understanding immutable natural laws)
   
   WHY THIS CLASSIFICATION:
   To Earth System Scientists, planetary boundaries represent a 'Mountain'. 
   They are objective biophysical limits that, once crossed, could lead to
   irreversible environmental changes. These boundaries are immutable natural
   laws that cannot be negotiated, only respected.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    planetary_boundaries, 
    mountain, 
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(planetary_boundaries_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(planetary_boundaries, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_boundaries, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(planetary_boundaries, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(planetary_boundaries_tests).

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
 * 1. MANDATROPHY STATUS: High extractiveness (0.8) and suppression (0.8)
 *    confirm this as a severe Mandatrophic constraint. It is 'RESOLVED' by
 *    recognizing that the 'Mountain' of biophysical limits is interpreted as
 *    a 'Rope' for global coordination, but a 'Snare' for developing economies.
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Developing Populations (Snare): Economic growth constrained by limits.
 *    - Global Governance (Rope): Tool for international environmental policy.
 *    - Earth System Scientists (Mountain): Immutable biophysical laws.
 * 
 * 3. CORE INSIGHT: Planetary Boundaries function as a 'Mountain' of biophysical
 *    reality. This scientific understanding becomes a 'Rope' for global
 *    governance, enabling coordination for sustainability. However, this same
 *    framework can act as an economic 'Snare' for developing nations,
 *    highlighting a profound tension between environmental imperative and
 *    developmental equity.
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
    physics_vs_policy_tension,
    "To what degree is a boundary violation a physical 'Mountain' collapse versus a policy-driven 'Snare' tightening that could be renegotiated or alleviated?",
    resolution_mechanism("Real-time monitoring of Earth system process thresholds coupled with political economic analysis of policy enforcement mechanisms and their differential impact on nations."),
    impact("If Mountain: Inevitable biophysical collapse, regardless of human policy. If Snare: Reformable developmental ceilings imposed by human choice."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Unlimited Economic Growth / Resource Consumption
 *    Viability: The dominant economic paradigm of the 19th and 20th centuries.
 *    Suppression: Actively suppressed by scientific understanding of finite resources and ecological limits.
 *
 * CONCLUSION:
 * Planetary Boundaries represent an unavoidable 'Mountain' that fundamentally
 * suppresses the alternative of unlimited growth. While this creates a 'Rope'
 * for global coordination, it is simultaneously a developmental 'Snare'
 * for those aspiring to improve their living standards without access to
 * resource-decoupled technologies.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/planetary_boundaries].
 * 2. Multi-perspective: ?- multi_index_report(planetary_boundaries).
 * 3. Run tests: ?- run_tests(planetary_boundaries_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */