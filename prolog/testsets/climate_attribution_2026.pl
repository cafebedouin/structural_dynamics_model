% ============================================================================
% CONSTRAINT STORY: climate_attribution_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "Pinning extreme weather on climate change" by Madeleine Cuff (Jan 2026)
% ============================================================================

:- module(climate_attribution_2026, []).

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
 * * constraint_id: climate_attribution_2026
 * human_readable: Extreme Weather Attribution Science
 * domain: scientific/political/economic
 * temporal_scope: 2003-2026
 * spatial_scope: Global
 * * SUMMARY:
 * Attribution science is a framework that quantifies how much human-induced climate 
 * change has altered the risk of specific extreme weather events. 
 * It transitions individual disasters from "acts of God" to "measurable consequences 
 * of policy and industry".
 * * KEY AGENTS:
 * - Myles Allen & Peter Stott: Pioneer scientists who established the field 
 * using model simulations of "two worlds".
 * - Displaced Resident: (e.g., Myles Allen in 2003 Oxford) - Experiences the 
 * physical constraint of floodwaters.
 * - Carbon-Heavy Industry: (Institutional) - The subject of potential legal 
 * liability enabled by this science.
 * * NARRATIVE ARC:
 * Starting with personal experiences of flooding and heatwaves in 2003, 
 * researchers used thousands of simulations to conclude that human activity 
 * at least doubled the risk of the 2003 European heatwave.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(climate_attribution_2026, 0, 10).

% Base extractiveness: 0.2 (Low-Moderate)
% Rationale: While the science itself is descriptive, its application in 
% 2026 is increasingly used for legal "extraction" of damages from polluters.
domain_priors:base_extractiveness(climate_attribution_2026, 0.2).

% Suppression: 0.3 (Low)
% Rationale: Natural variability is still recognized, but the "it's only 
% weather" narrative is being suppressed by data.
domain_priors:suppression_score(climate_attribution_2026, 0.3).

% Enforcement: Emerges naturally through scientific advancement.
domain_priors:emerges_naturally(climate_attribution_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(climate_attribution_2026, climate_litigation_plaintiffs).
constraint_victim(climate_attribution_2026, greenhouse_gas_emitters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISPLACED RESIDENT (Oxford 2003) - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Resident watching water seep into home)
   WHEN: immediate (Short-term survival)
   WHERE: trapped (Physical flooding of the property)
   SCOPE: local (Oxford, UK)
   
   WHY THIS CLASSIFICATION:
   For the individual experiencing the flood, the weather event is a Mountain. 
   It is an unchangeable natural force with zero degrees of freedom; the science 
   behind why it is happening does not change the physical fact of the water.
   
   NARRATIVE EVIDENCE:
   Myles Allen "watched as floodwaters from the Thames river threatened to 
   seep into his home".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    climate_attribution_2026,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ATTRIBUTION SCIENTIST - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Scientific authority)
   WHEN: historical (Analyzing multi-century climate trends)
   WHERE: mobile (Can simulate different world models)
   SCOPE: global (Field applied to droughts, storms, and heatwaves)
   
   WHY THIS CLASSIFICATION:
   For the scientist, attribution is a Rope—a coordination mechanism that 
   links specific data points to global trends. It is a functional tool used 
   to "pin down" responsibility.
   
   NARRATIVE EVIDENCE:
   Stott and Allen "ran the model simulations for both worlds thousands of 
   times" to identify human influence.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    climate_attribution_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LEGAL DEFENSE ANALYST - Noose
   --------------------------------------------------------------------------
   WHO: analytical (Reviewing liability risk)
   WHEN: biographical (Corporate liability horizons)
   WHERE: constrained (Bound by jurisdictional laws)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For a corporation being sued for climate damages, attribution science is 
   a Noose. It is a mechanism of enforcement that uses scientific data to 
   constrain their financial and operational freedom by proving asymmetric harm.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    climate_attribution_2026,
    noose,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(climate_attribution_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(climate_attribution_2026, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_attribution_2026, Type2, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2.

test(time_immutability_transition) :-
    % Short term = Mountain (disaster happening). Long term = Rope (causality understood).
    constraint_indexing:constraint_classification(climate_attribution_2026, mountain, context(_, immediate, _, _)),
    constraint_indexing:constraint_classification(climate_attribution_2026, rope, context(_, historical, _, _)).

:- end_tests(climate_attribution_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.2): Set low because attribution science is 
 * fundamentally a knowledge-generation tool. The extraction occurs in 
 * legal systems using this knowledge as evidence.
 * 2. PERSPECTIVE SELECTION: Used the individual powerless (resident) to ground 
 * the physical reality of extreme weather as a Mountain.
 * 3. CLASSIFICATION: Chose "Noose" for the legal/industrial perspective 
 * because the science actively "pins" liability onto agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_latency_resolution,
    "Can attribution analysis be moved from 'months/years' to 'real-time'?",
    resolution_mechanism("Development of automated near-real-time model ensemble analysis"),
    impact("If real-time: Shifts from analytical Rope to immediate political Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    legal_causality_standard,
    "Will courts accept 'doubled risk' as sufficient evidence for 'but-for' causation?",
    resolution_mechanism("Supreme Court level precedent on climate liability cases"),
    impact("If yes: Noose for emitters. If no: The science remains a purely academic Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Natural Variability Model
 * Viability: The standard view before 2004.
 * Suppression: Scientists previously "refused to blame climate change" 
 * due to a lack of attribution tools.
 * Evidence: Stott and Allen's paper was a "groundbreaking" shift away from 
 * this alternative.
 * * CONCLUSION:
 * Attribution science turned the "Natural Variability" Mountain (an excuse 
 * for inaction) into a "Human Influence" Rope (a tool for action).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [climate_attribution_2026].
% Report: ?- multi_index_report(climate_attribution_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
