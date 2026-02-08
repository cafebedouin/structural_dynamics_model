% ============================================================================
% CONSTRAINT STORY: climate_attribution_2026
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "Pinning extreme weather on climate change" by Madeleine Cuff (Jan 2026)
% ============================================================================

:- module(constraint_climate_attribution_2026, []).

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
 * 
 * constraint_id: climate_attribution_2026
 * human_readable: Extreme Weather Attribution Science
 * domain: scientific/political/economic
 * temporal_scope: 2003-2026
 * spatial_scope: Global
 * 
 * SUMMARY:
 * Attribution science quantifies how much human-induced climate change has altered 
 * the risk of specific extreme weather events. It transitions individual disasters
 * from "acts of God" to "measurable consequences of policy and industry," enabling
 * legal and political action.
 * 
 * KEY AGENTS:
 * - Displaced Resident (Individual Powerless): Experiences the physical constraint of extreme weather.
 * - Attribution Scientist (Institutional): Pioneer scientists who establish the field and quantify risks.
 * - Carbon-Heavy Industry (Analytical): The subject of potential legal liability enabled by this science.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(climate_attribution_2026, 0, 10).
narrative_ontology:constraint_claim(climate_attribution_2026, rope).

% Base extractiveness: 0.2 (Low-Moderate)
% While the science itself is descriptive, its application is increasingly used for
% legal "extraction" of damages from polluters.
domain_priors:base_extractiveness(climate_attribution_2026, 0.2).

% Suppression: 0.3 (Low)
% Natural variability is still recognized, but the "it's only weather" narrative
% is being suppressed by data.
domain_priors:suppression_score(climate_attribution_2026, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(climate_attribution_2026, extractiveness, 0.2).
narrative_ontology:constraint_metric(climate_attribution_2026, suppression_requirement, 0.3).

% Enforcement: Emerges naturally through scientific advancement.
domain_priors:emerges_naturally(climate_attribution_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(climate_attribution_2026, climate_litigation_plaintiffs).
constraint_victim(climate_attribution_2026, greenhouse_gas_emitters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISPLACED RESIDENT - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (Resident watching water seep into home)
   WHEN: immediate (Short-term survival)
   WHERE: trapped (Physical flooding of the property)
   
   WHY THIS CLASSIFICATION:
   For the individual experiencing the flood, the weather event is a 'Mountain.' 
   It is an unchangeable natural force with zero degrees of freedom; the science 
   behind why it is happening does not change the physical fact of the water.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    climate_attribution_2026,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ATTRIBUTION SCIENTIST - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Scientific authority, e.g., the IPCC or research institutions)
   WHEN: historical (Analyzing multi-century climate trends)
   WHERE: mobile (Can simulate different world models)
   
   WHY THIS CLASSIFICATION:
   For the scientist, attribution is a 'Rope'—a coordination mechanism that 
   links specific data points to global trends. It is a functional tool used 
   to "pin down" responsibility and inform policy.
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
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE LEGAL DEFENSE ANALYST - Snare
   --------------------------------------------------------------------------
   WHO: analytical (Reviewing liability risk for carbon-heavy industries)
   WHEN: biographical (Corporate liability horizons)
   WHERE: constrained (Bound by jurisdictional laws)
   
   WHY THIS CLASSIFICATION:
   For a corporation being sued for climate damages, attribution science is 
   a 'Snare.' It is a mechanism of enforcement that uses scientific data to 
   constrain their financial and operational freedom by proving asymmetric harm.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    climate_attribution_2026,
    snare,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(climate_attribution_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(climate_attribution_2026, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_attribution_2026, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(climate_attribution_2026, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(climate_attribution_tests).

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
 * 1. CLASSIFICATION RATIONALE:
 *    - Displaced Resident (Mountain): The physical reality of the flood is immutable.
 *    - Attribution Scientist (Rope): A tool for understanding and legal action.
 *    - Legal Defense Analyst (Snare): Science becomes a mechanism for financial liability.
 * 
 * 2. CORE INSIGHT: Attribution science transforms seemingly "natural" disasters
 *    into measurable consequences, shifting the landscape from a 'Mountain' of
 *    unavoidable fate to a 'Rope' for legal and political accountability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the speed and legal acceptance of attribution science.
 */

omega_variable(
    attribution_latency_resolution,
    "Can attribution analysis be moved from 'months/years' to 'real-time' (e.g., within days of an event) to enable immediate legal and political action?",
    resolution_mechanism("Development of automated near-real-time model ensemble analysis and AI-driven data processing for extreme weather events."),
    impact("If real-time: Shifts from analytical 'Rope' to immediate political 'Snare' for polluters. If slow: Science remains a purely academic 'Rope'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Natural Variability Model
 *    Viability: The standard view before 2004, where extreme weather was attributed solely to natural cycles.
 *    Suppression: Scientists previously "refused to blame climate change" due to a lack of attribution tools. This narrative is now actively suppressed by data-driven science.
 *
 * CONCLUSION:
 * Attribution science turned the "Natural Variability" 'Mountain' (an excuse for inaction)
 * into a "Human Influence" 'Rope' (a tool for accountability). The suppression of
 * the "natural variability" narrative is what empowers this science.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/climate_attribution_2026].
 * 2. Multi-perspective: ?- multi_index_report(climate_attribution_2026).
 * 3. Run tests: ?- run_tests(climate_attribution_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Coordination mechanism in scientific domain — moderate institutional framing
domain_priors:theater_ratio(climate_attribution_2026, 0.08).
narrative_ontology:constraint_metric(climate_attribution_2026, theater_ratio, 0.08).
