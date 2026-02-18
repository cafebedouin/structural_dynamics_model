% ============================================================================
% CONSTRAINT STORY: electrification_scale_2026
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The electrification of everything" by Chris Stokel-Walker
% ============================================================================

:- module(constraint_electrification_scale_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: electrification_scale_2026
 * human_readable: Industrial Scale Electrification
 * domain: technological/economic/environmental
 * temporal_scope: 2010s-2026
 * spatial_scope: Global (initially Nevada/Tesla)
 * 
 * SUMMARY:
 * The transition from fossil fuels to renewables catalyzed by "gigafactories"
 * that leverage economies of scale and supply-chain integration.
 * This constraint represents the shift from fossil fuel-powered infrastructure
 * to a dispatchable solar and electric vehicle ecosystem.
 * 
 * KEY AGENTS:
 * - Coal Miner / Oil Rig Worker (Individual Powerless): Faces job displacement and livelihood threats.
 * - Energy Planner (Institutional): Views electrification as a tool for dispatchable power.
 * - Legacy Auto/Oil Industry (Individual Powerful): Views the shift as a disruptive Snare.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(electrification_scale_2026, 0, 10).
narrative_ontology:constraint_claim(electrification_scale_2026, tangled_rope).
narrative_ontology:human_readable(electrification_scale_2026, "Industrial Scale Electrification").
narrative_ontology:topic_domain(electrification_scale_2026, "technological/economic/environmental").

% Base extractiveness: 0.15 (Low)
% The system extracts energy from the sun rather than from labor
% or subjects, though mineral extraction for batteries is an externalized
% factor not deeply explored in the source.
domain_priors:base_extractiveness(electrification_scale_2026, 0.15).

% Suppression: 0.5 (Moderate)
% Renewables are becoming "competitive with fossil fuels,"
% effectively suppressing the economic viability of the older model.
domain_priors:suppression_score(electrification_scale_2026, 0.5).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(electrification_scale_2026, extractiveness, 0.15).
narrative_ontology:constraint_metric(electrification_scale_2026, suppression_requirement, 0.5).

% Enforcement: Requires active enforcement: Scaling to "giga" levels requires massive capital
% integration "under one roof".
domain_priors:requires_active_enforcement(electrification_scale_2026).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(electrification_scale_2026, renewable_energy_sector).
narrative_ontology:constraint_victim(electrification_scale_2026, fossil_fuel_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: COAL MINER / OIL RIG WORKER - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Faces job displacement and livelihood threats)
   WHEN: immediate (Short-term economic disruption)
   WHERE: trapped (Dependent on declining fossil fuel industries)
   
   WHY THIS CLASSIFICATION:
   For a coal miner or oil rig worker, electrification is a 'Snare'. The rapid 
   shift to renewables threatens their livelihoods, communities, and traditional
   ways of life, leaving them with limited viable exit options within their existing skill sets.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    electrification_scale_2026,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: ENERGY PLANNER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-shaping/Planning for national energy infrastructure)
   WHEN: biographical (Planning for the 2010s-2026 transition)
   WHERE: mobile (Can navigate global energy markets and technological shifts)
   
   WHY THIS CLASSIFICATION:
   For the institutional energy planner, electrification is a 'Rope'—a coordination
   mechanism that finally makes solar "dispatchable" through battery
   integration. It provides a way to move the energy system
   from one state to another using technological leverage.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    electrification_scale_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: LEGACY AUTO/OIL INDUSTRY - Snare
   --------------------------------------------------------------------------
   WHO: powerful (Legacy industrial leaders with entrenched infrastructure)
   WHEN: immediate (Short-term loss of competitiveness and market share)
   WHERE: constrained (Tied to fossil fuel reserves/combustion tech)
   
   WHY THIS CLASSIFICATION:
   As solar electricity becomes competitive and EV production scales, the legacy
   fossil fuel system faces a 'Snare'. Their exit options are costly as the
   "turning point" makes their model increasingly obsolete. This threatens their
   long-term viability and market dominance.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    electrification_scale_2026,
    snare,
    context(
        agent_power(powerful),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(electrification_scale_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(electrification_scale_2026, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electrification_scale_2026, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(electrification_scale_2026, Type3, context(agent_power(powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(electrification_scale_2026_tests).

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
 * 1. INDIVIDUAL POWERLESS PERSPECTIVE: Added 'Coal Miner / Oil Rig Worker' to
 *    represent the human cost of the energy transition, experiencing it as a 'Snare'.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Coal Miner (Snare): Job displacement, livelihood threats.
 *    - Energy Planner (Rope): A tool for building a new energy system.
 *    - Legacy Industry (Snare): Obsolete model, loss of market share.
 * 
 * 3. TANGLED ROPE: Electrification is a 'Tangled Rope'. It is a 'Rope'
 *    for energy planners and the environment, facilitating a cleaner future.
 *    However, it becomes a 'Snare' for those whose livelihoods are tied to
 *    the fossil fuel industry, creating significant social and economic friction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the sustainability of the required mineral supply and grid stability.
 */

omega_variable(
    lithium_supply_ceiling,
    "Is the availability of critical minerals (e.g., lithium, rare earths) a 'Mountain' that will eventually limit the Giga-scale transition, or will new chemistries and recycling create a 'Rope' of continuous supply?",
    resolution_mechanism("Geological audit of extractable reserves vs. projected vehicle and storage demand by 2040; investment in battery recycling technologies."),
    impact("If Mountain: The transition stalls. If Rope: Electrification continues unabated."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Hydrogen Fuel Cells
 *    Viability: Historically seen as a direct alternative to Li-ion for transport and energy storage.
 *    Suppression: Outscaled by battery "gigafactories" due to economies of scale and faster market adoption.
 *
 * CONCLUSION:
 * While electrification is a powerful 'Rope' for energy transition, its
 * rapid scaling creates a 'Snare' for legacy industries and workers. The
 * suppression of alternative energy vectors (like hydrogen) further solidifies
 * the path towards a predominantly battery-electric future.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/electrification_scale_2026].
 * 2. Multi-perspective: ?- multi_index_report(electrification_scale_2026).
 * 3. Run tests: ?- run_tests(electrification_scale_2026_tests).
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
% Mixed coordination/extraction — theater masks extraction component
domain_priors:theater_ratio(electrification_scale_2026, 0.24).
narrative_ontology:constraint_metric(electrification_scale_2026, theater_ratio, 0.24).
