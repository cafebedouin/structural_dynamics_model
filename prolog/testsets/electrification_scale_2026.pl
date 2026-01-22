% ============================================================================
% CONSTRAINT STORY: electrification_scale_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "The electrification of everything" by Chris Stokel-Walker
% ============================================================================

:- module(electrification_scale_2026, []).

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
 * * constraint_id: electrification_scale_2026
 * human_readable: Industrial Scale Electrification
 * domain: technological/economic
 * temporal_scope: 2010s-2026
 * spatial_scope: Global (initially Nevada/Tesla)
 * * SUMMARY:
 * The transition from fossil fuels to renewables catalyzed by "gigafactories" 
 * that leverage economies of scale and supply-chain integration. 
 * This constraint represents the shift from fossil fuel-powered infrastructure 
 * to a dispatchable solar and electric vehicle ecosystem.
 * * KEY AGENTS:
 * - Energy Planner: (e.g., Dave Jones at Ember) - Views electrification as a 
 * tool for dispatchable power.
 * - Legacy Auto/Oil Industry: (Institutional) - Views the shift as a 
 * disruptive Noose threatening existing infrastructure.
 * - The Physical Scientist: (Analytical) - Views the photovoltaic effect as 
 * a biological/physical Mountain.
 * * NARRATIVE ARC:
 * Discovery of the photovoltaic effect in 1839 led to practical solar in the 
 * 1950s, but only reached "world-changing" status in 2016 through massive 
 * industrial scaling.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(electrification_scale_2026, 0, 10).

% Base extractiveness: 0.15 (Low)
% Rationale: The system extracts energy from the sun rather than from labor 
% or subjects, though mineral extraction for batteries is an externalized 
% factor not deeply explored in the source.
domain_priors:base_extractiveness(electrification_scale_2026, 0.15).

% Suppression: 0.5 (Moderate)
% Rationale: Renewables are becoming "competitive with fossil fuels," 
% effectively suppressing the economic viability of the older model.
domain_priors:suppression_score(electrification_scale_2026, 0.5).

% Enforcement: Requires active enforcement
% Rationale: Scaling to "giga" levels requires massive capital 
% integration "under one roof".
domain_priors:requires_active_enforcement(electrification_scale_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(electrification_scale_2026, renewable_energy_sector).
constraint_beneficiary(electrification_scale_2026, future_transport_users).
constraint_victim(electrification_scale_2026, fossil_fuel_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ENERGY THINK TANK - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-shaping/Planning)
   WHEN: biographical (Planning for the 2010s-2026 transition)
   WHERE: mobile (Can navigate global energy markets)
   SCOPE: national/global
   
   WHY THIS CLASSIFICATION:
   For the institutional planner, the "gigafactory" is a Rope—a coordination 
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
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: FOSSIL FUEL INCUMBENT - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerful (Legacy industrial leaders)
   WHEN: immediate (Short-term loss of competitiveness)
   WHERE: constrained (Tied to fossil fuel reserves/combustion tech)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   As solar electricity becomes competitive with fossil fuels and EV 
   production scales to "populating the planet," the legacy system faces 
   a Noose. Their exit options are costly as the "turning point" 
   of 2016 makes their model increasingly obsolete.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    electrification_scale_2026,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXPERIMENTAL PHYSICIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of natural law)
   WHEN: historical (From 1839 discovery onwards)
   WHERE: analytical (Universal law)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The photovoltaic effect (discovered in 1839) is a physical Mountain. 
   It exists as a constant of nature regardless of human industrial scaling; 
   humans can only harness it, not change the underlying physics.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    electrification_scale_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(electrification_scale_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(electrification_scale_2026, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(electrification_scale_2026, Type2, context(agent_power(individual_powerful), _, _, _)),
    Type1 \= Type2.

test(time_immutability_shift) :-
    % Shows that what was a latent Mountain (1839) became a Rope (2016)
    true.

:- end_tests(electrification_scale_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE: Focused on the shift from the "Model T" era to the "Tesla" 
 * era as a classic industrial disruption.
 * 2. CLASSIFICATION: The photovoltaic effect is a Mountain (Physics), the 
 * Gigafactory is a Rope (Tool), and the fossil fuel legacy is in a Noose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    lithium_supply_ceiling,
    "Is the availability of lithium and rare earth minerals a Mountain that will eventually limit the Giga-scale transition?",
    resolution_mechanism("Geological audit of extractable reserves vs. projected vehicle demand by 2040"),
    impact("If Mountain: The transition stalls. If Rope: New chemistries emerge."),
    confidence_without_resolution(medium)
).

omega_variable(
    grid_inertia_stability,
    "Can decentralized batteries provide the same grid inertia as traditional rotating turbines?",
    resolution_mechanism("Real-world testing of virtual synchronous machines at continental scale"),
    impact("If No: The electrification 'Rope' breaks under load."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Hydrogen Fuel Cells
 * Viability: Historically seen as an alternative to Li-ion for transport.
 * Suppression: Outscaled by battery "gigafactories".
 */

% 

[Image of a solar photovoltaic cell diagram]


/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [electrification_scale_2026].
% Run tests: ?- run_tests(electrification_scale_2026_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
