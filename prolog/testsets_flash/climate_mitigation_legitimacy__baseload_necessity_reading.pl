% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity for Decarbonization
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint asserts that reliable decarbonization fundamentally
 *   requires dispatchable baseload power, which intermittent renewables
 *   cannot provide at scale. This framing positions technologies like nuclear
 *   power and fossil fuels with carbon capture and storage (CCS) as
 *   indispensable, while renewable-only pathways are deemed insufficient. It
 *   influences energy policy, investment, and regulatory frameworks, often
 *   leading to subsidies or preferential treatment for baseload technologies.
 *
 * KEY AGENTS:
 *   - nuclear_power_industry: Primary beneficiary (institutional/arbitrage) — benefits from policy support and investment.
 *   - fossil_fuel_industry_with_ccs: Secondary beneficiary (institutional/constrained) — benefits from continued relevance through CCS.
 *   - grid_operators: Beneficiary (institutional/constrained) — benefits from perceived grid stability and reduced complexity.
 *   - renewable_energy_advocates: Primary victim (organized/constrained) — faces policy barriers and reduced investment for renewable-only pathways.
 *   - taxpayers_subsidizing_baseload: Victim (powerless/constrained) — bears the cost of subsidies for baseload technologies.
 *   - climate_scientists: Observer (analytical/analytical) — provides data on climate change and energy systems, but does not directly benefit or pay.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.6).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.4).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity for Decarbonization").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '6f75f350-c3fa-451c-9112-3a72571289d6').
narrative_ontology:cs_kernel_codification('6f75f350-c3fa-451c-9112-3a72571289d6', implicit).
narrative_ontology:cs_authority_grounding('6f75f350-c3fa-451c-9112-3a72571289d6', extraction).
narrative_ontology:cs_interpretation_layer_present('6f75f350-c3fa-451c-9112-3a72571289d6').
narrative_ontology:cs_reading_relation('6f75f350-c3fa-451c-9112-3a72571289d6', climate_mitigation_legitimacy__renewable_primacy_reading, influences).
narrative_ontology:cs_reading_relation('6f75f350-c3fa-451c-9112-3a72571289d6', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('6f75f350-c3fa-451c-9112-3a72571289d6', climate_mitigation_legitimacy__degrowth_sufficiency_reading, forecloses).
narrative_ontology:cs_axiom('6f75f350-c3fa-451c-9112-3a72571289d6', foundational, grid_stability_requires_dispatchable_baseload).
narrative_ontology:cs_axiom_status(grid_stability_requires_dispatchable_baseload, holdable).
narrative_ontology:cs_axiom_grounding('6f75f350-c3fa-451c-9112-3a72571289d6', grid_stability_requires_dispatchable_baseload, empirically_contingent).
narrative_ontology:cs_axiom('6f75f350-c3fa-451c-9112-3a72571289d6', foundational, renewables_cannot_provide_baseload_at_scale).
narrative_ontology:cs_axiom_status(renewables_cannot_provide_baseload_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('6f75f350-c3fa-451c-9112-3a72571289d6', renewables_cannot_provide_baseload_at_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('6f75f350-c3fa-451c-9112-3a72571289d6', traditional_grid_architecture).
narrative_ontology:cs_drift_state('6f75f350-c3fa-451c-9112-3a72571289d6', contemporary_energy_transition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6f75f350-c3fa-451c-9112-3a72571289d6', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_industry_with_ccs).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, taxpayers_subsidizing_baseload).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it presents a genuine coordination problem (reliable grid stability during decarbonization) but couples it with asymmetric extraction. The extraction comes from prioritizing capital-intensive, long-lived baseload assets, often requiring public subsidies, over potentially cheaper but intermittent alternatives. Suppression (0.4) is moderate, reflecting policy and regulatory hurdles for renewable-only solutions. Extractiveness (0.6) is substantial due to the high capital concentration and long-term commitments to specific technologies. Theater ratio (0.2) is low, as the concern for grid stability is real, but the 'necessity' argument often serves to maintain incumbent industry positions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of baseload industry and grid operators, this constraint is a necessary technical truth for grid stability, ensuring reliable power during the energy transition. From the perspective of renewable advocates and taxpayers, it is a policy choice that entrenches incumbent technologies, extracts rents through subsidies, and slows down a full transition to renewables. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear and fossil fuel (with CCS) industries are clear beneficiaries (d near 0.0) as the constraint channels investment and policy support towards them. Grid operators also benefit from the perceived stability. Renewable energy advocates and taxpayers are victims (d near 1.0) as their preferred solutions face structural disadvantages and they bear the costs of baseload subsidies. The 'necessity' argument acts as a powerful lever to direct resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to ensure reliable decarbonization. However, if renewable technologies with storage prove capable of providing grid stability at scale, the 'baseload necessity' mandate could become a form of mandatrophy, where the constraint persists due to institutional inertia and the beneficiaries' interest, rather than genuine technical need. The current classification as Tangled Rope reflects this potential for function-creep and rent-seeking within a legitimate coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity_empirical_validity,
    'Is the claim that renewables cannot provide dispatchable baseload power at scale empirically sound, considering advancements in storage and grid management?',
    'Long-term empirical data from grids with high renewable penetration and advanced storage solutions; independent engineering and economic modeling.',
    'If proven false, the constraint shifts from a ''natural'' necessity to a policy choice, increasing its extractiveness and suppression metrics, potentially reclassifying it as a Snare. If proven true, it reinforces the Mountain-like aspects of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_validity, empirical, 'Empirical validity of baseload necessity claim.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''baseload necessity'' or a framing used by incumbent industries to secure market share in decarbonization efforts?',
    'Analysis of lobbying efforts, policy outcomes, and financial flows to baseload vs. renewable sectors; expert consensus on grid stability requirements.',
    'If it''s a framing, the constraint''s true extractiveness is higher, and its claimed coordination function is a cover for rent-seeking. This would shift its classification towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''baseload_necessity_reading'' of the ''climate_mitigation_legitimacy'' kernel. Sibling readings like ''renewable_primacy_reading'' (renewables + storage suffice) and ''degrowth_sufficiency_reading'' (demand reduction makes large-scale generation unnecessary) would lead to different beneficiary/victim sets and policy prescriptions. The ''portfolio_pragmatism_reading'' (technology-neutral portfolio) would broaden the beneficiary set to include both nuclear and renewables without prioritizing baseload.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, energy_market_design_rules).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_subsidies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_legitimacy' kernel. Other readings (renewable_primacy_reading, portfolio_pragmatism_reading, degrowth_sufficiency_reading) offer alternative structural claims about the path to decarbonization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
