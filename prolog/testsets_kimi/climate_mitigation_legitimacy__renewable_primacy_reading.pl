% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy Decarbonization Constraint
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the renewable_primacy_reading of the
 *   contested climate_mitigation_legitimacy kernel. The claim that renewables
 *   plus storage can fully decarbonize faster and cheaper than nuclear has
 *   been institutionalized in green taxonomies, subsidy regimes, and
 *   technology-specific targets across multiple jurisdictions. It operates as
 *   both a coordination mechanism (directing collective investment toward
 *   scalable low-carbon technologies) and an extractive structure (actively
 *   suppressing nuclear competition and stranding baseload assets). KEY
 *   AGENTS (by structural relationship): - utility_scale_renewable_developers
 *   and battery_storage_vendors: Primary beneficiaries (powerful/mobile) â
 *   collect redirected capital and policy support. - nuclear_fleet_operators:
 *   Primary target (institutional/trapped) â bears extraction through
 *   exclusion from finance and taxonomy. - centralized_baseload_utilities:
 *   Secondary payer (institutional/constrained) â absorbs stranded asset
 *   costs and reliability obligations. - renewable_policy_alliance:
 *   Agenda-setter (institutional/constrained) â administers taxonomy and
 *   target rules but is structurally captured by the coalition. -
 *   transmission_system_operators: Payer (institutional/constrained) â
 *   bears integration costs. - independent_grid_modelers: Analytical observer
 *   â sees full structure but is contested by all sides.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.71).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy Decarbonization Constraint").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, 'fc63df69-5480-4639-975c-ba5a1acc73ce').
narrative_ontology:cs_kernel_codification('fc63df69-5480-4639-975c-ba5a1acc73ce', distributed).
narrative_ontology:cs_authority_grounding('fc63df69-5480-4639-975c-ba5a1acc73ce', diffuse_epistemic).
narrative_ontology:cs_reading_relation('fc63df69-5480-4639-975c-ba5a1acc73ce', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('fc63df69-5480-4639-975c-ba5a1acc73ce', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('fc63df69-5480-4639-975c-ba5a1acc73ce', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('fc63df69-5480-4639-975c-ba5a1acc73ce', foundational, renewable_sufficiency_without_nuclear).
narrative_ontology:cs_axiom_status(renewable_sufficiency_without_nuclear, holdable).
narrative_ontology:cs_axiom_grounding('fc63df69-5480-4639-975c-ba5a1acc73ce', renewable_sufficiency_without_nuclear, empirically_contingent).
narrative_ontology:cs_axiom('fc63df69-5480-4639-975c-ba5a1acc73ce', foundational, distributed_generation_preference).
narrative_ontology:cs_axiom_status(distributed_generation_preference, holdable).
narrative_ontology:cs_axiom_grounding('fc63df69-5480-4639-975c-ba5a1acc73ce', distributed_generation_preference, empirically_contingent).
narrative_ontology:cs_reference_frame('fc63df69-5480-4639-975c-ba5a1acc73ce', distributed_renewable_transition).
narrative_ontology:cs_drift_state('fc63df69-5480-4639-975c-ba5a1acc73ce', renewable_scaling_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc63df69-5480-4639-975c-ba5a1acc73ce', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, utility_scale_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_vendors).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_solar_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_fleet_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, centralized_baseload_utilities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, transmission_system_operators).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_learning_curve_dominance).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_preference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive preferential grid access, feed-in tariffs, and green taxonomy eligibility that privilege wind and solar deployment. Their competitive position depends on continued policy discrimination against nuclear and fossil baseload alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, utility_scale_renewable_developers, beneficiary,
    powerful, biographical, mobile, global).

% Positioned as the necessary complement to variable renewables; receive public R&D funding, procurement mandates, and capacity-market revenues contingent on renewable primacy policy frameworks.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Advocacy coalitions and energy cooperatives promoting rooftop solar and prosumer ownership models. Benefit from regulatory frameworks that distribute generation privileges away from centralized utilities.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_solar_advocates, beneficiary,
    organized, generational, constrained, national).

% Existing nuclear fleets face early retirement pressure and exclusion from green financing. Regulatory frameworks reclassify nuclear as non-renewable, denying access to clean energy mandates and carbon accounting standards that define legitimate decarbonization pathways. Sunk capital cannot be redeployed outside regulated electricity markets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_fleet_operators, payer,
    institutional, generational, trapped, national).

% Traditional utilities with sunk costs in centralized dispatchable generation see asset values devalued by policies favoring variable renewable output. Must absorb grid integration costs and reliability obligations while renewable entrants receive priority dispatch and subsidized market entry.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, centralized_baseload_utilities, payer,
    institutional, generational, constrained, national).

% International and national bodies that set technology-specific targets, green taxonomy criteria, and subsidy allocation rules. Administers the constraint by defining renewable-plus-storage as the legitimate decarbonization pathway and excluding nuclear from qualifying investment categories.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_policy_alliance, agenda_setter,
    institutional, generational, constrained, global).

% Tasked with maintaining grid reliability while integrating high shares of variable renewables. Bears the technical and cost burden of frequency regulation, backup procurement, and network reinforcement that renewable primacy policies mandate but systematically underfund relative to generation subsidies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, transmission_system_operators, payer,
    institutional, biographical, constrained, national).

% Produce integrated assessment and grid adequacy studies cited by all sides. Their technical conclusions about renewable sufficiency at high penetration rates are politically contested and selectively deployed to support competing technology claims.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, independent_grid_modelers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs collective decarbonization investment toward technologies with observed declining cost curves and shorter deployment timelines, solving the coordination failure of fragmented climate investment and avoiding nuclear cost overruns and proliferation governance bottlenecks.
% TRANSFER_FUNCTION: Moves subsidy flows, grid access priority, green taxonomy eligibility, and private capital from nuclear and centralized baseload portfolios to renewable generation and battery storage; shifts reliability and integration costs to transmission system operators and ratepayers.
% ABSENT_VOICES: Nuclear engineers and plant communities facing fleet retirement; baseload-dependent industrial users concerned about intermittency; energy poverty advocates in regions where renewable buildout lags demand growth; long-duration storage skeptics noting unproven seasonal reliability.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, capital would reallocate toward nuclear new-build and existing fleet extensions, renewable subsidy frameworks would compress or neutralize, green taxonomies would reopen to nuclear, and grid planning would revert toward centralized dispatchable portfolios.
% FOUNDING_PROBLEM: The need to decarbonize electricity generation rapidly while avoiding the historical cost overruns, construction delays, and geopolitical proliferation risks associated with nuclear power expansion.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and renewable industry analysts attest to the urgency from within the beneficiary set. Independent energy-systems modelers and nuclear regulatory bodies outside the beneficiary set contest whether excluding nuclear accelerates or delays decarbonization; grid reliability studies from transmission operators provide mixed corroboration on the sufficiency claim.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects substantial policy-driven redirection of capital and grid access away from nuclear toward renewables, though not total rent capture. Suppression (0.71) is higher because the constraint's persistence depends on actively excluding nuclear from green taxonomies and public finance â a structure that would not hold without enforcement. Theater ratio (0.38) captures the growing gap between 100% renewable pledges and actual storage deployment timelines. Accessibility collapse (0.68) is high because once the policy framework establishes renewable primacy, nuclear alternatives become politically and financially inaccessible. Resistance (0.58) reflects organized pushback from nuclear operators, some utilities, and pro-nuclear states. Temporal measurements show monotonic rises on a single shared grid, tracking the maturation of enforcement machinery (taxonomies, subsidy allocation) and accumulating extraction over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (renewable developers, storage vendors) experience the constraint as genuine coordination that solves investment uncertainty and accelerates deployment. The payer seats (nuclear operators, baseload utilities) experience the same structure as extractive displacement that strands viable assets and loads integration costs onto their balance sheets. The agenda-setter seat (policy alliance) experiences it as necessary climate governance. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (renewable developers, storage vendors, distributed advocates) receive low directionality values because the constraint subsidizes their market position through policy preference and finance access. Victims (nuclear operators, baseload utilities) receive high directionality because the constraint extracts from them through exclusion and cost-shifting. The renewable_policy_alliance has moderate directionality: it is not the ultimate recipient of extracted value but is structurally captured by the coalition it administers. Transmission operators sit near symmetric but slightly target-side because they bear uncompensated integration costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this constraint as pure extraction (snare) because it does solve a genuine coordination problem: directing fragmented decarbonization investment toward deployable technologies with declining costs. It also prevents mislabeling as pure coordination (rope) because the asymmetric victim set and active suppression of nuclear reveal extraction layered onto the coordination function. The temporal measurements show extraction accumulation over the interval, suggesting the coordination function may be partially supplanted by rent maintenance, but the base structure retains both faces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_primacy_kernel_position,
    'This constraint instantiates the renewable_primacy_reading of the climate_mitigation_legitimacy kernel. How would classification change under the portfolio_pragmatism_reading or baseload_necessity_reading?',
    'Comparative analysis of sibling constraint stories within the same kernel family.',
    'Under portfolio_pragmatism_reading, nuclear shifts from victim to beneficiary and extractiveness redistributes across the technology portfolio; under baseload_necessity_reading, the renewable sufficiency claim collapses as a false coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_primacy_kernel_position, conceptual, 'Kernel reading position and structural alternatives').

omega_variable(
    storage_scale_reliability,
    'Does utility-scale and long-duration storage achieve the cost and reliability necessary to validate the renewable sufficiency claim at high grid penetration?',
    'Empirical tracking of LDES deployment, grid reliability statistics, and system-level LCOE in jurisdictions with high renewable share.',
    'If storage fails to scale, the coordination function is undermined and extraction from nuclear becomes pure opportunity cost without compensating system benefit, pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_scale_reliability, empirical, 'Empirical viability of storage at renewable primacy scale').

omega_variable(
    enforcement_vs_market_driver,
    'Is nuclear suppression driven by active policy enforcement, or by genuine market cost convergence favoring renewables?',
    'Counterfactual analysis of nuclear financing access under technology-neutral subsidy and taxonomy regimes.',
    'If suppression is mostly market-driven, the constraint approaches rope; if policy-enforced exclusion is doing the structural work, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_market_driver, empirical, 'Policy enforcement versus market cost driver ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel, decomposed per the Îµ-invariance principle because the natural-language concept 'decarbonization pathway' conflates multiple structurally distinct technology commitments with different beneficiary/victim structures and Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
