% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy as Legitimacy Gate for Climate Mitigation
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The renewable primacy reading asserts that renewables plus storage can
 *   achieve full decarbonization faster and cheaper than nuclear, positioning
 *   nuclear as a capital-intensive distraction that delays the transition.
 *   This reading has gained institutional dominance in climate policy since
 *   roughly 2015, shaping investment criteria, permitting frameworks, and
 *   subsidy allocation. The constraint operates as a legitimacy gate:
 *   projects and policies aligned with renewable primacy access capital and
 *   political support; nuclear projects face escalating regulatory hurdles,
 *   financing exclusion, and narrative marginalization. The coordination
 *   function is real — modular, short-lead-time technologies do deploy faster
 *   and benefit from steeper learning curves. The extraction is also real —
 *   nuclear-dependent communities, ratepayers, and grid operators bear
 *   stranded asset risk, employment loss, and firm capacity gaps without
 *   proportional compensation. The constraint requires active enforcement
 *   through policy mechanisms (tax credits structured for renewables, nuclear
 *   exclusion from clean energy standards, permitting asymmetry).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.45).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy as Legitimacy Gate for Climate Mitigation").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '900ff526-f11a-4204-8ada-ad995147d2f0').
narrative_ontology:cs_kernel_codification('900ff526-f11a-4204-8ada-ad995147d2f0', distributed).
narrative_ontology:cs_authority_grounding('900ff526-f11a-4204-8ada-ad995147d2f0', extraction).
narrative_ontology:cs_interpretation_layer_present('900ff526-f11a-4204-8ada-ad995147d2f0').
narrative_ontology:cs_reading_relation('900ff526-f11a-4204-8ada-ad995147d2f0', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('900ff526-f11a-4204-8ada-ad995147d2f0', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('900ff526-f11a-4204-8ada-ad995147d2f0', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('900ff526-f11a-4204-8ada-ad995147d2f0', foundational, modular_deployment_speed_outranks_firm_capacity_value).
narrative_ontology:cs_axiom_status(modular_deployment_speed_outranks_firm_capacity_value, holdable).
narrative_ontology:cs_axiom_grounding('900ff526-f11a-4204-8ada-ad995147d2f0', modular_deployment_speed_outranks_firm_capacity_value, empirically_contingent).
narrative_ontology:cs_axiom('900ff526-f11a-4204-8ada-ad995147d2f0', foundational, learning_curve_dynamics_justify_technology_discrimination).
narrative_ontology:cs_axiom_status(learning_curve_dynamics_justify_technology_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('900ff526-f11a-4204-8ada-ad995147d2f0', learning_curve_dynamics_justify_technology_discrimination, empirically_contingent).
narrative_ontology:cs_axiom('900ff526-f11a-4204-8ada-ad995147d2f0', secondary, nuclear_capital_intensity_is_systemic_drag_on_transition).
narrative_ontology:cs_axiom_status(nuclear_capital_intensity_is_systemic_drag_on_transition, holdable).
narrative_ontology:cs_axiom_grounding('900ff526-f11a-4204-8ada-ad995147d2f0', nuclear_capital_intensity_is_systemic_drag_on_transition, empirically_contingent).
narrative_ontology:cs_reference_frame('900ff526-f11a-4204-8ada-ad995147d2f0', technology_neutral_urgency_framing).
narrative_ontology:cs_drift_state('900ff526-f11a-4204-8ada-ad995147d2f0', post_ira_2022_deployment_acceleration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('900ff526-f11a-4204-8ada-ad995147d2f0', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_policy_makers_prioritizing_rapid_deployment).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, storage_technology_investors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, utility_ratepayers_in_nuclear_regions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, communities_dependent_on_nuclear_employment).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_requiring_firm_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_requiring_firm_capacity).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, levelized_cost_of_energy_methodology_favors_short_lead_time_technologies).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, learning_curve_dynamics_favor_modular_technologies).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_enhances_grid_resilience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives deployment subsidies, tax equity financing, streamlined permitting, and market access through clean energy standards that favor their technology characteristics. Can pivot across jurisdictions and technology subtypes (solar, wind, storage). Capital follows policy signals globally.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_industry, beneficiary,
    organized, biographical, mobile, global).

% Capital allocators targeting battery and long-duration storage deployment. Benefit from renewable primacy because storage value scales with renewable penetration. High capital mobility across energy transition themes.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, storage_technology_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% NGOs, community groups, and policy entrepreneurs who frame distributed generation as resilience, equity, and democratic control. Their identity and funding are fused to the distributed energy paradigm; exit means abandoning the framing that gives them standing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_advocates, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_advocates, agenda_setter).

% Government officials, legislators, and regulators who design clean energy standards, tax credit structures, and permitting regimes. They benefit politically from visible rapid deployment but are constrained by institutional mandates and electoral cycles; cannot easily reverse course without admitting policy error.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_policy_makers_prioritizing_rapid_deployment, agenda_setter,
    institutional, biographical, constrained, national).

% Vertically integrated utilities, reactor vendors, and fuel cycle companies with multi-decade asset horizons. Face exclusion from clean energy frameworks, financing restrictions, and narrative marginalization. Exit options limited by sunk capital, regulatory licenses, and specialized workforce — cannot pivot to renewables without abandoning core competencies and stranded assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry, payer,
    powerful, generational, constrained, global).

% Residential, commercial, and industrial electricity customers in regions with existing nuclear fleets. Pay for renewable deployment through rates while nuclear assets they already paid for are depreciated early. Limited exit: cannot choose generation mix, face regulatory capture in rate cases, geographic mobility is costly.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, utility_ratepayers_in_nuclear_regions, payer,
    moderate, biographical, constrained, regional).

% Towns and regions where nuclear plants provide high-wage employment, tax base, and community identity. Bear transition costs (job loss, tax base erosion, social disruption) without proportional transition funding. Geographically trapped; skills are specialized; political voice is diluted in state/national policy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, communities_dependent_on_nuclear_employment, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, communities_dependent_on_nuclear_employment, excluded).

% ISOs, RTOs, and vertically integrated utilities responsible for reliability. Gain operational tools from distributed resources and storage but lose firm capacity assurance as nuclear retires. Mandated to maintain reliability but constrained by resource adequacy frameworks that undervalue firm capacity. Cannot exit the reliability obligation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_requiring_firm_capacity, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_requiring_firm_capacity, beneficiary).

% Researchers who argue that renewables+storage cannot reliably meet decarbonization targets at required speed/scale without nuclear or carbon capture. Excluded from dominant policy framing and funding streams aligned with renewable primacy. Can publish and advocate but face citation and funding penalties for challenging the consensus.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_scientists_contesting_sufficiency, excluded,
    moderate, biographical, mobile, global).

% Labor, environmental justice, and community organizations tracking whether transition policies address nuclear community impacts. Analyze the constraint's distributional effects but do not set its agenda or directly bear its costs.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, just_transition_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the urgent decarbonization coordination problem by aligning capital, policy, and deployment around the fastest-scaling low-carbon technologies (modular renewables and storage with steep learning curves and short lead times), avoiding the multi-decade lock-in of capital-intensive baseload.
% TRANSFER_FUNCTION: Moves capital, policy support, and market access from nuclear and centralized generation toward renewable and storage deployment; moves stranded asset risk and transition costs onto nuclear-dependent communities and ratepayers; moves firm capacity responsibility onto grid operators without commensurate resource adequacy compensation.
% ABSENT_VOICES: Nuclear-dependent communities (geographically trapped, politically diluted), ratepayers in nuclear regions (no choice in generation mix), grid reliability engineers (institutional obligation without resource authority), and climate scientists contesting renewable sufficiency (excluded from dominant funding/framing). These voices would object to the speed and asymmetry of the transition but are not seated at the policy table.
% DISAPPEARANCE_RATIONALE: If the renewable primacy constraint vanished overnight, clean energy standards would become technology-neutral, nuclear would regain access to financing and permitting parity, capacity markets would value firm capacity explicitly, and capital would reallocate across the full low-carbon portfolio. The global decarbonization trajectory would shift toward a portfolio path with higher nuclear share, slower initial deployment but potentially higher ultimate reliability. Nuclear communities would avoid abrupt transition; ratepayers would see different cost allocation.
% FOUNDING_PROBLEM: The climate urgency problem: decarbonization must happen at unprecedented speed, and the only technologies capable of deploying at the required pace within the critical 2020-2035 window are modular renewables and storage with short lead times and steep learning curves. Nuclear's multi-decade construction cycles and capital intensity make it too slow for the urgency.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (outside the renewable industry) validates urgency but also notes nuclear's role in most 1.5C pathways. IEA Net Zero by 2050 (institutional, not industry) models substantial nuclear expansion alongside renewables. Nuclear industry and labor unions contest that urgency requires exclusion. The founding problem (urgency) is live; the claim that nuclear cannot contribute to urgent deployment is contested by major institutional analyses outside the renewable beneficiary set.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the capital redistribution from nuclear to renewables/storage enforced through policy mechanisms that privilege the latter's cost structure. Suppression (0.45) is moderate — nuclear is not banned but is structurally disadvantaged in financing, permitting, and market access. Theater ratio (0.32) captures the growing gap between 'technology-neutral' policy rhetoric and the de facto renewable primacy embedded in deployment incentives. Accessibility collapse (0.58) reflects that alternative decarbonization pathways (nuclear-heavy, degrowth) remain technically viable but are institutionally marginalized. Resistance (0.55) comes from nuclear industry advocacy, labor unions, grid reliability organizations, and a subset of climate scientists who contest the sufficiency claim.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable industry seat, the constraint is genuine coordination solving the climate urgency problem with the fastest available tools. From the nuclear industry seat, it is targeted extraction using climate urgency as cover to redirect capital. From the grid operator seat, it is a tangled coordination problem where the fastest deployment creates new reliability challenges. From the ratepayer seat, it is a wealth transfer with uncertain net benefit. The engine computes these seat-level types from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable industry and storage investors are structural beneficiaries (d near 0.0-0.2) — they collect deployment subsidies, tax equity, and market share. Distributed generation advocates benefit from policy frameworks that privilege their model. Nuclear industry and dependent communities are targets (d near 0.8-0.95) — they bear stranded asset costs, employment transition burdens, and exclusion from clean energy frameworks. Grid operators sit near symmetric (d ~0.5) — they gain operational flexibility from distributed resources but lose firm capacity assurance. Ratepayers in nuclear regions are net payers (d ~0.7) — they fund renewable deployment while nuclear assets depreciate. Climate policymakers prioritizing rapid deployment are agenda_setters (d ~0.15) — they design the constraint and benefit from its perceived success.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — urgent decarbonization with the fastest available technology — remains live (climate urgency has increased). However, the constraint's extraction profile has grown as renewable deployment has matured: the coordination function (rapid deployment) is being achieved, but the extraction (nuclear exclusion, ratepayer burden, firm capacity gap) persists and intensifies. This suggests the constraint is drifting from tangled_rope toward snare if the coordination function saturates while extraction continues. The mandatrophy question is whether the constraint's enforcement machinery will relax as renewable+storage costs fall below nuclear, or whether the institutional coalition benefiting from renewable primacy will maintain the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint one reading of the contested kernel ''climate_mitigation_legitimacy'', and what would sibling readings change structurally?',
    'Comparative analysis of the four declared readings'' beneficiary/victim structures, coordination functions, and founding problems to map the kernel''s constraint family.',
    'If readings are distinct constraints, each requires its own story with independent ε; if they are measurement variants of one constraint, ε-invariance is violated and the framework''s core principle fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint instantiates the renewable_primacy_reading of the climate_mitigation_legitimacy kernel; sibling readings are baseload_necessity_reading, portfolio_pragmatism_reading, and degrowth_sufficiency_reading.').

omega_variable(
    nuclear_as_capital_sink_extraction,
    'Does nuclear power function as a capital sink that structurally delays renewable deployment, or does it provide complementary firm capacity that enables higher renewable penetration?',
    'Integrated resource planning studies with technology-neutral optimization under varying cost trajectories for renewables, storage, and nuclear; historical analysis of deployment sequences in jurisdictions with different policy frameworks.',
    'If nuclear is a capital sink, its inclusion in the victim set is structurally correct and extraction from nuclear-dependent communities is a direct cost of the renewable primacy constraint; if complementary, the constraint''s victim set misidentifies coordination partners as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_as_capital_sink_extraction, empirical, 'Structural relationship between nuclear capital intensity and renewable deployment velocity.').

omega_variable(
    storage_sufficiency_at_scale,
    'Can storage technologies at projected cost curves provide sufficient firm capacity to replace baseload generation at continental scale?',
    'Multi-decade grid reliability modeling with stochastic renewable profiles, storage degradation dynamics, and correlated weather events; comparison against historical nuclear capacity factors.',
    'If storage cannot scale to provide seasonal firm capacity, the renewable primacy constraint''s coordination function is overstated and its extractiveness on nuclear-dependent systems is higher than claimed; if sufficient, the constraint''s coordination claim is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(storage_sufficiency_at_scale, empirical, 'Technical feasibility of storage as full baseload substitute at continental scale.').

omega_variable(
    distributed_generation_privilege_as_coordination,
    'Does privileging distributed generation solve a genuine coordination problem (resilience, equity, land use) or does it extract from centralized generation stakeholders without commensurate system benefit?',
    'Cost-benefit analysis of distributed vs. centralized generation at system level including transmission, land use, resilience metrics, and equity outcomes across multiple grid architectures.',
    'If distributed generation privilege is extractive cover, the constraint moves toward snare; if genuine coordination, it supports tangled_rope classification with real beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_generation_privilege_as_coordination, conceptual, 'Whether distributed generation preference serves coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 2010, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(clim_tr_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2030, 0.31).
narrative_ontology:measurement(clim_tr_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2035, 0.32).

% Extraction over time
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(clim_be_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2025, 0.63).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2030, 0.66).
narrative_ontology:measurement(clim_be_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(clim_su_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2030, 0.44).
narrative_ontology:measurement(clim_su_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2035, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__renewable_primacy_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_market_design__capacity_mechanism_reform).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_standards__firm_capacity_definition).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, just_transition_policy__nuclear_community_support).

% DUAL FORMULATION NOTE:
% This constraint is one member of the climate_mitigation_legitimacy constraint family. The four readings share the kernel (climate mitigation legitimacy) but instantiate distinct constraints with different beneficiary/victim structures, coordination functions, and ε values. The renewable_primacy_reading privileges modular short-lead-time technologies and treats nuclear as a capital sink. The baseload_necessity_reading treats firm capacity as the coordination function and renewables as the extractive variable. The portfolio_pragmatism_reading treats technology neutrality as the coordination function and policy bias as extraction. The degrowth_sufficiency_reading treats demand reduction as the coordination function and all supply-side expansion as extraction. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__renewable_primacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__renewable_primacy_reading, powerful, 0.85).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__renewable_primacy_reading, organized, 0.65).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__renewable_primacy_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
