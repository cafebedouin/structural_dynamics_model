% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'baseload necessity reading' of the
 *   contested kernel 'climate mitigation legitimacy.' The reading asserts
 *   that reliable decarbonization structurally requires dispatchable baseload
 *   power (nuclear) because renewables cannot provide firm capacity at scale.
 *   This claim functions as a constraint: it shapes regulatory frameworks,
 *   subsidy allocation, capacity market design, and integrated resource
 *   planning to favor nuclear new-build and existing fleet retention. The
 *   constraint has a genuine coordination function — grid reliability during
 *   decarbonization is a real collective action problem — but it also
 *   extracts asymmetrically: nuclear beneficiaries collect concentrated
 *   subsidies and guaranteed returns while costs diffuse across ratepayers,
 *   frontline communities, and excluded innovators. Active enforcement
 *   maintains the constraint through capacity market rules that disadvantage
 *   storage, licensing regimes that favor incumbent technologies, and
 *   narrative control in official scenario modeling.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '4b107df9-728f-40e6-aa5b-f1811fc49980').
narrative_ontology:cs_kernel_codification('4b107df9-728f-40e6-aa5b-f1811fc49980', formalized).
narrative_ontology:cs_authority_grounding('4b107df9-728f-40e6-aa5b-f1811fc49980', extraction).
narrative_ontology:cs_interpretation_layer_present('4b107df9-728f-40e6-aa5b-f1811fc49980').
narrative_ontology:cs_reading_relation('4b107df9-728f-40e6-aa5b-f1811fc49980', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4b107df9-728f-40e6-aa5b-f1811fc49980', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b107df9-728f-40e6-aa5b-f1811fc49980', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('4b107df9-728f-40e6-aa5b-f1811fc49980', foundational, dispatchable_baseload_physically_necessary_for_grid_stability).
narrative_ontology:cs_axiom_status(dispatchable_baseload_physically_necessary_for_grid_stability, holdable).
narrative_ontology:cs_axiom_grounding('4b107df9-728f-40e6-aa5b-f1811fc49980', dispatchable_baseload_physically_necessary_for_grid_stability, empirically_contingent).
narrative_ontology:cs_axiom('4b107df9-728f-40e6-aa5b-f1811fc49980', foundational, nuclear_uniquely_provides_zero_carbon_baseload_at_scale).
narrative_ontology:cs_axiom_status(nuclear_uniquely_provides_zero_carbon_baseload_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('4b107df9-728f-40e6-aa5b-f1811fc49980', nuclear_uniquely_provides_zero_carbon_baseload_at_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('4b107df9-728f-40e6-aa5b-f1811fc49980', post_oil_crisis_nuclear_expansion_paradigm).
narrative_ontology:cs_drift_state('4b107df9-728f-40e6-aa5b-f1811fc49980', contemporary_renewable_storage_breakthrough_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4b107df9-728f-40e6-aa5b-f1811fc49980', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, regulated_utilities_with_nuclear_assets).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, capital_intensive_infrastructure_firms).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_fuel_supply_chain).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_only_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_bearing_capital_costs).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, communities_near_nuclear_facilities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_energy_innovators).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_requires_dispatchable_generation).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, deep_decarbonization_requires_firm_zero_carbon_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, builds, and operates nuclear plants; receives construction subsidies, production tax credits, liability caps (Price-Anderson), and ratepayer-backed cost recovery. The baseload necessity narrative justifies continued public support for a technology that otherwise struggles to compete on levelized cost.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Own nuclear fleets and earn guaranteed returns on capital through rate base regulation. They influence integrated resource planning (IRP) processes to favor baseload resources they own, and resist market designs that value flexibility over capacity.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, regulated_utilities_with_nuclear_assets, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, regulated_utilities_with_nuclear_assets, agenda_setter).

% Engineering, procurement, and construction (EPC) firms and financiers that specialize in mega-projects. They benefit from the high capital concentration of nuclear new-build and the policy certainty that baseload necessity claims provide for multi-decade revenue streams.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, capital_intensive_infrastructure_firms, beneficiary,
    powerful, biographical, mobile, global).

% Uranium miners, enrichment services, fuel fabricators. Their demand depends on the committed reactor fleet; the baseload narrative secures long-term offtake agreements and strategic stockpiling policies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_fuel_supply_chain, beneficiary,
    organized, biographical, constrained, global).

% Build wind, solar, and storage. Face regulatory barriers (capacity markets that favor baseload, interconnection queues biased toward firm resources) and narrative headwinds (portrayed as unreliable). Capital access is constrained when policy signals favor nuclear.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_only_developers, payer,
    organized, biographical, constrained, global).

% Residential, commercial, and industrial electricity customers who pay for nuclear construction cost overruns, stranded asset recovery, and above-market power purchase agreements through regulated rates. No meaningful exit from monopoly service territory.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_bearing_capital_costs, payer,
    powerless, biographical, trapped, regional).

% Host communities for reactors, waste storage, and fuel cycle facilities. Bear radiological risk, emergency planning burdens, and property value impacts. Often economically dependent on the facility, creating identity lock-in that suppresses opposition.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, communities_near_nuclear_facilities, payer,
    powerless, generational, identity_locked, local).

% Developers of virtual power plants, demand response aggregators, microgrid operators, and behind-the-meter storage. Their solutions reduce the need for centralized baseload but are excluded from capacity markets and grid planning processes structured around the baseload paradigm.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_energy_innovators, excluded,
    moderate, biographical, constrained, global).

% ISOs/RTOs and NERC/regional reliability entities. They define reliability standards and capacity accreditation rules. Historically framed reliability around firm baseload; increasingly recognizing inverter-based resource capabilities but institutionally embedded in legacy frameworks.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators_and_reliability_coordinators, agenda_setter,
    institutional, biographical, analytical, continental).

% Model decarbonization pathways (IPCC, IEA, national labs). Their scenarios set the epistemic boundaries for what counts as 'feasible.' The baseload necessity reading influences which technologies appear in 'core' vs. 'marginal' roles in integrated assessment models.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures grid reliability and resource adequacy during deep decarbonization by maintaining a fleet of dispatchable, zero-carbon generators that can operate independently of weather and time of day.
% TRANSFER_FUNCTION: Moves capital risk and cost from nuclear developers and utilities to ratepayers and taxpayers via construction work in progress (CWIP) rate recovery, loan guarantees, production tax credits, liability limitation, and long-term power purchase agreements. Moves regulatory favorable treatment (capacity market rules, permitting priority) toward nuclear and away from variable renewables and demand-side resources.
% ABSENT_VOICES: Indigenous communities affected by uranium mining and milling (Navajo Nation, Northern Saskatchewan, Kazakhstan steppe); future generations who inherit waste stewardship without consent; energy justice advocates who argue reliability can be achieved through distributed resilience rather than centralized vulnerability; Global South nations pressured into nuclear agreements via climate finance conditionalities.
% DISAPPEARANCE_RATIONALE: If the baseload necessity claim disappeared overnight, capacity markets would reform to value flexibility and energy-limited resources fairly; interconnection queues would prioritize lowest-cost zero-carbon resources; public subsidies would shift from nuclear new-build to storage, transmission, and demand-side flexibility; grid planning would adopt probabilistic reliability metrics that properly value diverse portfolios. The political economy of decarbonization would reorganize around modular, rapidly deployable technologies.
% FOUNDING_PROBLEM: The 1970s oil crises created energy security imperatives that aligned with early nuclear expansion; emerging climate science in the 1980s-90s identified fossil displacement as urgent, and nuclear was the only proven zero-carbon technology operating at scale. The arrangement was built to solve: 'How to decarbonize baseload generation without sacrificing reliability or energy independence?'
% FOUNDING_PROBLEM_CORROBORATION: Grid operators (NERC, ISOs) corroborate that reliability challenges persist with high renewable penetration but increasingly cite storage, transmission, and demand response as solutions rather than exclusively nuclear. Independent system operators' renewable integration studies (CAISO, ERCOT, PJM) demonstrate reliability at 80-90% renewable penetration without new nuclear. The nuclear industry and its regulatory capture network (NRC, IAEA, national nuclear agencies) attest the problem remains live and requires nuclear. No disinterested party corroborates that nuclear is uniquely necessary.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the massive capital transfers to nuclear via CWIP, loan guarantees, PTCs, and liability caps — costs socialized while profits privatize. Suppression (0.58) captures regulatory barriers: capacity markets that pay for capacity not energy, interconnection rules that privilege firm resources, NRC licensing costs that create moats, and IRP processes that treat baseload as a fixed requirement. Theater ratio (0.42) measures the growing gap between the 'baseload necessity' narrative and the operational reality that grids increasingly run on inverter-based resources with synthetic inertia. Accessibility collapse (0.62) reflects how thoroughly the baseload frame closes off imagination of alternative reliability architectures. Resistance (0.67) captures sustained opposition from renewable industries, consumer advocates, environmental justice groups, and increasingly, grid operators themselves.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear beneficiary seats, the constraint appears as genuine coordination: they built the fleet, they maintain reliability, they deserve cost recovery. From the payer seats (ratepayers, communities, renewable developers), the same structure operates as enforced extraction: a technology that cannot compete on merits uses regulatory capture to lock in revenue streams. The engine computes this divergence from the structural data — the declared beneficiaries, victims, power levels, and exit options produce different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry and regulated utilities are structural beneficiaries (d near 0.0): they collect subsidies, set agendas via IRP, and control the narrative through institutional channels. Ratepayers and frontline communities are structural targets (d near 1.0): trapped by monopoly service territory and identity lock-in, bearing costs without voice. Renewable developers and distributed innovators are constrained payers (d ~0.7): they face regulatory headwinds but retain some market access. Grid operators sit near symmetric (d ~0.5): they genuinely need reliability but are institutionally embedded in the baseload paradigm. Climate analysts are analytical observers (d ~0.5) but their scenario choices shape the epistemic boundary conditions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s energy security + early climate urgency) has been substantially overtaken by technological change: solar/wind/storage costs have dropped 80-90%, grid integration capabilities have advanced, and demand-side flexibility has proven scalable. Yet the arrangement persists and expands (new SMR subsidies, existing fleet credits). This is mandatrophy: the mandate ('we need nuclear for baseload') has outlived its function (reliable zero-carbon power is now available cheaper and faster via portfolios), but the constraint persists through institutional inertia and the capital concentration it created. The coordination story is real but the extraction has become primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the baseload necessity claim a reading of the contested kernel ''climate mitigation legitimacy,'' or does it represent the kernel itself?',
    'Trace the genealogy: the kernel is the legitimating commitment that ''climate mitigation requires certain energy system structures.'' The baseload necessity reading is one instantiation. The other readings (renewable_primacy, portfolio_pragmatism, degrowth_sufficiency) are competing instantiations. This omega records the framing uncertainty.',
    'If treated as the kernel itself, the constraint appears as a natural law of energy physics. If treated as a reading, its extraction becomes visible as a contested political-economic claim. Classification shifts from mountain/tangled_rope ambiguity to clear tangled_rope with identified beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether this constraint is a reading of a contested kernel or the kernel itself.').

omega_variable(
    baseload_technical_necessity,
    'Is dispatchable baseload physically necessary for grid reliability at high renewable penetration, or is this a sociotechnical construct of legacy planning paradigms?',
    'Empirical: grids operating at 80%+ renewable penetration (South Australia, Denmark, California spring days) with no baseload and no blackouts. Modeling: NREL''s 100% clean electricity studies showing reliability via storage, transmission, hydrogen, demand response. If physical necessity is falsified, the coordination function collapses and the constraint becomes pure snare.',
    'If baseload is not physically necessary, the constraint''s coordination function is a cover story — extraction is the primary function. Classification shifts from tangled_rope to snare. If necessary, the tangled_rope classification holds with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_technical_necessity, empirical, 'Physical necessity of baseload generation for grid reliability.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternatives structural (regulatory barriers, market rules) or internalized (industry and regulator belief that baseload is physically necessary)?',
    'Post-exit suppression trajectory: if jurisdictions that reform capacity markets and interconnection rules see rapid renewable+storage deployment without reliability loss, suppression was primarily structural. If reformed jurisdictions still under-invest in alternatives due to planner beliefs, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists even after formal barriers are removed because the agents carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of renewable-only pathways.').

omega_variable(
    capital_concentration_persistence,
    'Does the high capital concentration of nuclear create a self-reinforcing political economy that persists regardless of technological necessity?',
    'Track subsidy flows and regulatory outcomes after cost crossover: if nuclear continues receiving preferential treatment (CWIP, PTCs, capacity payments) after LCOE parity with renewables+storage, capital concentration drives persistence independent of coordination function.',
    'If capital concentration drives persistence, the constraint exhibits piton dynamics — atrophied coordination function maintained by inertial political economy. Theater ratio would rise over time as coordination function decays.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_concentration_persistence, empirical, 'Whether capital lock-in sustains the constraint beyond its coordination justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_tr_t9, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_tr_t18, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_tr_t27, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 27, 0.36).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_tr_t36, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 36, 0.39).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_tr_t45, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 45, 0.41).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_tr_t54, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 54, 0.42).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_be_t9, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_be_t18, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_be_t27, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 27, 0.62).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_be_t36, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 36, 0.65).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_be_t45, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 45, 0.67).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_be_t54, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 54, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_su_t9, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 9, 0.51).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_su_t18, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_su_t27, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 27, 0.55).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_su_t36, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 36, 0.56).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_su_t45, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 45, 0.57).
narrative_ontology:measurement(climate_mitigation_legitimacy__baseload_necessity_reading_su_t54, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 54, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__baseload_necessity_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_liability_cap_subsidy).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, capacity_market_baseload_bias).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, integrated_resource_planning_baseload_requirement).

% DUAL FORMULATION NOTE:
% This constraint is the baseload_necessity_reading of the climate_mitigation_legitimacy kernel. It decomposes the natural-language concept 'climate mitigation requires baseload' from the sibling readings. The epsilon values differ substantially: this reading has high extraction (0.68) due to nuclear subsidies and ratepayer burden; renewable_primacy_reading has lower extraction (renewables compete on merit); portfolio_pragmatism_reading has moderate extraction (technology-neutral but still favors incumbents via transition costs); degrowth_sufficiency_reading has near-zero extraction (demand reduction avoids generation expansion). They are linked through the kernel's shared legitimating commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
