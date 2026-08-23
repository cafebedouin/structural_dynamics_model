% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Renewable Primacy as Legitimate Decarbonization Pathway
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The renewable primacy reading asserts that a fully decarbonized
 *   electricity system can be built faster and cheaper using variable
 *   renewables (wind, solar) plus storage than by relying on nuclear power.
 *   This reading shapes policy in the EU, California, Germany, and elsewhere,
 *   directing trillions in capital toward renewables while excluding nuclear
 *   from green taxonomies and clean energy standards. The constraint is the
 *   policy regime that institutionalizes this reading: renewable portfolio
 *   standards, storage mandates, nuclear phase-outs, and subsidy structures.
 *   It coordinates investment (rope function) but extracts from
 *   nuclear-dependent communities and industries (snare function), requiring
 *   active enforcement to maintain the exclusion of nuclear from 'clean'
 *   categories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.45).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy as Legitimate Decarbonization Pathway").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '9e034d97-2b98-4ef9-a07d-a3ffa9aceedf').
narrative_ontology:cs_kernel_codification('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', distributed).
narrative_ontology:cs_authority_grounding('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', extraction).
narrative_ontology:cs_interpretation_layer_present('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf').
narrative_ontology:cs_reading_relation('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', foundational, renewable_speed_cost_primacy).
narrative_ontology:cs_axiom_status(renewable_speed_cost_primacy, holdable).
narrative_ontology:cs_axiom_grounding('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', renewable_speed_cost_primacy, empirically_contingent).
narrative_ontology:cs_axiom('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', secondary, distributed_generation_preference).
narrative_ontology:cs_axiom_status(distributed_generation_preference, holdable).
narrative_ontology:cs_axiom_grounding('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', distributed_generation_preference, empirically_contingent).
narrative_ontology:cs_reference_frame('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', speed_cost_legitimacy_frame).
narrative_ontology:cs_drift_state('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', contemporary_energy_policy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e034d97-2b98-4ef9-a07d-a3ffa9aceedf', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_owners).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, storage_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, uranium_mining_communities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_dependent_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_owners).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, energy_consumers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_sufficiency).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, storage_viability_at_scale).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, speed_cost_primacy_in_decarbonization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives production tax credits, investment subsidies, and mandated market access; benefits from policy frameworks that prioritize variable renewable deployment over firm capacity. Capital flows toward solar, wind, and associated supply chains.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_industry, beneficiary,
    powerful, biographical, constrained, global).

% Households and businesses with rooftop solar and behind-the-meter storage; gain from net metering, self-consumption savings, and resilience value. Also pay grid costs shifted onto non-adopting ratepayers.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_owners, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_generation_owners, payer).

% Battery and long-duration storage companies; depend on renewable integration mandates and capacity market reforms that value flexibility. Revenue streams tied to policy-driven storage procurement targets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, storage_developers, beneficiary,
    moderate, biographical, constrained, global).

% Existing fleet faces early retirement pressure; new builds excluded from clean energy standards and financing. Capital allocation shifts away from nuclear toward renewables+storage, creating stranded asset risk and workforce dislocation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry, payer,
    powerful, generational, constrained, national).

% Remote and often Indigenous communities dependent on uranium mining employment and tax base; face economic collapse as demand contracts. Limited alternative employment; geographic isolation compounds exit difficulty.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, uranium_mining_communities, payer,
    moderate, biographical, trapped, regional).

% Regions where nuclear plants are primary employers and tax base; community identity fused to nuclear technology. Transition planning resisted because it threatens social cohesion and political representation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_dependent_regions, payer,
    organized, generational, identity_locked, regional).

% System operators (ISOs/TSOs) must integrate rising variable renewable shares while maintaining reliability; implement market rules that increasingly favor storage and demand response over firm capacity. Caught between political mandates and physics.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, national).

% Legislators and regulators designing decarbonization policy; set renewable portfolio standards, clean energy standards, and nuclear phase-out timelines. Legitimacy rests on delivering rapid emissions cuts at perceived lowest cost.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Ratepayers fund renewable subsidies and grid upgrades through bills; benefit from cleaner air and long-term cost declines but face near-term cost increases. Industrial consumers face competitiveness pressure.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_consumers, payer,
    organized, biographical, constrained, national).

% Oil, gas, and coal interests; structurally excluded from renewable-primacy policy coalitions. Would oppose any binding decarbonization but are not part of the intra-low-carbon debate over nuclear vs renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_industry, excluded,
    powerful, generational, constrained, global).

% Academic, NGO, and consultancy analysts modeling system costs, integration limits, and land-use constraints. Provide evidence cited by all sides; no material stake in outcome.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, independent_energy_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global capital deployment toward the fastest and cheapest decarbonization pathway (renewables+storage) to avoid climate catastrophe, solving the collective action problem of directing investment away from incumbent high-carbon and slow-build technologies.
% TRANSFER_FUNCTION: Moves capital from nuclear (and fossil) generation toward renewable generation and storage, and from ratepayers to renewable developers via subsidies and mandated procurement. Nuclear industry bears stranded asset costs; renewable industry captures policy rents.
% ABSENT_VOICES: Nuclear industry and uranium communities are structurally marginalized in renewable-centric policy forums; their concerns about reliability and just transition are dismissed as obstruction. Fossil fuel interests are excluded entirely from the low-carbon policy coalition.
% DISAPPEARANCE_RATIONALE: If renewable primacy policy vanished overnight, capital would flow back to nuclear life extensions and new gas, decarbonization timelines would extend, and the current renewable+storage supply chain would face demand collapse. The energy transition's direction and pace would fundamentally reorganize.
% FOUNDING_PROBLEM: The urgent need to decarbonize electricity rapidly and affordably to meet Paris Agreement targets, given the slow build rates and cost overruns of nuclear and the climate deadline.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII, IEA Net Zero by 2050 roadmap, and independent academic studies (e.g., Jacobson et al., Brown et al.) outside the renewable industry attest that rapid decarbonization remains unachieved and that renewables+storage are the fastest deployable option at scale. Nuclear industry and some governments contest the sufficiency claim.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects massive capital redirection: nuclear loses access to low-cost finance and clean energy credits while renewables gain guaranteed markets. Suppression (0.45) is moderate — nuclear isn't banned outright but is disadvantaged in permitting, finance, and market rules. Theater ratio (0.32) captures performative '100% renewable' targets that obscure integration costs and land-use conflicts. Accessibility collapse (0.55) because nuclear remains technically available but politically/institutionally inaccessible. Resistance (0.68) is high: nuclear industry, labor unions, and some governments actively contest the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable industry seat, the constraint is a rope: genuine coordination solving climate urgency. From nuclear-dependent regions, it's a snare: extraction disguised as climate action. From grid operators, it's a tangled rope: real coordination need (reliability with variables) but asymmetric burden (they bear integration costs). The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's judgment that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable industry and storage developers are structural beneficiaries (d ~0.15) — they collect subsidies and mandated revenue. Distributed generation owners are dual: benefit from net metering but pay shifted grid costs (d ~0.45). Nuclear industry and uranium communities are targets (d ~0.85) — they bear stranded asset and transition costs with constrained exit. Nuclear-dependent regions are identity-locked (d ~0.9) — community identity fused to nuclear makes exit psychologically prohibitive. Grid operators and policy-makers are agenda-setters with constrained exit (d ~0.5) — they administer the constraint but are trapped by their own mandates. Consumers pay (d ~0.6). Fossil fuel excluded (d irrelevant). Analysts analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rapid affordable decarbonization) remains live — emissions are still rising globally. However, the renewable primacy reading's claim that nuclear is unnecessary is contested by baseload_necessity and portfolio_pragmatism readings. If the founding problem is 'decarbonize fast and cheap', the reading's mandate persists; if the problem is 'decarbonize reliably at scale', the mandate may be obsolete. The classification prevents mislabeling: calling it a pure snare ignores the genuine coordination of global renewable deployment; calling it a pure rope ignores the deliberate disadvantaging of nuclear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    system_cost_at_high_penetration,
    'What is the full system cost (generation + storage + transmission + overbuild + land) of renewables+storage at >90% penetration compared to nuclear-inclusive portfolios?',
    'Integrated resource planning with realistic storage duration, transmission build-out, and land-use constraints; comparative system LCOE studies with consistent reliability metrics (LOLE, SAIDI).',
    'If system costs exceed nuclear-inclusive portfolios, the renewable primacy claim''s empirical foundation weakens, shifting classification toward snare (extraction without coordination efficiency). If confirmed cheaper, reinforces tangled_rope with stronger coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_cost_at_high_penetration, empirical, 'Whether the speed-cost advantage holds at full decarbonization scale including integration costs.').

omega_variable(
    foreclosure_vs_coexistence_with_baseload,
    'Does the renewable primacy reading logically foreclose the baseload necessity reading, or can both be held as complementary (e.g., renewables for bulk energy, nuclear for firm capacity)?',
    'Analysis of policy frameworks that include both (e.g., UK, France, Ontario): do they treat nuclear as necessary firm capacity or as a transitional bridge? Examination of whether ''100% renewable'' mandates explicitly exclude nuclear or merely prioritize renewables.',
    'If forecloses, the two readings cannot coexist in a single policy framework — classification of one as legitimate delegitimizes the other. If coexists_with, the kernel supports a portfolio_pragmatism synthesis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_with_baseload, conceptual, 'Logical relationship between renewable primacy and baseload necessity as legitimacy claims.').

omega_variable(
    suppression_mechanism_nuclear_exclusion,
    'Is the suppression of nuclear (exclusion from clean standards, financing bans) structural (legal/institutional barriers) or internalized (nuclear industry''s own cost structure and risk perception)?',
    'Counterfactual: if policy barriers removed, would nuclear deploy at scale given current cost curves? Compare Vogtle, Flamanstein, Olkiluoto overruns vs. renewable+storage cost declines.',
    'If structural, suppression metric reflects active policy enforcement. If internalized, suppression is lower than measured — nuclear''s decline is market-driven, not policy-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_nuclear_exclusion, empirical, 'Origin of nuclear''s competitive disadvantage: policy suppression vs. intrinsic economics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 18, 0.41).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__renewable_primacy_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_phase_out_policy).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_subsidy_regime).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, clean_energy_standard_design).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, grid_integration_mandates).

% DUAL FORMULATION NOTE:
% This reading and baseload_necessity_reading form a constraint family decomposing the kernel climate_mitigation_legitimacy. Renewable primacy claims speed-cost advantage; baseload necessity claims reliability necessity. They have different ε (this reading: 0.62 extractive toward nuclear; baseload reading would have ε toward renewables as intermittent). Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__renewable_primacy_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
