% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Growth-Compatible Mitigation Pathway (Below-2°C via Innovation and Carbon Markets)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The dominant international climate framework since the Kyoto Protocol and
 *   consolidated at Paris (2015) sets a below-2°C (preferably 1.5°C)
 *   temperature ceiling to be achieved primarily through emissions reductions
 *   enabled by clean-technology deployment and market-based carbon pricing,
 *   explicitly designed to be compatible with continued GDP growth in both
 *   developed and developing economies. This reading concentrates near-term
 *   adjustment costs in high-emitting sectors and offset-hosting land, defers
 *   adaptation financing for climate-vulnerable regions, and rests on an
 *   assumption — contested among climate scientists — that carbon removal
 *   technology will scale enough to manage any overshoot.
 *
 * KEY AGENTS:
 *   - innovation_capacity_nations: primary beneficiary (institutional/arbitrage) — sets terms, captures technology and market rents
 *   - carbon_market_intermediaries: beneficiary (organized/arbitrage) — collects fees on trading volume regardless of abatement additionality
 *   - global_south_frontline_states: primary target (powerless/trapped) — bears residual physical risk the pathway tolerates
 *   - future_generations: primary target (powerless/trapped, non-agent) — inherits carbon-removal debt with no representation
 *   - high_emitting_sector_workers: secondary target (moderate/constrained) — absorbs compressed-timeline transition costs
 *   - climate_scientists_and_iea_analysts: analytical observer — measures gap between pledged and required mitigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.61).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.47).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.61).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Growth-Compatible Mitigation Pathway (Below-2°C via Innovation and Carbon Markets)").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, '49fd10cb-0910-4f2c-93b9-268f25b6e55a').
narrative_ontology:cs_kernel_codification('49fd10cb-0910-4f2c-93b9-268f25b6e55a', formalized).
narrative_ontology:cs_authority_grounding('49fd10cb-0910-4f2c-93b9-268f25b6e55a', distributed).
narrative_ontology:cs_reading_relation('49fd10cb-0910-4f2c-93b9-268f25b6e55a', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('49fd10cb-0910-4f2c-93b9-268f25b6e55a', climate_response_action__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('49fd10cb-0910-4f2c-93b9-268f25b6e55a', foundational, growth_and_decarbonization_are_jointly_achievable).
narrative_ontology:cs_axiom_status(growth_and_decarbonization_are_jointly_achievable, holdable).
narrative_ontology:cs_axiom_grounding('49fd10cb-0910-4f2c-93b9-268f25b6e55a', growth_and_decarbonization_are_jointly_achievable, empirically_contingent).
narrative_ontology:cs_axiom('49fd10cb-0910-4f2c-93b9-268f25b6e55a', secondary, market_price_signals_are_the_efficient_abatement_allocator).
narrative_ontology:cs_axiom_status(market_price_signals_are_the_efficient_abatement_allocator, holdable).
narrative_ontology:cs_axiom_grounding('49fd10cb-0910-4f2c-93b9-268f25b6e55a', market_price_signals_are_the_efficient_abatement_allocator, instrumental).
narrative_ontology:cs_reference_frame('49fd10cb-0910-4f2c-93b9-268f25b6e55a', unfccc_common_but_differentiated_responsibility).
narrative_ontology:cs_drift_state('49fd10cb-0910-4f2c-93b9-268f25b6e55a', post_paris_agreement_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('49fd10cb-0910-4f2c-93b9-268f25b6e55a', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, innovation_capacity_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, incumbent_energy_majors).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, green_technology_exporters).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, global_south_frontline_states).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, high_emitting_sector_workers).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, smallholder_land_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, incumbent_energy_majors).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, decoupling_of_growth_and_emissions_is_achievable).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, carbon_pricing_efficiently_allocates_abatement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the patents, capital, and industrial base for renewables, batteries, and carbon capture. Set the negotiating terms at COP forums and in bilateral trade deals, framing the below-2°C pathway as growth-compatible because their own growth is compatible with it. Export technology and carbon credits at a profit while their domestic emissions curve down fastest.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, innovation_capacity_nations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, innovation_capacity_nations, agenda_setter).

% Broker, verify, and trade offset credits and allowances. Collect fees regardless of whether the underlying abatement is real or additional. Their business model depends on the mitigation pathway remaining market-based rather than being replaced by direct regulation or degrowth-style consumption limits.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Fund carbon capture pilots and green subsidiaries that let them claim alignment with the 2°C target while continuing fossil extraction under a longer transition timeline than physics would require. They pay compliance costs but recoup them through subsidized transition financing and offset sales.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, incumbent_energy_majors, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, incumbent_energy_majors, payer).

% Manufacture solar panels, EV batteries, and grid technology sold into the global mitigation buildout. Demand for their exports is created directly by the policy framework; a shift to degrowth or adaptation-first framing would shrink their addressable market.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, green_technology_exporters, beneficiary,
    powerful, generational, arbitrage, global).

% Contributed least historical emissions but face the residual warming the mitigation-only framework tolerates before adaptation finance materializes. Receive a fraction of pledged climate finance, are told to wait for technology diffusion, and bear sea-level, drought, and crop-failure costs that mitigation targets treat as an acceptable remainder.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, global_south_frontline_states, payer,
    powerless, generational, trapped, global).

% Inherit whatever temperature overshoot and carbon-removal debt the current pathway leaves unresolved. The framework's reliance on future negative-emissions technology at scale effectively borrows against their capacity to correct course, with no seat at any table where the borrowing is decided.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_action__mitigation_priority, future_generations).

% Employed in coal, steel, and heavy manufacturing regions undergoing rapid decarbonization mandates. Absorb job losses and local economic contraction on compressed timelines set by emissions targets, with retraining and just-transition funding chronically underdelivered relative to promises.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_emitting_sector_workers, payer,
    moderate, biographical, constrained, national).

% Have land enrolled in offset and afforestation schemes that generate tradeable credits for distant buyers, often with restricted use rights and displaced customary access, while receiving a small share of the credit revenue their land generates.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, smallholder_land_communities, payer,
    powerless, generational, trapped, regional).

% Model the physical feasibility of below-2°C pathways and publish gap reports showing pledged mitigation falling short of required trajectories. Provide the empirical basis that both supports and critiques the mitigation-priority framing.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_scientists_and_iea_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__mitigation_priority, innovation_capacity_nations).
narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns disparate national economies around a shared, quantifiable temperature ceiling and a market mechanism (carbon pricing/trading) that lets abatement happen where it is cheapest, in principle mobilizing private capital toward decarbonization faster than command regulation alone could.
% TRANSFER_FUNCTION: Moves near-term adjustment costs onto high-emitting sector workers and offset-hosting land communities, moves financing and technology rents toward innovation-capacity nations and green exporters, and moves residual physical risk and carbon-removal debt onto Global South frontline states and future generations.
% ABSENT_VOICES: Global South frontline states have formal seats at COP but limited negotiating leverage against bloc positions set by high-capacity economies; future generations and smallholder land communities whose land is enrolled in offset schemes have no direct representation in either the target-setting or the market-design process.
% DISAPPEARANCE_RATIONALE: Innovation-capacity nations and market intermediaries would say the world rearranges catastrophically without a coordinated mitigation target — investment signals collapse and emissions accelerate. Frontline states and degrowth-oriented critics would say the underlying warming trajectory and its distributional harms are barely changed by the framework's disappearance, since actual delivered mitigation has consistently lagged pledged mitigation; the dispute over which is true is itself part of what the constraint contests.
% FOUNDING_PROBLEM: Following the 1992 UNFCCC and especially post-Kyoto disappointment, the problem was to build an emissions-reduction architecture that major emitting economies would actually join, which meant making decarbonization compatible with continued growth and channeling it through markets and innovation rather than binding consumption limits.
% FOUNDING_PROBLEM_CORROBORATION: IPCC and IEA gap-report authors (analytical, outside the beneficiary set) attest the founding problem — avoiding dangerous warming — remains live and that current pledged mitigation under this framework is insufficient to meet it. Frontline-state negotiating blocs (also outside the beneficiary set) attest the framework has partially shifted from solving the problem to managing which parties bear its costs, without independently verifying the beneficiaries' claim that decoupling and technology diffusion are on track.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, contested).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that this is not simple coordination: the framework mobilizes genuine cross-national cooperation on a real collective-action problem (coordination function present) while systematically routing benefits toward capital- and technology-rich actors and costs toward those least equipped to refuse them (asymmetric extraction present) — the twin conditions for tangled_rope. Suppression (0.47) is moderate rather than severe: no single actor is coerced into the framework by force, but exit from the carbon-market architecture is heavily disfavored by trade and finance conditionality, and offset-hosting communities often cannot meaningfully refuse enrollment. Theater ratio (0.42) is substantial and rising — a large and growing share of pledged mitigation activity (net-zero pledges without implementation plans, offset credits later found non-additional) is now understood as performative relative to physical emissions outcomes, a documented and worsening pattern from the Kyoto era through the post-Paris pledge-and-review period.
 *
 * DIRECTIONALITY LOGIC:
 *   Innovation-capacity nations and green technology exporters sit near the full-beneficiary end: the pathway is structured around their existing capabilities and creates export demand for their products. Carbon market intermediaries and incumbent energy majors likewise benefit from a market-based rather than regulatory architecture. Global South frontline states, future generations, and smallholder land communities sit near the full-target end: trapped exit options (no alternative pathway is on offer to them individually; the kernel's other readings are contested at the same negotiating tables they lack leverage in), diffuse and generational time horizons, and structural inability to renegotiate the terms. High-emitting sector workers occupy an intermediate position — moderate power, constrained exit — bearing real but more localized and potentially compensable costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — building a decarbonization architecture broad economies would actually join — was real in 1997 and remains real in 2026 (IPCC/IEA corroboration outside the beneficiary set). This prevents dismissing the framework as pure extraction: it is not a hollow shell defending a solved problem, which is why the classification is tangled_rope rather than snare. But the founding_problem_status is authored 'contested' rather than 'live' because delivered mitigation has persistently lagged pledged mitigation while market and technology-export beneficiaries have captured value regardless of delivery — the coordination function is real but increasingly decoupled from the physical outcome it claims to produce, which is exactly the drift tangled_rope is built to register rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_removal_feasibility_at_scale,
    'Will negative-emissions technology (direct air capture, enhanced weathering, large-scale afforestation) actually reach the deployment scale the mitigation pathway''s overshoot accounting assumes, or does the framework structurally borrow against a technological capacity that will not materialize?',
    'Track actual gigaton-scale deployment of verified carbon removal against IPCC pathway requirements over the next two decades; compare cost curves against renewable-energy precedents where scaling did occur.',
    'If removal technology fails to scale, the mitigation-priority reading''s core feasibility assumption collapses and the deferred residual-warming costs currently borne by future generations and frontline states become permanent rather than transitional — sharply raising this constraint''s effective extraction and strengthening the case for the degrowth_transformation or adaptation_priority readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_removal_feasibility_at_scale, empirical, 'Whether assumed future carbon-removal capacity is real or a structural IOU against the powerless.').

omega_variable(
    growth_decoupling_reality,
    'Is absolute decoupling of GDP growth from emissions actually occurring at the pace and scale the mitigation-priority framework requires, or is observed decoupling partly an artifact of offshoring emissions-intensive production to the Global South?',
    'Compare consumption-based versus production-based emissions accounting across innovation-capacity nations over the interval; assess whether decoupling holds under consumption accounting.',
    'If decoupling is substantially an accounting artifact, the vindicated proposition ''decoupling_of_growth_and_emissions_is_achievable'' is weaker than claimed, and part of the measured benefit to innovation-capacity nations is actually externalized production emissions rather than genuine mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_reality, empirical, 'Whether growth-emissions decoupling is real or partly offshored.').

omega_variable(
    kernel_reading_selection_legitimacy,
    'Is the dominance of the mitigation_priority reading over adaptation_priority and degrowth_transformation in international negotiations a function of which reading best solves the climate problem, or a function of which reading is least disruptive to the interests of the parties with the most negotiating power?',
    'Compare negotiating-bloc positions and voting patterns at COP sessions against national interest profiles (fossil fuel dependency, technology export capacity, growth trajectory) to test whether reading preference tracks structural position rather than problem-solving merit.',
    'If reading selection tracks power rather than merit, the mitigation_priority reading''s dominance is itself evidence of the tangled_rope structure rather than a neutral policy conclusion — this bears on how confident to be that this reading, rather than a sibling, should organize global response at all.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_legitimacy, conceptual, 'Whether kernel-reading dominance tracks problem-solving fit or negotiating power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 1997, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1997, climate_response_action__mitigation_priority, theater_ratio, 1997, 0.2).
narrative_ontology:measurement(clim_tr_t2005, climate_response_action__mitigation_priority, theater_ratio, 2005, 0.27).
narrative_ontology:measurement(clim_tr_t2012, climate_response_action__mitigation_priority, theater_ratio, 2012, 0.33).
narrative_ontology:measurement(clim_tr_t2018, climate_response_action__mitigation_priority, theater_ratio, 2018, 0.37).
narrative_ontology:measurement(clim_tr_t2022, climate_response_action__mitigation_priority, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(clim_tr_t2026, climate_response_action__mitigation_priority, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t1997, climate_response_action__mitigation_priority, base_extractiveness, 1997, 0.34).
narrative_ontology:measurement(clim_be_t2005, climate_response_action__mitigation_priority, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(clim_be_t2012, climate_response_action__mitigation_priority, base_extractiveness, 2012, 0.48).
narrative_ontology:measurement(clim_be_t2018, climate_response_action__mitigation_priority, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(clim_be_t2022, climate_response_action__mitigation_priority, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement(clim_be_t2026, climate_response_action__mitigation_priority, base_extractiveness, 2026, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1997, climate_response_action__mitigation_priority, suppression_requirement, 1997, 0.22).
narrative_ontology:measurement(clim_su_t2005, climate_response_action__mitigation_priority, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(clim_su_t2012, climate_response_action__mitigation_priority, suppression_requirement, 2012, 0.33).
narrative_ontology:measurement(clim_su_t2018, climate_response_action__mitigation_priority, suppression_requirement, 2018, 0.39).
narrative_ontology:measurement(clim_su_t2022, climate_response_action__mitigation_priority, suppression_requirement, 2022, 0.43).
narrative_ontology:measurement(clim_su_t2026, climate_response_action__mitigation_priority, suppression_requirement, 2026, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint, adaptation_priority, and degrowth_transformation form the climate_response_action kernel family. Each reading concentrates costs and benefits on different actor sets and rests on different feasibility assumptions; they are linked here rather than merged because their ε values, beneficiary/victim structures, and classifications differ materially (this reading: tangled_rope, ε≈0.61, beneficiaries concentrated in innovation-capacity nations; sibling readings author their own ε and type independently).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
