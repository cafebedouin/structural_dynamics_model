% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Portfolio-Optimization Reading of the Climate Mitigation Imperative (Nuclear-Inclusive Carbon-Intensity Standard)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the portfolio-optimization reading of the
 *   contested climate mitigation imperative kernel: the claim that
 *   decarbonization requires maximizing deployment of ALL low-carbon sources,
 *   with nuclear treated as structurally necessary for reliable baseload
 *   power. Under this reading the mitigation standard is written as a
 *   technology-neutral carbon-intensity threshold rather than a
 *   technology-specific mandate, which functionally routes subsidy,
 *   capacity-market revenue, and construction-cost recovery toward nuclear
 *   operators and advanced reactor developers while fossil generation is
 *   displaced on a compliance timeline. Two sibling readings of the same
 *   kernel exist as separate constraints: the opportunity_cost_reading holds
 *   that nuclear's capital intensity and multi-year construction timelines
 *   make it a net-harmful allocation of mitigation capital relative to
 *   faster-deploying renewables plus storage; the systems_transition_reading
 *   holds that mitigation requires decentralizing and democratizing energy
 *   systems, and that nuclear's centralized, capital-concentrated,
 *   technocratically-governed structure is itself part of the problem
 *   mitigation should dismantle. All three readings share the founding
 *   problem (rapid, reliable decarbonization) but diverge sharply on
 *   beneficiary/victim structure, coordination logic, and what 'success'
 *   looks like. This file addresses ONLY the portfolio-optimization reading;
 *   ε, beneficiaries, and victims here are NOT averaged or hedged against the
 *   sibling readings.
 *
 * KEY AGENTS:
 *   - nuclear_utility_operators: primary beneficiary (institutional/mobile) — collects subsidy and capacity revenue under the technology-neutral standard
 *   - advanced_reactor_developers: secondary beneficiary (organized/constrained) — depends on the framing for long-horizon capital access
 *   - grid_reliability_agencies: agenda_setter (institutional/analytical) — administers the reliability-adequacy rules that operationalize baseload necessity
 *   - fossil_fuel_incumbents: primary target (powerful/constrained) — bears displacement under the compliance schedule
 *   - coal_dependent_labor_regions: diffuse payer (powerless/trapped) — bears the transition's regional employment cost
 *   - ratepayers_in_new_build_jurisdictions: diffuse payer (powerless/trapped) — bears construction cost-overrun risk
 *   - distributed_renewable_developers: structurally disadvantaged competitor (moderate/constrained) — excluded from capacity-market weighting despite cost competitiveness
 *   - climate_scientists_and_iea_modelers: analytical observer (analytical/analytical) — sees the full modeling-assumption structure driving the reading's plausibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Portfolio-Optimization Reading of the Climate Mitigation Imperative (Nuclear-Inclusive Carbon-Intensity Standard)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '6c73e1b4-1485-4aa0-b33e-e8b857781652').
narrative_ontology:cs_kernel_codification('6c73e1b4-1485-4aa0-b33e-e8b857781652', distributed).
narrative_ontology:cs_authority_grounding('6c73e1b4-1485-4aa0-b33e-e8b857781652', distributed).
narrative_ontology:cs_reading_relation('6c73e1b4-1485-4aa0-b33e-e8b857781652', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c73e1b4-1485-4aa0-b33e-e8b857781652', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('6c73e1b4-1485-4aa0-b33e-e8b857781652', foundational, technology_neutral_carbon_intensity_is_the_correct_metric).
narrative_ontology:cs_axiom_status(technology_neutral_carbon_intensity_is_the_correct_metric, holdable).
narrative_ontology:cs_axiom_grounding('6c73e1b4-1485-4aa0-b33e-e8b857781652', technology_neutral_carbon_intensity_is_the_correct_metric, instrumental).
narrative_ontology:cs_axiom('6c73e1b4-1485-4aa0-b33e-e8b857781652', foundational, dispatchable_firm_capacity_is_necessary_for_reliability).
narrative_ontology:cs_axiom_status(dispatchable_firm_capacity_is_necessary_for_reliability, holdable).
narrative_ontology:cs_axiom_grounding('6c73e1b4-1485-4aa0-b33e-e8b857781652', dispatchable_firm_capacity_is_necessary_for_reliability, empirically_contingent).
narrative_ontology:cs_reference_frame('6c73e1b4-1485-4aa0-b33e-e8b857781652', thermal_era_reliability_adequacy_standard).
narrative_ontology:cs_drift_state('6c73e1b4-1485-4aa0-b33e-e8b857781652', high_renewable_penetration_present, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6c73e1b4-1485-4aa0-b33e-e8b857781652', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_utility_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_component_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_agencies).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, advanced_reactor_developers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, coal_dependent_labor_regions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers_in_new_build_jurisdictions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, distributed_renewable_developers).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, technology_neutral_carbon_accounting_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__portfolio_optimization_reading, baseload_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing and planned nuclear fleets and receive production tax credits, loan guarantees, and favorable capacity-market treatment once the mitigation standard is written to be technology-neutral on carbon intensity rather than technology-specific. Their existing plants, previously at risk of early retirement against cheaper gas and renewables, gain a durable revenue floor. They lobby actively to keep the standard framed around carbon intensity and reliability rather than deployment speed or decentralization.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_utility_operators, beneficiary,
    institutional, generational, mobile, national).

% Small modular reactor and advanced fission startups depend entirely on the portfolio-optimization framing to access long-horizon capital and government demonstration funding; the opportunity-cost reading would starve them of capital by ranking near-term deployment speed above their still-unproven cost curve.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, advanced_reactor_developers, beneficiary,
    organized, civilizational, constrained, national).

% System operators and reliability regulators write the capacity-adequacy rules that operationalize 'baseload necessity' into procurement mandates, interconnection priority, and capacity payments favoring dispatchable low-carbon generation. They administer the standard and could, in principle, rewrite adequacy metrics to be storage-and-demand-response-neutral instead of baseload-centric, but doing so would require rebuilding reliability models institutions have run on for decades.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_reliability_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Coal and, increasingly, gas generators are classified as high carbon-intensity sources and face closure schedules, carbon pricing, and exclusion from capacity procurement once nuclear is validated as the reliable low-carbon alternative. They retain enough political power to slow the transition but cannot escape the eventual reclassification; their fallback is regulatory delay, not exit.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_incumbents, payer,
    powerful, biographical, constrained, national).

% Communities whose local economy is built around coal extraction and coal-fired generation bear plant closures and job losses on the mitigation standard's timeline, while nuclear's long construction lead times mean replacement jobs and tax base do not arrive on the same schedule, if the plant is sited in the region at all. They have little influence over where new nuclear capacity is sited.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, coal_dependent_labor_regions, payer,
    powerless, biographical, trapped, regional).

% Residents of states or utility territories where new nuclear construction is approved absorb cost overruns and schedule delays through rate base recovery mechanisms, often years before the plant generates a kilowatt. Nuclear's history of large capital overruns means the ratepayer, not the developer, typically bears construction risk under current regulatory structures.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, ratepayers_in_new_build_jurisdictions, payer,
    powerless, biographical, trapped, regional).

% Wind, solar, and storage developers compete for the same procurement dollars and interconnection queue slots; when reliability agencies weight baseload dispatchability heavily, distributed and variable resources are structurally disadvantaged in capacity auctions regardless of their levelized cost advantage, even though they could plausibly meet the same reliability need through aggregation and storage.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, distributed_renewable_developers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, distributed_renewable_developers, excluded).

% Publish integrated assessment models and scenario analyses comparing portfolios with and without significant nuclear contribution; their modeling choices about discount rates, deployment constraints, and reliability assumptions materially shift which reading of the mitigation imperative looks correct, without themselves collecting from any resolution.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_scientists_and_iea_modelers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation and grid-reliability planning around a technology-neutral carbon-intensity threshold, allowing utilities, regulators, and financiers to converge on a shared definition of 'acceptable' generation without re-litigating technology choice project by project.
% TRANSFER_FUNCTION: Moves capacity-market revenue, loan guarantees, and construction-cost recovery from ratepayers and fossil-fuel-displaced regions toward nuclear operators and developers, justified by the reliability premium nuclear is asserted to provide over variable renewables.
% ABSENT_VOICES: Coal-region workers and ratepayers in nuclear new-build territories have no seat in the reliability-standard rulemaking process, which is dominated by utility technical staff, reactor vendors, and system-operator engineers; their objections surface only after cost overruns or plant closures are already underway.
% DISAPPEARANCE_RATIONALE: If the baseload-necessity framing were abandoned, nuclear subsidies and capacity-market preferences tied to dispatchability would likely be rewritten around storage-inclusive reliability metrics — nuclear operators and advanced reactor developers would lose a durable revenue argument, while distributed renewable developers would gain procurement access. Reliability agencies dispute whether the underlying physical need for dispatchable capacity would truly vanish or merely be met by a different technology mix; that empirical question is exactly what the sibling readings contest.
% FOUNDING_PROBLEM: Rapid decarbonization requires displacing fossil generation while keeping electricity grids reliable at all hours, including periods when wind and solar output is low; some coordination mechanism is needed to decide which low-carbon sources qualify for support and how reliability is defined and paid for.
% FOUNDING_PROBLEM_CORROBORATION: Independent grid engineers and the International Energy Agency corroborate that a reliability-adequacy problem is real and unresolved by variable renewables alone at high penetration; however, storage-industry analysts and several national grid operators outside the nuclear-vendor coalition (e.g. reports from independent system operators in renewables-heavy markets) attest that the specific 'nuclear is necessary' formulation of the solution is a contested policy choice, not a settled engineering conclusion — corroboration for the founding problem's existence is broader than corroboration for this particular reading's prescribed fix.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).
:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the standard genuinely solves a coordination problem (defining acceptable low-carbon generation, enabling capital planning) but layers asymmetric transfer on top — nuclear capital costs and construction risk are substantially socialized onto ratepayers via cost-recovery mechanisms, while coal regions absorb transition costs on a schedule uncoupled from replacement investment timing. Suppression (0.38) reflects real but not overwhelming coercive force: fossil incumbents face closure schedules and carbon pricing, and distributed renewable developers face capacity-market rules weighted against them, but neither group is trapped without any recourse — political and legal contestation of the reliability standards remains active and sometimes successful. Theater ratio (0.28) is moderate-low: baseload-necessity claims are grounded in real grid-reliability physics at high renewable penetration, though the specific inference that NUCLEAR (rather than storage, demand response, or transmission expansion) is the necessary solution carries a performative element defended more by incumbent modeling conventions than by settled engineering consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear utility operators and advanced reactor developers sit near the beneficiary end: the technology-neutral carbon-intensity framing is precisely the argument that converts their existing/planned assets into subsidy-eligible, capacity-market-favored generation. Fossil fuel incumbents sit near the target end: displacement is the constraint's stated function toward them, though their organized power gives them more room to negotiate transition timing than powerless payers get. Coal-dependent labor regions and new-build ratepayers are structurally close to full-target despite bearing no direct decision-making role — they are trapped by geography and rate-base mechanics respectively, with no meaningful exit from the costs the standard assigns them. Distributed renewable developers are a harder case: they are low-carbon sources who should, under the imperative's own logic, be maximized alongside nuclear, but the baseload-necessity framing specifically privileges dispatchable generation in procurement weighting, making them structural payers relative to what an all-sources-equal reading would give them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (grid reliability during decarbonization) remains genuinely live per independent grid-engineering corroboration — this blocks any claim that the entire standard is a dead mandate. What is contested, and what the R5 corroboration explicitly flags, is whether nuclear-specific baseload necessity is the correct or only solution to that live problem, versus a technology-neutral reliability standard that could equally be met by storage-plus-renewables portfolios. This is the diagnostic case the tangled_rope classification is built for: coordination function is real (a reliability standard is needed) and extraction is real (nuclear captures disproportionate subsidy relative to displaced or excluded alternatives) simultaneously, on the same structure — treating it as pure Rope would erase the extraction; treating it as pure Snare would erase the genuine reliability-coordination need the founding problem corroboration establishes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity_empirical_status,
    'Is nuclear generation structurally necessary for grid reliability at high renewable penetration, or is ''baseload necessity'' a modeling convention inherited from thermal-generation-era reliability standards that storage, demand response, and transmission expansion can substitute for?',
    'Comparative grid-reliability outcomes from jurisdictions that have reached high variable-renewable penetration with and without significant nuclear contribution (e.g. South Australia, Denmark vs. France, Ontario), controlling for interconnection and storage deployment levels.',
    'If reliability outcomes are equivalent without nuclear given sufficient storage and transmission investment, the portfolio-optimization reading''s core empirical premise weakens substantially and the constraint''s beneficiary structure (nuclear capturing capacity-market and subsidy revenue) becomes harder to justify as coordination rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_status, empirical, 'Whether nuclear is empirically necessary for reliability or a legacy modeling assumption.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three sibling readings of the climate mitigation imperative kernel should govern actual policy allocation, given that they produce materially different beneficiary/victim structures and none has achieved settled consensus?',
    'This is a conceptual/preference-laden question not resolvable by data alone: it depends on discount-rate assumptions (favoring fast-deploying options vs. long-horizon firm capacity), values about centralization vs. decentralization of infrastructure control, and risk tolerance for construction-cost variance. Route through explicit multi-criteria policy deliberation rather than treating it as a single empirical fact to discover.',
    'Selecting this reading over its siblings determines which real-world actors (nuclear operators vs. renewable/storage developers vs. community energy cooperatives) receive subsidy and procurement priority — the choice of reading IS a distributive policy choice, not a neutral technical finding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, preference, 'Which kernel reading should govern policy is a values-and-framing question, not a settled empirical one.').

omega_variable(
    cost_overrun_attribution,
    'Are nuclear construction cost overruns and schedule delays an inherent feature of the technology at current regulatory and supply-chain maturity, or an artifact of decades-long construction gaps that eroded institutional building capacity?',
    'Track cost and schedule performance of nuclear builds in jurisdictions with continuous construction pipelines (South Korea, historically France) versus jurisdictions restarting after long gaps (US, UK); a persistent gap under continuous-build conditions would indicate an inherent cost problem rather than a recoverable capacity problem.',
    'If overruns are a recoverable artifact of construction-gap deskilling, the ratepayer burden described in this story is a transitional cost that could shrink significantly with sustained build programs, weakening the extraction reading. If overruns persist even under continuous build, the extraction from ratepayers is closer to structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_overrun_attribution, empirical, 'Whether nuclear cost overruns are inherent to the technology or an artifact of interrupted construction pipelines.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.15).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the colloquial 'climate mitigation imperative' concept per the ε-invariance principle. All three share a founding problem (reliable rapid decarbonization) but diverge in beneficiary/victim structure and coordination logic: portfolio_optimization_reading (this file, nuclear as beneficiary, fossil fuels as primary victim, tangled_rope), opportunity_cost_reading (nuclear as victim of capital misallocation relative to faster options, likely rope or tangled_rope with inverted nuclear positioning), and systems_transition_reading (nuclear as target of decentralization displacement, likely tangled_rope or snare depending on how centralized-infrastructure extraction is characterized). Each carries its own stable ε and its own claimed_type; none is a measurement of the same underlying constraint viewed differently — they are structurally distinct policy commitments that happen to share a label and a founding problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
