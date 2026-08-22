% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Imperative (Opportunity Cost Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The climate mitigation imperative is a contested kernel: the consensus
 *   that greenhouse-gas emissions must be reduced rapidly is interpreted
 *   differently by competing policy frames. This constraint story
 *   instantiates the opportunity_cost_reading, which operationalizes the
 *   imperative as maximizing tons of carbon avoided per dollar per year.
 *   Under this reading, nuclear energy's long construction timelines and high
 *   upfront capital intensity make it net-harmful to mitigation because every
 *   dollar spent on nuclear is a dollar not deployed in faster-renewable or
 *   storage capacity. The constraint is the standing policy and financial
 *   arrangement that directs capital away from nuclear and toward fast-deploy
 *   alternatives. Key agents include the institutional actors setting
 *   procurement rules, the renewable and fast-deploy technology sectors that
 *   benefit from capital diversion, and the nuclear sector that bears the
 *   cost of exclusion. The reading is one of three in a constraint family;
 *   the other two (portfolio optimization and systems transition) produce
 *   different beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - climate_policy_gatekeepers: Primary agenda-setter (institutional/analytical) â sets the speed-per-dollar procurement rules.
 *   - renewable_energy_developers: Primary beneficiary (organized/mobile) â captures diverted capital and policy support.
 *   - fast_deploy_technology_vendors: Secondary beneficiary (powerful/arbitrage) â supplies the preferred modular technologies.
 *   - nuclear_energy_sector: Primary payer (organized/constrained) â excluded from climate finance and green taxonomies.
 *   - climate_systems_analysts: Analytical observer â models the metrics that underpin the reading without capturing gains.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.72).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.75).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Imperative (Opportunity Cost Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, 'a54bf53d-db39-42f0-94d0-23df32d90a05').
narrative_ontology:cs_kernel_codification('a54bf53d-db39-42f0-94d0-23df32d90a05', formalized).
narrative_ontology:cs_authority_grounding('a54bf53d-db39-42f0-94d0-23df32d90a05', expertise).
narrative_ontology:cs_interpretation_layer_present('a54bf53d-db39-42f0-94d0-23df32d90a05').
narrative_ontology:cs_reading_relation('a54bf53d-db39-42f0-94d0-23df32d90a05', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('a54bf53d-db39-42f0-94d0-23df32d90a05', climate_mitigation_imperative__systems_transition_reading, influences).
narrative_ontology:cs_axiom('a54bf53d-db39-42f0-94d0-23df32d90a05', foundational, fastest_deployment_priority).
narrative_ontology:cs_axiom_status(fastest_deployment_priority, holdable).
narrative_ontology:cs_axiom_grounding('a54bf53d-db39-42f0-94d0-23df32d90a05', fastest_deployment_priority, empirically_contingent).
narrative_ontology:cs_axiom('a54bf53d-db39-42f0-94d0-23df32d90a05', foundational, nuclear_opportunity_cost_prohibitive).
narrative_ontology:cs_axiom_status(nuclear_opportunity_cost_prohibitive, holdable).
narrative_ontology:cs_axiom_grounding('a54bf53d-db39-42f0-94d0-23df32d90a05', nuclear_opportunity_cost_prohibitive, empirically_contingent).
narrative_ontology:cs_reference_frame('a54bf53d-db39-42f0-94d0-23df32d90a05', urgent_carbon_budget_scarcity).
narrative_ontology:cs_drift_state('a54bf53d-db39-42f0-94d0-23df32d90a05', renewable_cost_parity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a54bf53d-db39-42f0-94d0-23df32d90a05', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, fast_deploy_technology_vendors).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_energy_sector).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, fastest_mitigation_hypothesis).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, opportunity_cost_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set technology-specific procurement rules, green taxonomies, and subsidy frameworks that operationalize the mitigation imperative as a speed-per-dollar optimization. They exclude nuclear from eligibility on timeline and capital-intensity grounds, directing public and private capital toward fast-deploy alternatives.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_policy_gatekeepers, agenda_setter,
    institutional, generational, analytical, global).

% Receive subsidized capital, streamlined permitting, and priority grid access justified by rapid deployment timelines. Their project finance models depend on the exclusion of nuclear from competing for the same public capital pools and on carbon-accounting frameworks that discount lifecycle density in favor of annual build-rate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Supply modular generation, storage, and grid-balancing hardware that fits the fast-per-dollar procurement paradigm. Their market expansion is structurally coupled to planning frameworks that reward short construction cycles and penalize long-lead infrastructure.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fast_deploy_technology_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Bears the cost of exclusion from climate finance, green taxonomies, and integrated resource plans. Capital that could fund next-generation nuclear is diverted to alternatives under the imperative of speed, while the sector faces higher financing costs due to policy signals that categorize nuclear as net-harmful to mitigation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_energy_sector, payer,
    organized, generational, constrained, global).

% Model emissions pathways and calculate carbon-per-dollar metrics. They provide the empirical scaffolding for the opportunity-cost reading but do not themselves capture the capital flows.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_systems_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The genuine coordination problem is reducing greenhouse-gas emissions rapidly enough to avoid catastrophic warming; the opportunity-cost reading operationalizes this by prioritizing technologies that maximize near-term emissions reductions per dollar invested.
% TRANSFER_FUNCTION: Moves capital, subsidy eligibility, and grid-planning priority from capital-intensive, slow-to-deploy nuclear generation to fast-deploy, modular low-carbon technologies (renewables, storage), under the imperative of maximizing mitigation speed.
% ABSENT_VOICES: Nuclear engineers and lifecycle analysts who demonstrate high carbon-density per unit of capital over plant lifetime; long-term grid planners emphasizing baseload reliability and system inertia; future ratepayers in jurisdictions that may face backup and integration costs from a high-renewables, no-nuclear portfolio.
% DISAPPEARANCE_RATIONALE: If the fastest-per-dollar imperative and its enforcement vanished overnight, integrated resource planning would re-weight lifecycle carbon density and reliability, nuclear projects would regain financing access in climate-aligned capital pools, and public subsidy flows would shift toward mixed-baseload systems â the global low-carbon investment landscape would reorganize.
% FOUNDING_PROBLEM: Catastrophic anthropogenic climate change requiring rapid decarbonization of the energy sector.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and the IPCC attest the urgency of mitigation from outside the renewable-industry beneficiary set. However, the specific claim that nuclear is too slow to contribute is contested by independent energy-systems analysts and the IEA, which corroborate a portfolio need for nuclear in many jurisdictions.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint structurally diverts billions in climate finance from nuclear to renewables based on a speed-per-dollar criterion that treats nuclear's lifecycle carbon density as irrelevant. Suppression (0.75) is high because the constraint persists through active exclusion: green taxonomies, procurement rules, and subsidy regimes that render nuclear ineligible. Theater ratio (0.40) reflects moderate performative maintenance â the 'fastest deployment' frame is partly genuine (renewables are fast) and partly a legitimizing narrative for technology preferences that predate the analysis. Accessibility collapse (0.65) captures the marginalization of long-lead infrastructure planning within climate finance; alternatives to the speed-per-dollar frame exist but are excluded from mainstream policy discourse. Resistance (0.60) reflects organized pushback from nuclear industries and some national governments.
 *
 * PERSPECTIVAL GAP:
 *   The renewable developer seat experiences the constraint as enabling coordination: public rules create a predictable investment environment for fast-build generation. The nuclear sector seat experiences the same arrangement as extraction: eligibility rules strip it of access to the same capital pools. The policy gatekeeper seat sees a technocratic optimization. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options (mobile vs. constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (renewable_energy_developers, fast_deploy_technology_vendors) have low directionality: the constraint subsidizes their market access and capital flows. The payer (nuclear_energy_sector) has high directionality: the constraint extracts by denying finance and policy legitimacy. Agenda-setters sit near the middle but lean beneficiary because their authority is reinforced by the arrangement's output.
 *
 * MANDATROPHY ANALYSIS:
 *   If the coordination function were absent â if the imperative were not addressing a genuine emissions-reduction problem â the constraint would be a Snare. If the extraction were absent â if nuclear were not structurally excluded and capital were allocated neutrally â the constraint would be a Rope. The Tangled Rope classification captures that both are present: the genuine coordination problem of rapid decarbonization is solved through a mechanism that asymmetrically extracts from one low-carbon sector to benefit others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the climate mitigation imperative best served by maximizing immediate deployment speed per dollar, or by optimizing total low-carbon generation over a multi-decade transition?',
    'Historical macro-analysis of emissions pathways in jurisdictions with versus without nuclear retention, controlling for GDP and grid structure.',
    'If nuclear-retaining jurisdictions achieve comparable or superior decadal emissions intensity reductions, the opportunity-cost reading''s victimization of nuclear collapses; if speed-per-dollar dominates, the reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether the opportunity-cost reading or the portfolio reading better describes effective mitigation.').

omega_variable(
    capital_diversion_mechanism,
    'Does the policy framework structurally divert capital from nuclear, or does nuclear fail to attract capital independently due to intrinsic market risk?',
    'Comparative analysis of financing costs for nuclear vs. renewable projects in jurisdictions with neutral technology-neutral climate auctions vs. technology-specific mandates.',
    'If neutral auctions still see nuclear underperform, the constraint''s extraction is lower than claimed; if technology-specific frameworks cause diversion, the constraint actively extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_diversion_mechanism, empirical, 'Whether capital exclusion is policy-driven or market-driven.').

omega_variable(
    framing_underdetermination,
    'Does the opportunity-cost reading foreclose the portfolio optimization reading within a single policy framework, or can they coexist?',
    'Examination of whether statutory frameworks can simultaneously mandate technology-neutral portfolio standards and fastest-deployment-per-dollar procurement rules.',
    'If foreclosed, the kernel is a zero-sum contest; if coexistent, the readings are factional positions within a live dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'Logical relationship between opportunity-cost and portfolio readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 25, 0.73).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, systems_transition_reading).

% DUAL FORMULATION NOTE:
% The kernel 'climate_mitigation_imperative' decomposes into three structurally distinct constraints per the epsilon-invariance principle. The opportunity_cost reading claims nuclear is net-harmful due to capital intensity; the portfolio reading claims nuclear is necessary for baseload; the systems reading claims nuclear perpetuates centralization. Each has different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
