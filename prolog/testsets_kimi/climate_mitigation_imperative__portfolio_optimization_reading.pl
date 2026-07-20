% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation Portfolio Optimization with Nuclear Baseload Mandate
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story models the portfolio_optimization reading of the
 *   climate_mitigation_imperative kernel: the claim that climate mitigation
 *   requires maximizing all low-carbon sources, with nuclear power treated as
 *   necessary for reliable baseload. It operates as a technology governance
 *   framework that coordinates decarbonization while asymmetrically directing
 *   public support toward nuclear operators and extracting compliance costs
 *   from fossil fuel incumbents and ratepayers. The constraint is actively
 *   enforced through carbon pricing, portfolio standards, and subsidy
 *   allocation. As one reading of a contested kernel, it coexists with
 *   sibling readings (opportunity_cost, systems_transition) that reallocate
 *   the same mitigation imperative across different technology sets.
 *
 * KEY AGENTS:
 *   - Nuclear industry (beneficiary / powerful / constrained exit) â receives subsidy and regulatory support justified by baseload necessity
 *   - Fossil fuel incumbents (payer / powerful / constrained exit) â bear phase-out costs and stranded assets
 *   - Renewable energy sector (beneficiary / organized / mobile exit) â gains from all-low-carbon framing
 *   - Electricity ratepayers (payer / organized / constrained exit) â fund subsidies through regulated rates
 *   - Climate policy architects (agenda_setter / institutional / analytical exit) â design the portfolio framework
 *   - Environmental justice groups (excluded / organized / constrained exit) â oppose nuclear but are sidelined by carbon-neutrality framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation Portfolio Optimization with Nuclear Baseload Mandate").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '480c00b2-3e83-4a09-a0f2-a829167addd1').
narrative_ontology:cs_kernel_codification('480c00b2-3e83-4a09-a0f2-a829167addd1', formalized).
narrative_ontology:cs_authority_grounding('480c00b2-3e83-4a09-a0f2-a829167addd1', expertise).
narrative_ontology:cs_interpretation_layer_present('480c00b2-3e83-4a09-a0f2-a829167addd1').
narrative_ontology:cs_reading_relation('480c00b2-3e83-4a09-a0f2-a829167addd1', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('480c00b2-3e83-4a09-a0f2-a829167addd1', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('480c00b2-3e83-4a09-a0f2-a829167addd1', foundational, baseload_reliability_requires_nuclear).
narrative_ontology:cs_axiom_status(baseload_reliability_requires_nuclear, holdable).
narrative_ontology:cs_axiom_grounding('480c00b2-3e83-4a09-a0f2-a829167addd1', baseload_reliability_requires_nuclear, empirically_contingent).
narrative_ontology:cs_axiom('480c00b2-3e83-4a09-a0f2-a829167addd1', foundational, carbon_intensity_technology_neutrality).
narrative_ontology:cs_axiom_status(carbon_intensity_technology_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('480c00b2-3e83-4a09-a0f2-a829167addd1', carbon_intensity_technology_neutrality, conventional).
narrative_ontology:cs_reference_frame('480c00b2-3e83-4a09-a0f2-a829167addd1', carbon_intensity_neutrality_framework).
narrative_ontology:cs_drift_state('480c00b2-3e83-4a09-a0f2-a829167addd1', post_renewable_cost_revolution, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('480c00b2-3e83-4a09-a0f2-a829167addd1', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives direct subsidies, loan guarantees, capacity payments, and streamlined permitting justified by baseload reliability and zero-carbon generation. Its revenue model and long-term capital planning depend on policy frameworks that classify nuclear as indispensable to portfolio optimization.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, national).

% Benefits from the all-low-carbon-sources framing that expands total climate spending, grid interconnection priority, and policy legitimacy, though it competes with nuclear for subsidy allocation and baseload-oriented grid planning attention.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_sector, beneficiary,
    organized, generational, mobile, national).

% Bear carbon pricing, phase-out mandates, exclusion from green financing frameworks, and stranded asset risk under the portfolio optimization regime. They are the primary economic losers in the shift to a technology-neutral carbon-intensity standard that structurally excludes them.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_incumbents, payer,
    powerful, biographical, constrained, global).

% Fund nuclear subsidies, capacity markets, and reliability premiums through regulated electricity rates and publicly backed debt. They receive low-carbon electricity but exercise limited voice over technology selection or cost allocation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, electricity_ratepayers, payer,
    organized, biographical, constrained, national).

% Design formally technology-neutral carbon-intensity rules, portfolio standards, and reliability criteria that embed baseload requirements structurally favoring nuclear inclusion while presenting the framework as an undifferentiated low-carbon mandate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_policy_architects, agenda_setter,
    institutional, generational, analytical, national).

% Oppose nuclear expansion due to waste-stream burdens, siting inequity, and legacy harm, but are sidelined by the technology-neutral carbon framing that treats such objections as secondary to emissions intensity metrics.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, environmental_justice_groups, excluded,
    organized, generational, constrained, national).

% Bear the physical impacts of climate change and are the stated beneficiaries of mitigation policy, yet possess negligible institutional influence over whether decarbonization follows a centralized nuclear pathway or an alternative systems-transition trajectory.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_vulnerable_communities, observer,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective climate mitigation by aligning energy investment toward decarbonization while promising to solve the grid-reliability problem through diversified low-carbon portfolio inclusion of nuclear baseload.
% TRANSFER_FUNCTION: Moves public subsidy, regulatory forbearance, guaranteed market share, and ratepayer capital from fossil fuel incumbents to nuclear operators (and secondarily to other low-carbon sources), structured through a technology-neutral carbon-intensity rule.
% ABSENT_VOICES: Anti-nuclear environmental and environmental-justice advocates are structurally excluded by the technology-neutral framing; fossil-fuel-dependent workers and regions are present in discourse but overruled on technology choice by the carbon-intensity metric.
% DISAPPEARANCE_RATIONALE: If the portfolio-optimization imperative vanished overnight, nuclear projects would lose their primary policy justification and financing, fossil fuel phase-outs might accelerate or stall depending on replacement economics, and grid planners would face a reliability-planning crisis without the baseload anchorârearranging capital flows, employment, and infrastructure lock-in across the energy sector.
% FOUNDING_PROBLEM: Anthropogenic climate change driven by greenhouse gas emissions, requiring rapid decarbonization of electricity generation without compromising grid reliability or economic stability.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC and national academies of science attest to the emissions problem from outside the nuclear beneficiary set. The specific claim that nuclear baseload is necessary is corroborated by centralized energy-systems modelers and engineering institutions, while independent renewable-grid analysts and energy economists contest the necessity claim from outside the benefiting parties.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the constraint genuinely coordinates decarbonization, yet a significant share of transferred value flows to nuclear operators through mechanisms decoupled from marginal carbon abatement cost. Suppression (0.58) reflects active regulatory exclusion of fossil pathways and the suppression of alternative readings in technology-neutral discourse. Theater ratio (0.42) captures the growing performative dimension of baseload-necessity arguments as empirical grid data evolves. Accessibility collapse (0.48) is incomplete because sibling readings remain structurally viable. Resistance (0.60) is substantial from fossil incumbents and anti-nuclear advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is coordination: solving the collective-action problem of decarbonization without blackouts. From the fossil fuel and ratepayer seats, the same structure reads as extraction: their capital and income are mobilized to underwrite nuclear projects they did not choose. The renewable seat experiences partial subsidy benefit but also competitive pressure from nuclear's guaranteed market share. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry and renewable sector are structural beneficiaries (low directionality, subsidized by the constraint). Fossil fuel incumbents and electricity ratepayers are targets (high directionality, paying through exclusion and surcharge). Policy architects sit near symmetric: they administer the constraint and are not personally extracted from, but their institutional legitimacy depends on its persistence. Excluded environmental justice groups would experience high directionality if incorporated, but their current structural exclusion places them outside the active derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâanthropogenic climate changeâis genuinely live, preventing classification as a snare or piton. However, the specific nuclear-baseload necessity claim may be a mandate layer added to the founding problem. If empirical analysis shows that renewables-plus-storage can deliver equivalent reliability at lower cost, the nuclear-baseload mandate becomes a tangled rope in which the coordination function (climate mitigation) is leveraged to justify asymmetric extraction (nuclear subsidies). The framework detects this by separating the live founding problem from the contested technology-specific mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity_empirical_status,
    'Is nuclear baseload genuinely necessary for grid reliability in high-renewable systems, or has this claim become a post-hoc justification for industrial policy?',
    'Comparative grid modeling and empirical operating data from jurisdictions with high renewable penetration, testing whether reliability metrics hold without nuclear contribution.',
    'If falsified, the nuclear beneficiary structure loses its coordination rationale and the constraint shifts toward snare classification; if verified, the extraction level is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_status, empirical, 'Empirical status of nuclear baseload necessity claim').

omega_variable(
    technology_neutrality_sincerity,
    'Does the carbon-intensity framework structurally privilege nuclear despite its technology-neutral veneer?',
    'Audit of subsidy flows, capacity market design, siting and permitting rules, and loan guarantee allocation relative to marginal abatement cost curves.',
    'Demonstrated privilege would confirm asymmetric extraction within the tangled rope; sincere neutrality would lower extractiveness metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_sincerity, empirical, 'Whether technology-neutral framing masks nuclear privilege').

omega_variable(
    kernel_reading_boundary,
    'This constraint instantiates the portfolio_optimization reading of the climate_mitigation_imperative kernel. Would adoption of the opportunity_cost reading (fastest deployment per dollar) or systems_transition reading (decentralization) eliminate the nuclear beneficiary set?',
    'Comparative policy adoption studies and energy-economics meta-analysis mapping reading adoption to technology-specific subsidy flows.',
    'Would reclassify the nuclear subsidy as contingent on reading selection rather than inherent to mitigation, confirming the kernel decomposition into distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural dependency of nuclear benefits on kernel reading selection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is the portfolio_optimization reading of the climate_mitigation_imperative kernel. The kernel decomposes into three structurally distinct claims because the same mitigation imperative yields different epsilon values and beneficiary/victim structures under different technology assumptions. This reading links to its siblings as coexisting alternative framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
