% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Carbon-per-Dollar-per-Year Mitigation Imperative (Opportunity-Cost Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the opportunity-cost reading of the
 *   climate_mitigation_imperative kernel: mitigation policy is understood as
 *   an allocation problem under a fixed, rapidly depleting carbon budget,
 *   where the correct criterion for capital allocation is gigatons of
 *   CO2-equivalent abated per dollar per year. Under this reading, nuclear
 *   power's decade-plus construction timelines and high capital intensity
 *   mean that dollars committed to nuclear deliver less cumulative abatement
 *   than the same dollars committed to utility-scale solar, wind, and
 *   storage, which can be deployed in a fraction of the time. The reading
 *   treats every year of nuclear construction before first power as a year of
 *   foregone faster abatement elsewhere, and treats capital diverted to
 *   nuclear as capital withheld from renewables. This is a genuinely
 *   different constraint from the portfolio_optimization_reading (which
 *   treats nuclear as necessary complementary firm capacity and would show
 *   negligible or negative extraction on nuclear) and from the
 *   systems_transition_reading (which frames nuclear as perpetuating
 *   centralized extractive energy governance, not primarily as a
 *   capital-diversion problem). All three share the label 'climate mitigation
 *   requires X' but instantiate structurally distinct constraints with
 *   different beneficiary/victim sets and different epsilon values — per the
 *   epsilon-invariance principle they are authored as three separate stories
 *   linked by network.affects_constraints, not as one story with a hidden
 *   observable parameter.
 *
 * KEY AGENTS:
 *   - climate_finance_institutions: agenda_setter (institutional/analytical) — administers the carbon-per-dollar-per-year eligibility criterion
 *   - utility_scale_solar_developers, wind_developers, battery_storage_manufacturers, grid_flexibility_service_providers: beneficiaries (organized/arbitrage-to-mobile) — capture capital reallocated under the fast-deployment metric
 *   - nuclear_developers, nuclear_supply_chain_workers, host_communities_of_planned_reactors, ratepayers_in_long_lead_time_jurisdictions: payers (powerful-to-powerless/trapped-to-constrained) — bear diverted capital, cancelled projects, and stranded costs
 *   - portfolio_diversification_advocates: excluded — argue firmness value is unpriced, but lack standing in climate-finance rulemaking
 *   - climate_policy_analysts: observer — model abatement-per-dollar-per-year but their findings are cited by all sides of the kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Carbon-per-Dollar-per-Year Mitigation Imperative (Opportunity-Cost Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5').
narrative_ontology:cs_kernel_codification('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', distributed).
narrative_ontology:cs_authority_grounding('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', distributed).
narrative_ontology:cs_reading_relation('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', foundational, deployment_speed_dominates_mitigation_value).
narrative_ontology:cs_axiom_status(deployment_speed_dominates_mitigation_value, holdable).
narrative_ontology:cs_axiom_grounding('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', deployment_speed_dominates_mitigation_value, empirically_contingent).
narrative_ontology:cs_axiom('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', secondary, capital_committed_to_slow_technology_is_capital_denied_to_fast_technology).
narrative_ontology:cs_axiom_status(capital_committed_to_slow_technology_is_capital_denied_to_fast_technology, holdable).
narrative_ontology:cs_axiom_grounding('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', capital_committed_to_slow_technology_is_capital_denied_to_fast_technology, empirically_contingent).
narrative_ontology:cs_reference_frame('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', carbon_budget_scarcity_allocation_framework).
narrative_ontology:cs_drift_state('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', post_2020_renewable_cost_collapse_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('248c0be9-c68b-4a0d-b1cd-d6d5e16f74f5', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, utility_scale_solar_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, wind_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, grid_flexibility_service_providers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_developers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_supply_chain_workers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, host_communities_of_planned_reactors).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, ratepayers_in_long_lead_time_jurisdictions).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, levelized_carbon_abatement_per_dollar_per_year_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, deployment_speed_dominance_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capture the bulk of climate-finance capital allocation once mitigation policy is framed around gigatons-per-dollar-per-year. Their short construction timelines and falling per-watt costs make them look categorically superior under this metric, and public and private capital follows the ranking almost automatically.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, utility_scale_solar_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Benefit from the same deployment-speed framing; project financing, subsidy eligibility, and grid interconnection queues increasingly prioritize technologies that show fast carbon-per-dollar returns, which favors wind's modular buildout over long-lead capital projects.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, wind_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Positioned as the necessary complement to intermittent renewables under this reading, they receive investment and policy support that would otherwise be split with baseload alternatives; the framing treats storage buildout as part of the same fast-deployment logic.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Sell demand-response and ancillary services that substitute for baseload firming; the opportunity-cost framing increases the perceived value of their services relative to building new firm generation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_flexibility_service_providers, beneficiary,
    moderate, biographical, mobile, national).

% Sunk into decade-long construction cycles and multi-billion-dollar capital commitments, they cannot pivot to faster deployment without abandoning sunk cost. Under carbon-per-dollar-per-year accounting, every year of construction before first power is treated as a foregone-mitigation cost, which is used to justify diverting subsidy, loan guarantees, and political support away from their projects mid-build.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_developers, payer,
    powerful, generational, trapped, national).

% Employed in forging, fuel fabrication, and specialized construction trades tied to reactor buildout schedules; when projects are cancelled or delayed because capital is redirected under the fast-deployment doctrine, they bear job losses with limited transferable pathways into renewables trades on short notice.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_supply_chain_workers, payer,
    moderate, biographical, constrained, national).

% Built local economic plans, tax base projections, and workforce pipelines around a reactor that may be cancelled, mothballed, or definancialized mid-construction once the opportunity-cost framing gains policy traction; they cannot relocate the sunk regional investment.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, host_communities_of_planned_reactors, payer,
    powerless, generational, trapped, local).

% Pay for cost overruns or stranded-asset write-downs on partially built reactors when the opportunity-cost doctrine triggers project cancellation after capital has already been spent, since regulated utility financing structures pass those costs through rates.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, ratepayers_in_long_lead_time_jurisdictions, payer,
    powerless, biographical, trapped, regional).

% Multilateral climate funds, green bond rating agencies, and philanthropic climate funders adopt carbon-per-dollar-per-year as the allocation criterion, setting eligibility rules and enforcing them through funding conditionality; they administer the metric and could revise it but currently treat it as settled methodology.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_finance_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Grid reliability engineers and systems planners who argue firm, dispatchable low-carbon generation has value the speed metric doesn't price — but their arguments are treated as special-pleading by nuclear incumbents rather than integrated into the allocation criterion, and they have limited standing in climate-finance rulemaking.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, portfolio_diversification_advocates, excluded,
    moderate, generational, constrained, national).

% Model abatement-per-dollar-per-year across technology portfolios and publish comparative analyses; their findings are cited by all sides of the kernel contest to support different readings of what mitigation 'requires.'
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__opportunity_cost_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs scarce mitigation capital toward whichever technologies deliver the largest verified reduction in cumulative emissions per dollar spent, given that atmospheric carbon budgets are being consumed continuously during any construction delay — a genuine allocation problem when capital and time are both binding constraints.
% TRANSFER_FUNCTION: Moves capital, subsidy eligibility, loan guarantees, and political attention away from long-lead nuclear projects and toward short-lead renewable and storage projects, on the basis of a carbon-per-dollar-per-year metric that structurally favors fast deployment over dispatchable firm capacity.
% ABSENT_VOICES: Grid reliability engineers and systems planners who would price firmness, dispatchability, and land-use footprint into the allocation criterion are largely absent from climate-finance rulemaking bodies, whose composition skews toward renewable-finance expertise; nuclear host communities have no seat in multilateral capital-allocation decisions made far from the reactor site.
% DISAPPEARANCE_RATIONALE: If the opportunity-cost framing disappeared overnight, renewable developers would lose a strong claim on marginal climate-finance dollars and nuclear projects would regain access to capital pools currently conditioned against them — beneficiaries argue mitigation speed would collapse and cumulative emissions would rise; nuclear advocates argue the sector would simply be evaluated on a fairer multi-attribute basis. The disagreement is exactly the kernel contest itself, which is why the verdict is contested rather than settled.
% FOUNDING_PROBLEM: Climate finance needed a defensible, quantifiable criterion for allocating limited mitigation capital across competing technologies under a hard, shrinking carbon budget, replacing ad hoc or politically driven allocation with something that could be defended as science-based.
% FOUNDING_PROBLEM_CORROBORATION: IPCC mitigation pathway modelers and independent energy-systems economists outside both the renewable-finance and nuclear-industry constituencies attest that near-term carbon budget constraints are real and that deployment speed has genuine climate value — this is corroborated by carbon budget accounting itself, not merely asserted by renewable beneficiaries. However, the same outside modelers are split on whether the opportunity-cost metric alone, versus a portfolio metric including reliability value, correctly captures total mitigation value, which is precisely the unresolved kernel dispute.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62 at t=20) reflects that capital and policy support genuinely move away from nuclear projects toward renewables under this metric, and that this transfer imposes real, sometimes irreversible costs (cancelled reactors, stranded regional investment) on identifiable parties — this is not merely a difference of technical opinion but an operative allocation mechanism with material winners and losers. Suppression is moderate (0.38) rather than high: nuclear proponents can and do contest the metric in courts, legislatures, and international climate negotiations, and the metric has not fully foreclosed alternative allocation criteria — hence resistance is authored high (0.72). Accessibility collapse is moderate-low (0.35): the portfolio and systems-transition readings remain live alternatives that nuclear advocates and others actively invoke, so the opportunity-cost framing has not achieved anything like mountain-level closure of alternatives. Theater ratio is modest but rising (0.12 to 0.28) as climate-finance institutions increasingly cite the metric performatively to justify decisions substantially driven by cost and political convenience rather than the modeling itself.
 *
 * PERSPECTIVAL GAP:
 *   From the climate_finance_institutions agenda-setter seat, this is a defensible science-based allocation rule solving a real scarcity problem under a real carbon budget — closer to a rope. From the nuclear_developers and host_communities payer seats, the same rule operates as an enforced, actively-defended reallocation mechanism that strands sunk capital and regional economic plans — closer to a tangled rope or snare, depending on how completely alternatives are foreclosed. The engine computes these divergent seat-level readings from the structural power/exit data; the claimed_type of tangled_rope reflects that BOTH a genuine coordination function (allocating scarce capital under real budget constraints) AND asymmetric extraction (nuclear-sector victims bearing losses that flow to renewable-sector beneficiaries through the same allocation mechanism) are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable and storage developers are declared beneficiaries because the metric's structure — favoring short construction timelines — mechanically routes capital and subsidy eligibility toward them; their exit options are arbitrage/mobile, reinforcing low derived d. Nuclear developers and their supply chain are declared victims because capital diversion mid-project is close to irreversible (sunk construction, specialized labor, long permitting) — their exit options are trapped, pushing derived d toward the full-target end. Host communities and ratepayers are especially powerless and trapped: they did not choose the capital-allocation criterion and cannot exit the regional or rate-base consequences of a project cancelled under it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating scarce mitigation capital under a real, depleting carbon budget — remains live (corroborated by IPCC-adjacent modeling outside either constituency), which is why this is authored as tangled_rope rather than snare: there IS a genuine coordination function being solved, not merely extraction dressed as coordination. What prevents mislabeling this as pure extraction is that the underlying scarcity (limited capital, finite remaining carbon budget) is real and independently attested. What prevents mislabeling it as pure coordination (rope) is that the specific metric chosen (deployment speed per dollar) is not neutral — it structurally advantages one technology class over another in a way that imposes concentrated, sometimes irreversible costs on a specific victim set, and climate-finance institutions administer this without input from the reliability-value constituency that would have argued for a different criterion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_completeness_of_carbon_per_dollar_per_year,
    'Does the carbon-per-dollar-per-year metric fully capture mitigation value, or does it systematically omit the value of dispatchable, weather-independent firm capacity that renewables-plus-storage may not fully substitute at high penetration?',
    'Long-horizon grid-decarbonization studies comparing realized system-level emissions outcomes (not just nameplate capacity additions) across jurisdictions that pursued renewables-dominant versus nuclear-inclusive portfolios, controlling for grid size and starting carbon intensity.',
    'If the metric substantially omits firmness value, the opportunity-cost reading is measuring a real but partial truth and the tangled_rope classification understates the coordination function being served (the true picture would look more like the portfolio_optimization_reading); if firmness value is negligible at relevant penetration levels, the current extraction reading is closer to complete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_completeness_of_carbon_per_dollar_per_year, empirical, 'Whether deployment-speed-per-dollar captures the full mitigation value function or omits reliability value.').

omega_variable(
    sunk_cost_causation_attribution,
    'When a nuclear project is cancelled or definancialized after the opportunity-cost framing gains policy traction, how much of the stranded cost is attributable to the framing''s capital diversion versus to nuclear''s own preexisting cost overruns and schedule slippage that would have caused cancellation regardless?',
    'Case-level forensic accounting of cancelled nuclear projects, separating capital withdrawal driven by opportunity-cost-metric-conditioned funding rules from capital withdrawal driven by project-specific overruns predating the metric''s adoption.',
    'If most cancellations were already overrun-driven, the opportunity-cost reading''s victim-set harm is smaller than authored and epsilon should be revised downward; if the metric materially accelerates cancellations that would otherwise have proceeded, the victim harm is as authored or greater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunk_cost_causation_attribution, empirical, 'Disentangling metric-caused harm to nuclear developers from nuclear''s own preexisting cost/schedule problems.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the opportunity-cost reading the most defensible single framing of ''what climate mitigation requires,'' or does the choice among opportunity-cost, portfolio-optimization, and systems-transition framings depend on prior political commitments (e.g., preference for centralized versus decentralized energy governance) that are doing more classificatory work than the carbon accounting itself?',
    'Trace which actors advocate which framing and whether framing choice correlates with prior institutional or ideological commitment to renewables-centric or nuclear-inclusive energy politics, independent of the carbon-accounting merits.',
    'If framing choice tracks prior political commitment rather than carbon-accounting merit, this reading and its siblings should be understood as competing normative claims dressed in technical language, which would strengthen the case for treating all three as coexisting readings rather than one being simply correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the choice of mitigation-requirement framing is carbon-accounting-driven or politically pre-determined.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement_basis(clim_tr_t4, observed).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(clim_tr_t16, projected).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(clim_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(clim_be_t4, observed).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(clim_be_t16, projected).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(clim_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement_basis(clim_su_t4, observed).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement_basis(clim_su_t16, projected).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(clim_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language label 'the climate mitigation imperative requires X regarding nuclear power.' The label conflates three structurally distinct claims with different epsilon values, different beneficiary/victim sets, and different failure modes: (1) opportunity_cost_reading (this story) — nuclear as capital/time diversion under a speed-per-dollar metric, tangled_rope, moderate-high epsilon; (2) portfolio_optimization_reading — nuclear as necessary firm-capacity complement, likely rope or mountain-adjacent with respect to reliability value, low epsilon on nuclear; (3) systems_transition_reading — nuclear as perpetuator of centralized extractive energy governance, likely snare or tangled_rope with a different victim set (decentralization advocates, energy-democracy movements) and a different mechanism entirely (political/structural centralization, not capital efficiency). Per the epsilon-invariance principle, these are authored as three separate constraint stories rather than one story with an observable-selection parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
