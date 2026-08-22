% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Portfolio Optimization Mitigation Imperative (Nuclear Baseload Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint captures one reading of the climate-mitigation kernel:
 *   that rapid decarbonization REQUIRES maximizing all available low-carbon
 *   energy sources, with nuclear as a necessary dispatchable baseload
 *   component. Under this reading, the constraint operates as a tangled rope:
 *   it solves a genuine coordination problem (how to meet carbon budgets
 *   under time pressure) but creates asymmetric extraction (fossil fuel
 *   workers and distributed-renewable advocates bear costs while nuclear
 *   vendors and centralized grid operators benefit). The
 *   portfolio-optimization framing is one of three live readings of the same
 *   kernel; the other readings (opportunity-cost, systems-transition)
 *   instantiate different constraints with different beneficiary/victim
 *   structures and different ε values. This JSON describes ONLY the
 *   portfolio-optimization reading, treated as a self-contained constraint
 *   per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - climate_policy_architects: agenda-setter; sets carbon targets and technology mandates
 *   - nuclear_technology_vendors: beneficiary; receive priority, subsidies, and long-term contracts under portfolio optimization
 *   - grid_operators_dependent_on_baseload: beneficiary; grid designs justified by nuclear's dispatchability narrative
 *   - fossil_fuel_producers: victim; stranded assets, policy exclusion, mandatory phase-out
 *   - distributed_renewable_advocates: victim; opportunity costs, subordination to baseload-centric planning
 *   - energy_transition_workers_in_fossil_fuels: payer/excluded; face job loss, limited retraining paths
 *   - climate_scientists: observer; model validation; critical to framing assumptions
 *   - alternative_reading_advocates: excluded; present opportunity-cost and systems-transition framings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Portfolio Optimization Mitigation Imperative (Nuclear Baseload Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '5859ecb8-4413-454d-994b-e944d7d16014').
narrative_ontology:cs_kernel_codification('5859ecb8-4413-454d-994b-e944d7d16014', distributed).
narrative_ontology:cs_authority_grounding('5859ecb8-4413-454d-994b-e944d7d16014', lineage).
narrative_ontology:cs_interpretation_layer_present('5859ecb8-4413-454d-994b-e944d7d16014').
narrative_ontology:cs_reading_relation('5859ecb8-4413-454d-994b-e944d7d16014', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('5859ecb8-4413-454d-994b-e944d7d16014', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('5859ecb8-4413-454d-994b-e944d7d16014', foundational, all_low_carbon_sources_necessary).
narrative_ontology:cs_axiom_status(all_low_carbon_sources_necessary, holdable).
narrative_ontology:cs_axiom_grounding('5859ecb8-4413-454d-994b-e944d7d16014', all_low_carbon_sources_necessary, empirically_contingent).
narrative_ontology:cs_axiom('5859ecb8-4413-454d-994b-e944d7d16014', foundational, baseload_dispatchability_required_for_grid_stability).
narrative_ontology:cs_axiom_status(baseload_dispatchability_required_for_grid_stability, holdable).
narrative_ontology:cs_axiom_grounding('5859ecb8-4413-454d-994b-e944d7d16014', baseload_dispatchability_required_for_grid_stability, empirically_contingent).
narrative_ontology:cs_axiom('5859ecb8-4413-454d-994b-e944d7d16014', secondary, technology_neutral_carbon_intensity_suffices_for_just_transition).
narrative_ontology:cs_axiom_status(technology_neutral_carbon_intensity_suffices_for_just_transition, holdable).
narrative_ontology:cs_axiom_grounding('5859ecb8-4413-454d-994b-e944d7d16014', technology_neutral_carbon_intensity_suffices_for_just_transition, instrumental).
narrative_ontology:cs_reference_frame('5859ecb8-4413-454d-994b-e944d7d16014', carbon_budget_imperative).
narrative_ontology:cs_drift_state('5859ecb8-4413-454d-994b-e944d7d16014', contemporary_alternative_pathway_emergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5859ecb8-4413-454d-994b-e944d7d16014', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_technology_vendors).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators_dependent_on_baseload).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, centralized_generation_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_producers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, distributed_renewable_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, energy_transition_workers_in_fossil_fuels).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce decarbonization targets under time pressure. Adopt the portfolio-optimization framing: every kilowatt of low-carbon generation, regardless of source, counts toward mitigation goals. Issue mandates to maximize carbon-free capacity, which under this reading elevates nuclear's role as the dispatchable low-carbon source. Set subsidy, regulatory priority, and target-achievement criteria accordingly.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_policy_architects, agenda_setter,
    institutional, generational, analytical, global).

% Design, manufacture, and operate nuclear reactors. Under portfolio optimization they are positioned as essential to meeting carbon targets; receive preferential regulatory treatment, subsidies, and long-term capacity contracts. Justify nuclear expansion by pointing to reliability requirements and carbon intensity, not by competing on capital cost or deployment speed.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_technology_vendors, beneficiary,
    powerful, generational, mobile, global).

% Operate electricity grids that have historically relied on continuous generation (coal, gas, nuclear). Under portfolio optimization, they benefit from nuclear's dispatchability and reliability narrative, which justifies grid designs that require large baseload plants. Their planning assumes nuclear will handle peak-hour guarantees, reducing the need for demand-response infrastructure or distributed storage.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators_dependent_on_baseload, beneficiary,
    institutional, biographical, constrained, regional).

% Extract and burn coal, gas, and oil. Portfolio optimization classifies them as victims by declaring carbon-intensive fuels incompatible with mitigation targets. Face stranded asset risk, policy exclusion, and mandatory phase-out timelines. Their cost-competitive advantages are nullified by carbon accounting; their labor, supply chains, and capital investments become policy targets.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_producers, payer,
    powerful, biographical, constrained, global).

% Promote and develop wind, solar, and small-scale hydroelectric systems. Under portfolio optimization their expansion is not *opposed* explicitly but is subordinated to reliability requirements that prioritize large baseload sources. They bear opportunity costs: capital that could fund solar/wind deployment is redirected to nuclear; grid upgrades for demand-response are deferred in favor of baseload-centric planning.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, distributed_renewable_advocates, payer,
    moderate, biographical, constrained, global).

% Mine coal, operate gas plants, staff oil refineries. Portfolio optimization declares fossil fuels stranded; they face job loss and devalued regional economies. Nuclear offers some skilled transition roles (maintenance technicians, security), but far fewer jobs per unit energy than renewables or the original fossil sector. Are formally excluded from mitigation target discussions; their retraining and community resilience are not resources constrained by the carbon-neutral portfolio choice.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, energy_transition_workers_in_fossil_fuels, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, energy_transition_workers_in_fossil_fuels, excluded).

% Assess mitigation pathways and carbon budgets. Under portfolio optimization framing they are positioned to validate that all low-carbon sources are necessary; nuclear is a necessary part of any plausible 1.5°C pathway. Their modeling choices (assumptions about demand growth, storage costs, grid flexibility) shape whether nuclear appears essential or optional.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_scientists_and_modelers, observer,
    institutional, generational, analytical, global).

% Argue for opportunity-cost or systems-transition readings. Contend that nuclear's capital intensity and timeline make it net-harmful for meeting carbon targets fastest; or that centralized nuclear perpetuates extractive energy systems. Are excluded from portfolio-optimization target-setting but their published research and policy testimony constitute potential resistance.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, alternative_reading_advocates, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_technology_vendors).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregate all available low-carbon generation sources to meet carbon-intensity targets and decarbonization timelines. Solves a collective-action problem: individual technology choices create path dependencies that may lock in carbon; portfolio optimization coordinates technology selection toward a shared carbon-budget outcome.
% TRANSFER_FUNCTION: Redirects capital investment, regulatory priority, subsidy, and skilled labor from fossil fuels toward nuclear energy. Moves decision-making authority from competitive technology selection toward centralized target-setting that mandates nuclear as a solution category. Transfers decommissioning and waste costs to future generations and taxpayers.
% ABSENT_VOICES: Fossil fuel workers' communities are structurally excluded from the framing that declares their livelihoods stranded; alternative-reading advocates (opportunity-cost, systems-transition) are excluded from target-setting and portfolio-composition decisions. They would testify that portfolio optimization under-counts capital costs, overestimates nuclear reliability, and embeds path dependencies that foreclose distributed alternatives.
% DISAPPEARANCE_RATIONALE: If portfolio-optimization framing and its mandates vanished, capital would redistribute: nuclear projects would stall unless financed by risk-accepting actors; renewables would accelerate (lower capex, faster deployment); grid architecture would shift toward distributed storage and demand response rather than baseload-dependent designs. Energy markets would reorganize around least-cost decarbonization rather than technology-neutral carbon intensity.
% FOUNDING_PROBLEM: Global carbon emissions must reach net-zero by mid-century to limit climate warming. Early climate models showed all decarbonization pathways included nuclear energy, suggesting nuclear is necessary for mitigation.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists attests the founding problem is live: warming is accelerating and low-carbon capacity must expand. The contention is not whether carbon must be reduced, but WHETHER portfolio optimization (all low-carbon sources, nuclear mandatory) is the correct framing for how. Opportunity-cost and systems-transition advocates corroborate that the founding problem exists but dispute the portfolio reading: they argue fastest emissions reduction per dollar or systems democratization are the correct objectives, not technology-agnostic carbon maximization. IPCC modeling shows multiple pathways, some with less nuclear than the portfolio reading assumes.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) reflects the constraint's asymmetric burden: policy mandates nuclear participation without competitive cost comparison, and redirects capital from alternatives. The extraction rises from 0.52 to 0.68 over the first 20 time periods (observed) as policy codification strengthens and nuclear subsidy regimes expand, then plateaus (projected 0.68 at t=35) as the policy regime stabilizes. Suppression (0.62) is substantial but not dominant: fossil fuel phase-out requires active enforcement (regulatory exclusion, subsidy withdrawal, asset devaluation), but distributed renewables are not suppressed—they are subordinated through prioritization and infrastructure design rather than coercion. Theater (0.41) is moderate: the real coordination function (meeting carbon targets) is genuine, but a growing proportion of policy activity defends nuclear's privileged position (modeling assumptions, reliability narratives, waste-disposal framing) rather than optimizing carbon per dollar. Accessibility collapse (0.72) is moderately high: once the portfolio-optimization framing is adopted, alternatives collapse—the mandate is technology-neutral in language but functionally nuclear-mandatory. Resistance (0.58) is substantial: alternative-reading advocates, distributed-renewable developers, and fossil-fuel interests all mount real resistance through research, litigation, and advocacy.
 *
 * PERSPECTIVAL GAP:
 *   From policy architects' and climate scientists' seats, the constraint is pure coordination: every low-carbon megawatt matters for carbon budgets, and nuclear's reliability is essential. From fossil fuel and distributed-renewable seats, the same constraint operates as policy-driven extraction: the mandate to include nuclear is justified by reliability claims, not by cost-competitiveness or speed. The divergence in classification is not noise—it is the constraint working: beneficiary seats compute tangled_rope's coordination function; target seats compute its extraction machinery. A genuine disagreement on whether nuclear is necessary sits at the root: opportunity-cost and systems-transition readings would show ε values 0.15–0.35 lower because they compute that mandatory nuclear is NOT necessary and therefore the extraction (cost redirection, opportunity cost) is not justified by the coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy architects sit at analytical power—they set the constraint but are not subject to it (d near 0.5, symmetric to the problem they solve). Nuclear vendors are full beneficiaries (d near 0.0): they collect subsidies and contract guarantees without bearing transition costs. Grid operators are partial beneficiaries (d near 0.2): they benefit from predictable baseload supply but also bear regulatory and financial risk. Fossil fuel producers are full targets (d near 1.0): they are mandated to exit without compensation beyond subsidy-adjusted transition programs. Distributed-renewable advocates are partial targets (d near 0.7): they are not coerced, but opportunity costs and capital prioritization extract value. Energy transition workers are full targets (d near 1.0): trapped in regions and skills that become stranded. The engine derives d from beneficiary/victim declarations and exit options; this commentary names why the structural asymmetry produces divergent seat classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (carbon reduction under time pressure) is live and urgent. The policy question is whether portfolio optimization (all low-carbon, nuclear included) is the correct framing. Mandatrophy emerges if the constraint persists after the founding problem is solved OR if evidence shows portfolio optimization is NOT the optimal framing for solving it. In this case: if emissions targets are met faster by opportunity-cost (fastest deployment per dollar, renewables + storage) or by systems-transition (decentralized grids reduce demand), then the portfolio-optimization constraint's mandate becomes obsolete cover for nuclear subsidy. The measurement series shows extractiveness plateaus at t=20 (policy regime solidifies) even as modeling shows distributed alternatives becoming cost-competitive. This plateau—when extraction stops increasing but the founding problem's urgency remains high—is the mandatrophy signal: the constraint may persist as policy inertia and vendor capture, not because the founding problem requires it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_portfolio_vs_opportunity_cost,
    'Is portfolio optimization (maximize all low-carbon, nuclear necessary) or opportunity-cost optimization (fastest emissions reduction per dollar, nuclear optional) the correct framing for mitigation?',
    'Empirical comparison of modeled decarbonization outcomes under both framings: (a) capital required to meet 1.5°C targets, (b) time to emissions neutrality, (c) stranded asset risk in each scenario, (d) cost per ton CO2 avoided. Compare to observed deployment data from regions adopting each framing.',
    'If opportunity-cost framing achieves targets faster and cheaper, the portfolio-optimization reading''s mandate becomes unjustified extraction. If portfolio optimization achieves comparable targets with lower risk, it validates nuclear as necessary. The reading contest itself is the irreducible uncertainty—no single model or metric adjudicates which framing is ''correct'' because they rest on different value premises (maximize carbon-free capacity vs. maximize cost-efficiency).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_portfolio_vs_opportunity_cost, conceptual, 'Whether the portfolio or opportunity-cost reading correctly identifies the founding problem''s solution.').

omega_variable(
    reading_contest_portfolio_vs_systems_transition,
    'Is the mitigation objective carbon-neutral energy (technology-agnostic, portfolio-optimization framing) or democratized energy systems (systems-transition framing)?',
    'Post-decarbonization analysis: in regions that achieve carbon neutrality under each framing, compare energy-system concentration (centralization vs. distribution), labor distribution (jobs per unit energy), and vulnerability to failure cascades. Assess whether centralized nuclear systems are more or less resilient to climate-driven disruption than distributed alternatives.',
    'If distributed, democratized systems achieve carbon neutrality with greater resilience and labor absorption, the systems-transition reading captures the fuller problem. If centralized systems are more cost-effective or reliable, portfolio optimization is vindicated. The framing contest—carbon neutrality as the sufficient objective vs. decarbonization as a lever for energy-system transformation—is irreducible because it depends on what ''solving the problem'' means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_portfolio_vs_systems_transition, preference, 'Whether mitigation is a carbon-intensity objective or a systems-transformation objective.').

omega_variable(
    nuclear_reliability_assumption,
    'Is nuclear''s role as reliable baseload generation necessary for grid stability, or can distributed renewables + storage + demand-response provide comparable reliability at lower cost?',
    'Grid simulation and real-world data from grids achieving high renewable penetration (Denmark, Uruguay, Costa Rica): do they require baseload plants to maintain stability? Are reliability and cost metrics better or worse than nuclear-heavy grids?',
    'If reliability can be maintained without nuclear, the baseload-necessity narrative is theater; the constraint''s mandatrophy threshold is crossed and extraction is undefended. If nuclear provides reliability advantages, portfolio optimization is structurally justified. The modeling assumptions (demand growth, storage-cost curves, grid-flexibility mechanisms) embedded in each reading determine whether nuclear appears necessary or optional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_reliability_assumption, empirical, 'Whether nuclear baseload is necessary for reliable decarbonization or substitutable by renewables + storage.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.62) structural (regulatory barriers, subsidy withdrawal, asset devaluation) or internalized (fossil fuel actors believe nuclear is necessary and accept phase-out)?',
    'Post-phase-out behavior: if fossil fuel workers and communities organize distributed-renewable deployment in their regions after assets are stranded, suppression was structural. If they remain economically and politically immobilized, suppression is internalized—identity and community attachment to fossil-fuel sectors persists even after the structural mechanism is removed.',
    'Structural suppression is externally imposed and reversible; internalized suppression is durable and requires different remedial approaches. If internalized, the constraint''s effective suppression is higher than the 0.62 metric suggests, and mandatrophy resolution requires addressing the internalized beliefs, not just the policy mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of fossil fuels is structural (policy-imposed) or internalized (believed).').

omega_variable(
    capital_reallocation_path_dependency,
    'Are the capital investments and grid infrastructure designs mandated by portfolio optimization reversible if the reading is displaced by opportunity-cost or systems-transition framings?',
    'Analysis of sunk costs in nuclear construction, grid-design lock-in, and regulatory barriers to retrofitting distributed alternatives. If nuclear plants are built, stranded-cost rules prevent their decommissioning; if grids are designed for baseload, demand-response infrastructure is deferred indefinitely.',
    'If portfolio optimization''s mandates are reversible, the reading contest remains live—alternative framings can be adopted with reasonable transition cost. If path-dependent, early adoption of portfolio optimization forecloses the opportunity-cost and systems-transition alternatives economically (not logically, but practically). This is a key asymmetry: the first reading adopted accrues lock-in advantage regardless of its theoretical merits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_reallocation_path_dependency, empirical, 'Whether portfolio-optimization mandates create irreversible path dependence that favors nuclear.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t35, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(clim_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t35, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(clim_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t35, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(clim_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories compose the climate_mitigation_imperative kernel family. This story (portfolio_optimization_reading) treats the kernel as a mandate to maximize all low-carbon sources with nuclear as necessary baseload. The sibling readings decompose the same kernel differently: opportunity_cost_reading treats it as a mandate to minimize cost per ton CO2 avoided (nuclear optional), and systems_transition_reading treats it as a mandate to transform energy systems toward decentralization and democratic control. Each reading has a distinct ε value (portfolio_optimization: 0.68; opportunity_cost estimated at 0.35–0.45; systems_transition estimated at 0.60–0.70 but different extraction targets). They share a common referent (the standing climate-policy commitment to decarbonization) but instantiate different structural constraints because the readings diverge on what 'mitigation' means and who bears its costs. The network links enable contamination propagation: if portfolio optimization is challenged by empirical evidence that renewables achieve targets faster, the opportunity-cost reading gains epistemic authority and constrains nuclear beneficiaries' policy capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, powerless, 1.0).
constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
