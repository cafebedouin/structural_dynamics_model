% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Climate Mitigation Legitimacy — Portfolio Pragmatism Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the portfolio-pragmatism reading of the
 *   climate-mitigation-legitimacy kernel — the contestable,
 *   committer-dependent framing that optimal decarbonization requires
 *   technology-neutral evaluation of nuclear and renewables under
 *   cost-minimization, with regional variation in optimal mix permitted. The
 *   reading is NOT a claim about physical reality (whether decarbonization is
 *   physically possible with renewables alone) but about legitimacy frames:
 *   what counts as a valid reason for technology selection in policy. Under
 *   this reading, 'this technology is cheaper and faster in this region' is a
 *   sufficient reason; 'renewables are inherently superior' or 'nuclear is
 *   necessary for baseload' are not. The constraint persists because
 *   engineering pragmatists and diversified investors benefit from the
 *   decision-mechanism it establishes, and because integrated assessment
 *   models (the epistemic authority) produce outputs that align with it.
 *   Extraction is moderate (0.38) because the reading does impose genuine
 *   costs on advocates of excluded readings (renewable-primacy and
 *   baseload-necessity proponents must argue on merit rather than principle)
 *   while distributing benefits to investors and system operators broadly.
 *   The measurement series show modest growth in extractiveness as competing
 *   readings crystallize and the pressure to suppress them intensifies;
 *   theater remains low because the reading's core function (technology
 *   selection via cost analysis) remains genuine even as political
 *   contestation rises.
 *
 * KEY AGENTS:
 *   - diversified_energy_investors: institutional beneficiary with arbitrage exit — collects revenue across multiple technology classes, permits capital reallocation if any single class becomes unviable
 *   - engineering_pragmatists: organized beneficiary with mobile exit — their professional judgment is legitimated by the reading; can move to other optimization frameworks if this one is displaced
 *   - grid_operators: organized beneficiary with constrained exit — require operational flexibility that portfolio diversity provides; exit would mean serving customers under unstable technical constraints
 *   - climate_scientists/IPCC: institutional agenda-setter with analytical exit — produce the IAMs that ground the reading's legitimacy; their outputs set the epistemic frame for 'technology-neutral' as a meaningful category
 *   - jurisdictional_policymakers: powerful agenda-setter with mobile exit — choose regional technology mix under the reading; can adopt competing readings but face international/investor pressure to remain aligned with IPCC/IEA consensus
 *   - renewable_energy_advocates: organized payer-beneficiary hybrid with constrained exit — frustrated by refusal to mandate renewables, but benefit from competitive legitimacy if their technology wins cost tenders
 *   - nuclear_power_operators: institutional payer-beneficiary hybrid with constrained exit — benefit from equal footing with renewables, but must compete directly on cost and cannot rely on baseload mandate
 *   - degrowth_advocates: organized excluded with constrained exit — their reading (degrowth_sufficiency) forecloses the portfolio frame by changing the baseline demand assumption; they remain outside this reading's scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.38).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.41).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Climate Mitigation Legitimacy — Portfolio Pragmatism Reading").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '23d8a2ff-d06c-4870-ab4b-bca47b898c1a').
narrative_ontology:cs_kernel_codification('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', distributed).
narrative_ontology:cs_authority_grounding('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', expertise).
narrative_ontology:cs_interpretation_layer_present('23d8a2ff-d06c-4870-ab4b-bca47b898c1a').
narrative_ontology:cs_reading_relation('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', climate_mitigation_legitimacy__baseload_necessity_reading, influences).
narrative_ontology:cs_reading_relation('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', climate_mitigation_legitimacy__renewable_primacy_reading, influences).
narrative_ontology:cs_reading_relation('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', foundational, technology_neutrality_in_optimization).
narrative_ontology:cs_axiom_status(technology_neutrality_in_optimization, holdable).
narrative_ontology:cs_axiom_grounding('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', technology_neutrality_in_optimization, instrumental).
narrative_ontology:cs_axiom('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', foundational, regional_variation_is_legitimate_output).
narrative_ontology:cs_axiom_status(regional_variation_is_legitimate_output, holdable).
narrative_ontology:cs_axiom_grounding('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', regional_variation_is_legitimate_output, instrumental).
narrative_ontology:cs_reference_frame('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', cost_minimization_under_carbon_budget).
narrative_ontology:cs_drift_state('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', contemporary_climate_policy_2020_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('23d8a2ff-d06c-4870-ab4b-bca47b898c1a', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_investors).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, engineering_pragmatists).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_power_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, low_carbon_electricity_consumers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_power_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large energy investment vehicles and pension funds deploy capital across nuclear, renewables, storage, and grid infrastructure. The portfolio-pragmatism reading permits balanced portfolio construction across all technology classes without regulatory bias. They benefit from regulatory frameworks that permit any low-carbon technology to compete for deployment capital on cost-merits; this maximizes their investment optionality and reduces technology-specific risk concentration.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Energy systems engineers, grid planners, and operations researchers whose professional judgment is grounded in optimization theory (cost-minimization, reliability constraints, resource availability). The portfolio-pragmatism reading legitimates their analytical approach: technology selection is derived from mathematical optimization of the electricity system under constraints, not from ideology. They benefit from policies that implement their analytical recommendations directly.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, engineering_pragmatists, beneficiary,
    organized, biographical, mobile, global).

% Electrical grid operators face hard constraints: reliability (generation must match load + reserves at all times), frequency stability, transmission congestion, and now decarbonization. The portfolio-pragmatism reading permits them to specify technical requirements (dispatchability, ramp rates, energy storage capacity) without being overridden by technology ideology. Nuclear provides baseload and frequency stability; renewables provide variable generation that drives storage deployment; grid infrastructure evolves to match the mix. They benefit from legitimacy for operating whatever technology portfolio physics and economics require.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_operators, beneficiary,
    organized, biographical, constrained, national).

% Environmental organizations, renewable energy manufacturers, and renewable-first advocates who argue renewables should be the primary decarbonization strategy. The portfolio-pragmatism reading frustrates them by refusing to mandate renewable primacy: they must prove cost-effectiveness and deployment speed on the merits rather than resting on principle ('renewables are inherently superior, so they should be mandated'). However, they also benefit from technology-neutrality because renewables routinely win competitive tenders in regions with good wind/solar resources, and the reading legitimates large-scale renewable deployment as a valid optimization outcome.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_advocates, beneficiary).

% Nuclear utilities and reactor manufacturers who operate and deploy nuclear capacity. The portfolio-pragmatism reading permits nuclear licensing and capital deployment under the same cost-neutral evaluation as renewables, which they benefit from (equal standing with competitors). But they also bear high costs: nuclear cannot claim a privileged 'baseload is necessary' role and must compete directly on overnight capital costs, which are currently high; on construction speed, where renewables are faster; and on capacity-factor potential, where renewables are increasingly competitive. The reading eliminates the possibility of baseload-guarantee policies that would guarantee them deployment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_power_operators, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_power_operators, beneficiary).

% The IPCC and integrated assessment modeling community produce the IAMs that generate technology-neutral optimization outputs. The portfolio-pragmatism reading permits their IAM outputs to set policy boundaries directly: least-cost portfolios under carbon budgets become policy targets. They gain epistemic authority and legitimacy for their analytical framework. They set the rules of the game (cost-minimization is the metric) and technology selection is determined by whose model runs fastest/cheapest.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists_and_ipcc, agenda_setter,
    institutional, civilizational, analytical, global).

% National and subnational governments that must decarbonize under binding carbon constraints (Paris Agreement, net-zero pledges, carbon budgets). The portfolio-pragmatism reading permits them to evaluate technology options regionally and choose economically optimal mixes. They can point to IPCC/IEA consensus (expertise-grounded authority) for their technology choices, which insulates them from pure-ideology challenges. However, they also must be transparent about trade-offs and accept that their choices can be evaluated against cost-efficiency benchmarks.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, jurisdictional_policymakers, agenda_setter,
    powerful, biographical, mobile, national).

% Solar panel and wind turbine manufacturers who could benefit from policy mandates privileging renewables. The portfolio-pragmatism reading excludes that possibility: they compete on cost and performance, not on policy preference. They could advocate for renewable-primacy reading (which would privilege renewables through mandates) but that reading is not this one. They have alternative exit: simply continue to improve cost and performance and win competitive tenders.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_manufacturing_incumbents, excluded,
    powerful, biographical, arbitrage, global).

% Uranium mining, fuel processing, and reactor component manufacturers. They could benefit from baseload-necessity reading (which would guarantee nuclear deployment) but that reading is not this one. They are excluded from technology-selection authority by the portfolio-pragmatism reading's cost-neutrality: their success depends on nuclear operators winning cost tenders, not on policy mandates.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_fuel_cycle_operators, excluded,
    institutional, generational, constrained, global).

% A non-agent entity representing the technical and political problem of long-term radioactive waste governance and disposal. The portfolio-pragmatism reading does NOT resolve waste governance — it permits nuclear deployment but defers waste-handling to a separate policy domain. Waste governance becomes a side constraint ('nuclear is cost-optimal IF a waste solution exists') rather than a veto, and the reading excludes waste-management voices from technology-selection discussions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_waste_management_systems, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_waste_management_systems).

% End consumers of electricity who benefit from rapid, cost-effective decarbonization regardless of which technologies achieve it. The portfolio-pragmatism reading legitimates whatever technology mix minimizes total system cost, which tends to lower electricity prices and emissions. However, they are identity-locked into consuming whatever electricity the regional grid provides and cannot exit if their preferred technology mix differs from the optimized regional portfolio.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, low_carbon_electricity_consumers, beneficiary,
    powerless, biographical, identity_locked, global).

% Advocates of demand reduction and economic degrowth as the primary decarbonization strategy argue that the portfolio-pragmatism reading's baseline (electricity demand as exogenously given) is itself the problem. They would argue for degrowth-sufficiency reading (which forecloses the portfolio frame by questioning the demand baseline) but that reading is not this one. They are excluded from the reading's scope by its foundational assumption that generation expansion, not demand reduction, is the decarbonization mechanism.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_advocates, excluded,
    organized, civilizational, constrained, global).

% Engineers, nuclear operators, and analysts who argue that reliable decarbonization requires dispatchable baseload power that renewables cannot provide at scale. The portfolio-pragmatism reading excludes them from mandating nuclear by forcing them to defend baseload necessity on grid-physics grounds rather than principle. They are seated in the broader policy ecosystem but structurally frustrated by the reading's refusal to privilege nuclear as 'necessary.'
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, baseload_necessity_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, engineering_pragmatists).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates electricity decarbonization by specifying cost-minimization under carbon constraint as the legitimate decision-criterion, permitting each jurisdiction to evaluate which technology portfolio (nuclear, renewables, storage, grid infrastructure) achieves least-cost decarbonization in its regional context. Solves the problem of technology selection without imposing ideological preference.
% TRANSFER_FUNCTION: Moves decision-making authority from technology ideology (principle-based mandates) to empirical optimization (cost-based selection). Winners receive deployment capital and regulatory support based on performance; losers do not. This transfers power from political-ideological factions to engineering and investment communities.
% ABSENT_VOICES: Degrowth advocates (who would argue the baseline electricity demand should be questioned rather than optimized for); advocates of renewable-only systems (who argue speed/cost justifies renewable primacy as policy mandate, not just economic outcome); advocates of nuclear-mandatory systems (who argue baseload necessity justifies nuclear as non-negotiable infrastructure). These advocates are seated but structurally frustrated by the reading's refusal to privilege their position. Waste-management systems have no voice in technology selection — the reading excludes them. Long-term climate justice voices and frontline communities often excluded from technocratic IAM processes that ground the reading's authority.
% DISAPPEARANCE_RATIONALE: If the portfolio-pragmatism reading vanished, jurisdictions would revert to technology-ideological selection: some would mandate renewable-only systems (losing dispatchability and storage efficiency), others would mandate nuclear (losing deployment speed), still others would fragment into competing mandates and delay decisions. Global decarbonization costs would increase substantially; capital would reallocate to regions with clearer technology policy; competing readings would claim exclusive validity, preventing coordinated long-term planning.
% FOUNDING_PROBLEM: Early climate policy and energy planning conflated two problems: (1) what technologies can reduce carbon, and (2) which technologies are culturally/politically acceptable. Technology mandates (coal bans, nuclear shutdowns, renewable percentage targets) typically mixed these two problems together, with political preference masquerading as technical necessity. The portfolio-pragmatism reading was constructed to separate them: permit technical optimization (cost-minimization) while leaving cultural/political questions to democratic process, and permit regional variation where resources and preferences differ.
% FOUNDING_PROBLEM_CORROBORATION: IPCC integrated assessment models (Rogelj et al. 2018, Luderer et al. 2021) and IEA Net Zero roadmaps consistently show mixed-technology portfolios as cost-optimal under virtually all carbon budgets and regional scenarios. Independent corroboration from NREL ReEDS modeling (Cole et al. 2016), UK Climate Change Committee (2020), and EU policy analysis (Kanellopoulos et al. 2021). These sources all adopt portfolio-pragmatism framing explicitly — no single technology dominates all optimal pathways. However, competing readings contest whether these models are truly neutral: renewable-primacy advocates argue the models underestimate renewable learning rates and storage potential; baseload-necessity advocates argue the models underestimate grid integration costs for variable renewables. The fact that the founding problem remains contested even among climate scientists and policy analysts indicates the reading's authority is epistemic (grounded in IAM methodology) rather than settled fact.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness remains moderate (0.38, plateauing by t=25) because the portfolio-pragmatism reading produces genuine coordination benefit (cost-minimization under carbon constraint) while simultaneously imposing asymmetric costs on competing readings. Advocates of renewable-primacy must argue speed/cost on the merits; advocates of baseload-necessity must argue grid physics on the merits; neither can rest on principle. This is extraction: the reading transfers power from ideology to empirics. However, extraction is not high (0.52 at upper bound) because the beneficiaries include diverse, competing interests (renewable manufacturers and nuclear operators both gain access to deployment capital; investors benefit from portfolio insurance). Suppression (0.41) is moderate and stable because the competing readings remain live — IPCC scenarios explicitly show baseload-only and renewable-only pathways as higher-cost alternatives, not impossibilities, so advocates can continue to argue their positions. They are suppressed only in the narrow sense that their preferred positions cannot be mandated by principle; they retain full ability to argue cost and performance. Theater (0.22) remains low because the core function (technology selection via cost analysis) is genuine: IAMs are real analytical tools, not theatrical justifications, even though they carry contested assumptions. The measurement series show extractiveness growing early (t=0 to t=15) as the reading's legitimacy crystallizes and competing framings are pushed to defend their positions empirically. Plateau at t=15 onward reflects stabilization: once the portfolio-pragmatism reading is established in IPCC reports and major energy policy frameworks, further increases in extractiveness face diminishing returns because all major actors have already adapted their positioning.
 *
 * PERSPECTIVAL GAP:
 *   From the portfolio-pragmatism seat (the IPCC analyst, the diversified investor), the arrangement is pure coordination: a decision-mechanism that permits cost-minimization and regional variation, producing lower total cost and faster deployment. From the renewable-primacy seat (environmental NGO, renewable manufacturer), the same arrangement is partial extraction: their preferred technology is forced to compete on cost when they would prefer mandate-based deployment. From the baseload-necessity seat (nuclear operator seeking long-term investment stability), it is also extraction: their technology loses guaranteed-role certainty and must compete directly. The engine computes these different classifications from directionality + power + exit. The reading does NOT adjudicate which perspective is correct — it is the reading that makes the portfolio-pragmatism frame the legitimate one, which is why it is contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations plus exit options. Diversified investors (arbitrage exit) sit at d~0.05 (strong beneficiary); engineering pragmatists (mobile exit) sit at d~0.1; grid operators (constrained exit) sit at d~0.2. Renewable and nuclear operators both sit near d~0.4 (moderate payer): they lose ideological privilege (cost) but gain competitive access to capital (benefit). No overrides are needed because the structural derivation correctly captures the asymmetry: beneficiaries have arbitrage/mobile exits, so they can easily reallocate if the reading changes; payers have constrained exits, so they must accept the new competitive frame. Degrowth advocates are excluded rather than payed, so they do not feed the directionality computation directly — their excluded status indicates they would have something to say but are not in the conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status (live) combined with disappearance verdict (world_rearranges) and the moderate extractiveness (0.38) create a classic rope-or-tangled-rope discrimination. The reading has a genuine coordination function (cost-minimization under carbon constraint is a real problem), so pure-extraction (snare) is ruled out. However, the extraction component (forcing competing readings to defend on merit) is substantial enough to elevate the constraint above pure rope. The fact that the reading's persistence depends on continued epistemic authority of the IPCC (requires active enforcement in the form of maintaining the authority of IAM outputs) and that this enforcement suppresses alternative frameworks (renewable-primacy, baseload-necessity) without eliminating them suggests tangled-rope structure: genuine coordination (technology-neutral cost optimization) married to asymmetric enforcement (competing readings must argue on cost, not on principle). However, the authored claimed_type is rope because the constraint is read by its beneficiaries (investors, pragmatists) as pure coordination, and that reading is not false. The tangled-rope risk emerges only when we account for the costs borne by advocates of suppressed readings. The IPCC acts as agenda-setter, enforcing the reading via scientific authority, which generates the tangled-rope signature. If the IPCC stopped producing technology-neutral IAMs (enforcement collapsed), the reading would degrade to a piton: technologically neutral in form but maintained only by bureaucratic inertia and academic convention, no longer serving a real coordination function for policymakers facing actual cost-benefit decisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_boundary,
    'Does the portfolio-pragmatism reading represent a genuinely neutral technology-selection mechanism, or does it embed hidden assumptions that favor particular technologies (e.g., favoring centralized dispatchable power over distributed variable generation)?',
    'Meta-analysis of integrated assessment model assumptions: comparing three IAM ensembles (IPCC AR5, AR6, CMIP6+) for implicit cost parameters, discount rates applied to nuclear vs. renewable learning curves, and treatment of grid integration costs for renewables vs. baseload flexibility premiums for nuclear. Sensitivity analysis on key assumptions should show whether portfolio composition is robust to plausible alternative assumptions.',
    'If hidden assumptions are detected and the portfolio composition changes substantially under alternative assumptions, the reading collapses from ''technology-neutral'' to ''embedded-preference-under-analytical-cover,'' which would shift the constraint from rope toward tangled-rope (active suppression of competing framings masked as neutrality). This would elevate the claim from ''coordination mechanism'' to ''false neutrality''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_boundary, conceptual, 'Whether the portfolio-pragmatism reading''s technology-neutrality is structurally sound or artifacts of model assumptions.').

omega_variable(
    reading_committer_indexing,
    'This constraint is authored as ONE reading of the climate-mitigation-legitimacy kernel, meaning its ε, beneficiaries, and classification are relative to the reading''s own epistemic commitments. Is the reading''s framing (cost-minimization under carbon constraint as the sole decision criterion) a committer-choice or a discovered fact about optimal decarbonization?',
    'Analysis of which actors authored the portfolio-pragmatism reading and when. The reading emerged from IPCC synthesis of IAM outputs (Rogelj et al., Luderer et al.) in the 2010s–2020s. If the reading was authored by consensus of all climate-policy factions (renewable advocates, nuclear proponents, degrowth critics), it would be a discovered fact; if it was authored by a subset and imposed on the others, it is a committer-choice. Witness: the intense contestation of IPCC AR6 (2021) by renewable-primacy and degrowth advocates indicates the reading is not consensus.',
    'If the reading is a committer-choice, the constraint''s classification becomes reading-indexed: for the reading''s beneficiaries, it is rope (pure coordination); for its suppressed rivals, it is tangled-rope (enforced extraction). This does NOT change the JSON classification (which is authored for the portfolio-pragmatism reading alone), but it resolves the ambiguity about whether the constraint''s legitimacy is universal or sitting-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_indexing, conceptual, 'Whether portfolio-pragmatism is a committer-choice framing or a universally endorsed principle.').

omega_variable(
    regional_optimization_vs_global_mandate,
    'The reading permits regional variation in optimal technology mix (nuclear in France, renewables in Denmark, hydro in Brazil). Does this regional flexibility constitute genuine coordination, or does it mask the absence of global decarbonization governance?',
    'Comparative analysis of three scenarios: (1) decarbonization under unified global carbon price (~$100/tonne CO2) implemented uniformly; (2) decarbonization under national jurisdiction with varied carbon policies; (3) decarbonization under portfolio-pragmatism framing with regional IAM optimization. Measure total cost, peak generation capacity required, deployment speed, and stranded assets. If scenario 1 outperforms scenario 3, the reading''s ''flexibility'' is actually suboptimal fragmentation.',
    'If regional variation produces higher total cost or slower deployment than unified governance would, the reading is extraction: jurisdictions adopt portfolio-pragmatism framing as a way to avoid the distributional costs of unified carbon pricing, and the constraint becomes a mechanism for shifting climate costs to lower-income regions unable to deploy all technology classes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_optimization_vs_global_mandate, empirical, 'Whether regional optimization under portfolio-pragmatism is globally efficient or locally cost-shifting.').

omega_variable(
    suppression_internalization_dynamic,
    'The reading suppresses renewable-primacy and baseload-necessity advocates by forcing them to argue on cost-merit rather than principle. Is this suppression structural (external barriers to principle-based argument) or internalized (advocates have come to accept the portfolio frame as legitimate)?',
    'Rhetorical analysis of major advocates'' public statements (2010–2026): Are renewable-primacy advocates still arguing ''renewables are inherently superior'' or have they shifted to ''renewables are faster/cheaper''? Are baseload-necessity advocates still arguing ''nuclear is necessary for civilization'' or ''nuclear is cost-competitive for reliable generation''? If the rhetoric has shifted toward cost-based argument, the suppression is internalized. If advocates still use principle-based claims while being excluded from policy, suppression is structural.',
    'If suppression is internalized, the constraint has successfully colonized the reasoning of its targets, and extraction is higher than the authored 0.38 because targets now argue within the reading''s frame without perceiving the frame as imposed. If suppression is purely structural (advocates argue principles but are ignored in policy), extraction is lower because the suppression relies on active exclusion rather than internalized acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_dynamic, empirical, 'Mechanism of suppression: structural external barriers vs. internalized adoption of the reading''s frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(clim_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(clim_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(clim_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the climate-mitigation-legitimacy kernel. The kernel is the contested decision-mechanism for technology selection in decarbonization policy; readings differ on which technologies are privileged a priori (none, nuclear baseload, renewables, or demand reduction). The portfolio-pragmatism reading permits cost-neutral evaluation and regional variation. The baseload_necessity and renewable_primacy readings constrain the optimization problem by imposing technology mandates. The degrowth_sufficiency reading changes the baseline (demand reduction instead of generation expansion). All four share a common referent (the decision-mechanism for technology selection under carbon constraint) but author different ε values relative to their own reading's commitments. The four stories form a constraint family linked by network.affects_constraints and are jointly sufficient to map the full kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
